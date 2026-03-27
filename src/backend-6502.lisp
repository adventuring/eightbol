;; src/backend-6502.lisp -- 6502 / 64tass code generation backend
;;
;; Implements COMPILE-TO-ASSEMBLY for the :6502 CPU keyword. Targets the
;; MOS  6502   instruction  set,   emitting  64tass   assembler  syntax.
;; Output conforms  to OOPS (Object-Oriented Programming  System) naming
;; conventions defined in Phantasia/SkylineTool.
;;
;; CPU-specific  instruction   sets  (64tass  format):  6502:   JMP  for
;; branches;  SED/CLD for  BCD;  undocumented opcodes  (lax, dcp)  where
;; helpful 65c02:  BRA, STZ, TRB,  TSB; no undocumented  opcodes 65c816:
;; Like 65c02 (emulation mode); 64tass  syntax HuC6280: Like 65c02; bank
;; switching via macros  RP2A03: 6502 subset; no SED  (no decimal mode);
;; software BCD
;;
;; Key  conventions:  Method  labels: MethodCharacterThink:  .block  ...
;; .bend GOBACK/EXIT: rts INVOKE Self "M": .CallMethod Call{Class}M, {Class}Class
;; INVOKE Var "M": .CallMethod Call{Type}M, {Type}Class, Var  Slot  access:  ldy
;; #{OriginClass}{Slot} /  lda (Self), y  Array access:  ldx index  / lda
;; base, x  (X  for  subscript;  Y   for  slot)  Constants  (78/77):  lda
;; #ConstName    (immediate   addressing)    Variables:   lda    VarName
;; (direct/absolute addressing) Fault:  .LogFault "code" (4-char string;
;; assembler does minifont)
;;
;; Register  usage: A=accumulator,  X=subscript  index  or temp,  Y=slot
;; offset.   Array   fetches   use    X   (or   Y   for   slot-of-self).
;; Complex expressions may have multiple subscripts at different stages.
;; When using X  for temp (e.g. 16-bit add low-byte),  prefer PHA/PLA if
;; operands contain subscripts to avoid conflict.

(in-package :eightbol)

;;; CPU variant selection (bound during compilation)

(defvar *6502-family-cpu* :6502
  "CPU keyword for 6502-family backends. One of :6502 :65c02 :65c816 :huc6280 :rp2a03.
Controls opcode choice: bra vs jmp, stz vs lda/sta, undocumented opcodes.")

(defun 6502-branch-always-mnemonic ()
  "Return \"jmp\" for 6502/RP2A03 (no BRA), \"bra\" for 65c02/65c816/HuC6280."
  (if (member *6502-family-cpu* '(:65c02 :65c816 :huc6280))
      "bra"
      "jmp"))

(defun 6502-has-stz-p ()
  "True if CPU has STZ (65c02 and derivatives)."
  (member *6502-family-cpu* '(:65c02 :65c816 :huc6280)))

(defun 6502-use-undocumented-p ()
  "True if CPU allows undocumented 6502 opcodes (lax, dcp, etc.). Only :6502."
  (eq *6502-family-cpu* :6502))

(defvar *6502-accumulator-expr* nil
  "AST for the value currently in A during 6502-family emission, or NIL if unknown.

Rebound per method. @code{emit-6502-load-expr} skips @code{lda} when @code{equal}
to the requested expression; @code{%invalidate-6502-accumulator-a} clears the
cache after subroutine calls or A-clobbering paths. After @code{sta} to a
single-byte lvalue, @code{%6502-note-accumulator-holds-value-of} records that A
still holds that value (avoids reloading after @code{adc}/@code{sta} to the same slot).")

(defun %invalidate-6502-accumulator-a ()
  "Clear @code{*6502-accumulator-expr*} so A is treated as unknown."
  (setf *6502-accumulator-expr* nil))

(defun %6502-note-accumulator-holds-value-of (expr)
  "Record that A holds the value of EXPR after a store or op that leaves it there.

EXPR must match what a later @code{emit-6502-load-expr} will receive (@code{equal}).
Used to omit redundant @code{lda} after @code{sta} to the same location."
  (setf *6502-accumulator-expr* expr))

(defun %6502-load-byte-n-sets-y-for-slot-store (expr class-id)
  "True if @code{emit-6502-load-byte-n} on EXPR emits @code{ldy} then @code{lda (Self),y} or @code{lda (Ptr),y}.

When the next op is @code{sta} to the same slot byte, @code{ldy} may be omitted — Y still holds the offset.
Not true for @code{:subscript} loads (uses X) or absolute globals (no Y)."
  (or (slot-of-self-p expr)
      (and (slot-of-expr expr) (not (slot-of-self-p expr)))
      (and (stringp expr) (implicit-instance-slot-p expr class-id))))

(defun %6502-subtract-2byte-inplace-eligible-p (from from-target result giving class-id w bcd-p)
  "True when SUBTRACT can use one @code{ldy} to the low slot byte, @code{sta}/@code{iny}/@code{lda}/@code{sbc}/@code{sta} for high byte (no @code{tax}/@code{dey} dance)."
  (and (= w 2)
       (not bcd-p)
       (null giving)
       (equal from-target result)
       (or (slot-of-self-p result)
           (and (stringp result) (implicit-instance-slot-p result class-id)))
       (not (expr-contains-subscript-p from-target))
       (not (expr-contains-subscript-p from))
       (not (expr-contains-subscript-p result))))

(defun emit-6502-subtract-2byte-self-inplace (out from result class-id bcd-p)
  "Emit 16-bit SUBTRACT @code{FROM} from @code{RESULT} (same Self slot), little-endian, in place.

Sequence: @code{ldy} low offset, @code{sec}, @code{lda (Self),y}, @code{sbc} low, @code{sta (Self),y},
@code{iny}, @code{lda (Self),y}, @code{sbc} high, @code{sta (Self),y}."
  (when bcd-p
    (error 'backend-error
           :message "EIGHTBOL/6502: BCD 2-byte in-place SUBTRACT not implemented"
           :cpu :6502
           :detail (list :subtract-inplace from result)))
  (let ((off (if (slot-of-self-p result)
                 (slot-symbol (second (slot-of-expr result)) class-id)
                 (slot-symbol result class-id))))
    (format out "~&~10Tldy ~a" (emit-6502-immediate off))
    (format out "~&~10Tsec")
    (format out "~&~10Tlda (Self), y")
    (if (expr-is-constant-p from)
        (let ((v (expr-constant-value from)))
          (format out "~&~10Tsbc #<~d" v))
        (format out "~&~10Tsbc ~a" (emit-6502-value from)))
    (format out "~&~10Tsta (Self), y")
    (format out "~&~10Tiny")
    (format out "~&~10Tlda (Self), y")
    (if (expr-is-constant-p from)
        (let ((v (expr-constant-value from)))
          (format out "~&~10Tsbc #>~d" v))
        (format out "~&~10Tsbc ~a + 1" (emit-6502-value from)))
    (format out "~&~10Tsta (Self), y")
    (%invalidate-6502-accumulator-a)))

(defun emit-6502-store-zero (out addr)
  "Emit code to store zero to memory at ADDR. Uses stz for 65c02+, lda # 0 + sta for 6502/RP2A03."
  (if (6502-has-stz-p)
      (progn
        (format out "~&~10Tstz ~a" addr)
        (%invalidate-6502-accumulator-a))
      (progn
        (format out "~&~10Tlda # 0")
        (format out "~&~10Tsta ~a" addr)
        (%invalidate-6502-accumulator-a))))

;;; Top-level entry point

(defun method-statements-list (method)
  "Return METHOD's @code{:statements} as a list with @code{NIL} placeholders removed.
Parser or optimization must not leave junk entries, but filtering keeps shape predicates robust."
  (remove nil (ensure-list (safe-getf (rest method) :statements))))

(defun method-blank-p (method)
  "True if METHOD has no statements (blank procedure division).

@itemize @bullet
@item
@code{:statements} is absent or @code{nil}.
@end itemize"
  (null (method-statements-list method)))

(defun method-trivial-return-only-p (method)
  "True if METHOD has exactly one statement that is only a return (@code{:goback}, @code{:exit-method}, @code{:exit-program}, @code{:exit}, @code{:stop-run}).

@itemize @bullet
@item
METHOD — a @code{:method} AST node.
@end itemize"
  (let ((stmts (method-statements-list method)))
    (and (= (length stmts) 1)
         (let ((s (first stmts)))
           (and (listp s)
                (not (null (member (first s)
                                   '(:goback :exit-method :exit-program :exit :stop-run)))))))))

(defun method-true-method-alias-p (method)
  "True if METHOD should be emitted as @code{MethodClassM = TrueMethod} (no @code{.block}).

Blank methods and single-statement GOBACK/EXIT*/STOP RUN match Phantasia's hand-written stubs.

@itemize @bullet
@item
METHOD — a @code{:method} AST node.
@end itemize"
  (or (method-blank-p method)
      (method-trivial-return-only-p method)))

(defun method-last-stmt-6502-no-trailing-rts-p (last-stmt)
  "True if LAST-STMT already ends control flow so @code{compile-6502-method} must not emit a trailing @code{rts}.

Covers returns and tail @code{CALL} that emits @code{jmp} when @code{:tail-call-p} is set
(e.g. @code{CALL … GOBACK}).  @code{INVOKE} uses @code{.CallMethod} (@code{jsr DoCallMethod}),
so the method always needs a trailing @code{rts}.  Far @code{CALL} never uses a bare tail @code{jmp}.

@itemize @bullet
@item
LAST-STMT — last statement in a method’s @code{:statements} list, or NIL.
@end itemize"
  (when (and last-stmt (listp last-stmt))
    (case (first last-stmt)
      ((:goback :exit-method :exit-program :exit :stop-run) t)
      (:call
       (let ((tail (safe-getf (rest last-stmt) :tail-call-p))
             (service (safe-getf (rest last-stmt) :service))
             (bank (safe-getf (rest last-stmt) :bank)))
         (and tail (not service) (not bank))))
      (t nil))))

(defun emit-one-6502-family-method (output-stream class-id method cpu compile-method-fn)
  "Emit one method: @code{MethodClassM = TrueMethod}, or a @code{.block} body from COMPILE-METHOD-FN.

COMPILE-METHOD-FN must accept @code{(METHOD CLASS-ID CPU)} and emit a @code{.block} body
(e.g. @code{compile-6502-method} or @code{compile-rp2a03-method}). CPU selects opcodes for
the shared 6502 statement helpers.

@table @asis
@item OUTPUT-STREAM
Assembly destination.
@item CLASS-ID
Compiling class id string (e.g. @code{\"Character\"}).
@item METHOD
@code{:method} AST plist.
@item CPU
Target keyword (@code{:6502}, @code{:rp2a03}, @dots{}).
@item COMPILE-METHOD-FN
Function of three arguments used when the method is not a @code{TrueMethod} alias.
@end table"
  (cond
    ((method-true-method-alias-p method)
     (format output-stream "~&Method~a~a = TrueMethod"
             class-id (cobol-id-to-assembly-symbol
                       (format nil "~a" (safe-getf (rest method) :method-id)))))
    (t (funcall compile-method-fn method class-id cpu))))

(defun compile-6502-family (ast output-stream cpu)
  "Shared code generation for 6502-family CPUs (6502, 65c02, 65c816, HuC6280).
CPU is the keyword (:6502 :65c02 :65c816 :huc6280). Binds *6502-family-cpu* for opcode selection.
Trivial methods emit @code{MethodClassM = TrueMethod}; @code{INVOKE Self} uses @code{.CallMethod} in the method body."
  (let ((*6502-family-cpu* cpu)
        (cpu-label (cpu-display-name cpu)))
    (unless (and (listp ast) (eq (first ast) :program))
      (error "EIGHTBOL/~a: expected :program AST node, got ~s" cpu-label (first ast)))
    (let* ((class-id (safe-getf (rest ast) :class-id))
           (methods (safe-getf (rest ast) :methods)))
      (multiple-value-bind (slot-table type-table const-table service-bank-table usage-table sign-table
                                        pic-size-table pic-width-table pic-frac-bits-table
                                        pic-nybble-semantics-table)
          (load-copybook-tables class-id)
        (let ((*output-stream* output-stream)
              (*class-id* class-id)
              (*slot-table* slot-table)
              (*type-table* type-table)
              (*const-table* const-table)
              (*service-bank-table* service-bank-table)
              (*usage-table* usage-table)
              (*sign-table* sign-table)
              (*pic-size-table* pic-size-table)
              (*pic-width-table* pic-width-table)
              (*pic-frac-bits-table* pic-frac-bits-table)
              (*pic-nybble-semantics-table* pic-nybble-semantics-table))
          (format output-stream
                  ";;; ~a -- generated by EIGHTBOL for ~a~%;;; DO NOT EDIT -- regenerate from ~a.cob~2%"
                  class-id cpu-label class-id)
          (dolist (method (ensure-list methods))
            (when (and (listp method) (eq (first method) :method))
              (emit-one-6502-family-method output-stream class-id method cpu #'compile-6502-method))))))))

(defmethod compile-to-assembly (ast (cpu (eql :6502)) output-stream)
  (compile-6502-family ast output-stream :6502))

;;; Method compilation

(defun compile-6502-method (method class-id cpu)
  "Emit one METHOD for CLASS-ID using CPU variant keyword.

Binds @code{*6502-family-cpu*} to CPU so opcode selection (e.g. @code{lax} vs
@code{lda}/@code{tax}) matches standalone compilation, not only @code{compile-6502-family}.

@table @asis
@item METHOD
@code{:method} AST node.
@item CLASS-ID
Compiling class string (e.g. @code{\"Character\"}).
@item CPU
@code{:6502}, @code{:65c02}, @code{:65c816}, or @code{:huc6280}.
@end table"
  (let* ((*6502-family-cpu* cpu)
         (*method-id* (safe-getf (rest method) :method-id))
         (method-dispatch-suffix (cobol-id-to-assembly-symbol
                                  (format nil "~a" *method-id*)))
         (stmts (method-statements-list method))
         (last-stmt (car (last stmts))))
    (format *output-stream* "~&Method~a~a: .block" class-id method-dispatch-suffix)
    (let ((*6502-accumulator-expr* nil))
      (dolist (stmt stmts)
        (cond
          ((null stmt))
          ((not (listp stmt))
           (error "EIGHTBOL: malformed procedure statement (expected list): ~s" stmt))
          ((first stmt)
           (compile-statement cpu (first stmt) (rest stmt))))))
    ;; Fall-through only: returns and tail jmp/jsr paths that never reach here.
    (unless (method-last-stmt-6502-no-trailing-rts-p last-stmt)
      (format *output-stream* "~&~10Trts"))
    (format *output-stream* "~&~10T.bend")))

;;; Statement dispatch via generic functions (compile-statement-* methods below)

;;; Parser format  normalization Parser  produces ("HP" "OF"  "Self") or
;;; ("HP" OF "Self"); backend expects (:of slot obj).

(defun normalize-slot-of (expr)
  "Convert parser format (slot \"OF\" obj) to (:of slot obj). Return NIL if not slot-of form."
  (when (and (listp expr) (= (length expr) 3))
    (let ((slot (first expr))
          (of (second expr))
          (obj (third expr)))
      (when (or (equal of "OF") (eq of 'eightbol::of))
        (list :of slot obj)))))

(defun slot-of-self-p (expr)
  "True if EXPR is slot OF Self in either (:of slot :self) or (slot OF Self) format."
  (let ((norm (or (and (listp expr) (eq (first expr) :of) expr)
                  (normalize-slot-of expr))))
    (when norm
      (member (third norm) '(:self "Self" self) :test #'equal))))

(defun slot-of-expr (expr)
  "Return (:of slot obj) form for EXPR, or NIL."
  (cond
    ((and (listp expr) (eq (first expr) :of)) expr)
    (t (normalize-slot-of expr))))

(defun %move-from-resolve-constant-of-self (from)
  "When FROM is (:of Name Self) but Name is a 77/78 in @code{*CONST-TABLE*}, return the canonical constant name.

Uses @code{constant-cobol-name-for-assembly} so class-qualified names (e.g. @code{Character-Song--Heal--ID}) become @code{Song--Heal--ID} for @code{cobol-constant-to-assembly-symbol} (@code{Song_Heal_ID}), not a mis-split @code{--} group.

MOVE, @code{emit-6502-load-expr}, compares, and ALU RHS paths must load immediates (@code{lda # Song_Heal_ID}), not @code{lda (Self),y} on a slot offset."
  (if (and (slot-of-self-p from) (slot-of-expr from))
      (let ((n (format nil "~a" (second (slot-of-expr from)))))
        (if (constant-p n)
            (constant-cobol-name-for-assembly n)
            from))
      from))

(defun %6502-move-source-byte-width (from)
  "Byte width of MOVE source: @code{max} of @code{expression-operand-width} and @code{operand-width}.

Ensures PIC 99 sources (e.g. @code{Character-Max-HP OF Self}) use width 2 when the copybook row is two bytes."
  (max (expression-operand-width from)
       (operand-width from)))

(defun 6502-object-pointer-label (obj-expr class-id)
  "Return 64tass label for object pointer OBJ-EXPR (ZP pair), e.g. Current-Actor → CurrentActor.

@table @asis
@item OBJ-EXPR
@code{:self} / @code{Self} / string data name.
@item CLASS-ID
Ignored (reserved for typed object refs).
@end table

@subsection Outputs
String usable as @code{lda (Label), y} base."
  (declare (ignore class-id))
  (cond
    ((member obj-expr '(:self "Self" self) :test #'equal)
     "Self")
    ((stringp obj-expr)
     (cobol-global-data-name-to-assembly-symbol obj-expr))
    (t
     (error 'backend-error
            :message "6502: unsupported object expression in slot OF (need Self or data name)"
            :cpu :6502
            :detail obj-expr))))

(defun emit-6502-alu-with-memory-rhs (out mnemonic expr class-id)
  "Emit MNEMONIC (@code{adc}, @code{sbc}, @code{and}, @code{ora}, @code{eor}) with RHS EXPR; A holds the other operand."
  (let ((expr (%move-from-resolve-constant-of-self expr)))
  (cond
    ((expr-is-constant-p expr)
     (format out "~&~10T~a ~a" mnemonic (emit-6502-immediate-operand expr)))
    ((and (stringp expr) (implicit-instance-slot-p expr class-id))
     (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol expr class-id)))
     (format out "~&~10T~a (Self), y" mnemonic))
    ((and (slot-of-expr expr) (not (slot-of-self-p expr)))
     (let* ((sof (slot-of-expr expr))
            (off (slot-symbol (second sof) class-id))
            (ptr (6502-object-pointer-label (third sof) class-id)))
       (format out "~&~10Tldy ~a" (emit-6502-immediate off))
       (format out "~&~10T~a (~a), y" mnemonic ptr)))
    (t (format out "~&~10T~a ~a" mnemonic (emit-6502-value expr)))))
  (%invalidate-6502-accumulator-a))

(defun emit-6502-cmp-memory-rhs (out expr class-id)
  "Emit @code{cmp} for single-byte RHS EXPR after A holds the left side.
Explicit @code{Max-HP OF Self} and bare instance slots use @code{ldy}/@code{cmp (Self),y}, not @code{cmp (Max-HP)}."
  (let ((expr (%move-from-resolve-constant-of-self expr)))
  (cond
    ((expr-is-constant-p expr)
     (format out "~&~10Tcmp ~a" (emit-6502-immediate-operand expr)))
    ((slot-of-self-p expr)
     (let ((offset (slot-symbol (second (slot-of-expr expr)) class-id)))
       (format out "~&~10Tldy ~a" (emit-6502-immediate offset))
       (format out "~&~10Tcmp (Self), y")))
    ((and (slot-of-expr expr) (not (slot-of-self-p expr)))
     (let* ((sof (slot-of-expr expr))
            (off (slot-symbol (second sof) class-id))
            (ptr (6502-object-pointer-label (third sof) class-id)))
       (format out "~&~10Tldy ~a" (emit-6502-immediate off))
       (format out "~&~10Tcmp (~a), y" ptr)))
    ((and (stringp expr) (implicit-instance-slot-p expr class-id))
     (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol expr class-id)))
     (format out "~&~10Tcmp (Self), y"))
    (t (format out "~&~10Tcmp ~a" (emit-6502-value expr))))))

(defun %emit-6502-alu-byte-n-of-expr (out mnemonic expr class-id n w)
  "Emit MNEMONIC (@code{cmp}, @code{adc}, or @code{sbc}) on byte N of W-byte EXPR.
A holds the accumulated other operand (same addressing as @code{emit-6502-load-byte-n} / @code{emit-6502-cmp-byte-n-of-expr})."
  (declare (type string mnemonic))
  (when (>= n w)
    (error "%emit-6502-alu-byte-n-of-expr: n ~d >= w ~d" n w))
  (let ((expr (%move-from-resolve-constant-of-self expr)))
    (cond
      ((and (stringp expr) (constant-p expr) (= w 1) (zerop n))
       (format out "~&~10T~a ~a" mnemonic (emit-6502-immediate-operand expr)))
      ((expr-is-constant-p expr)
       (format out "~&~10T~a ~a" mnemonic (emit-6502-immediate (or (constant-byte-value expr n) 0))))
      ((and (listp expr) (eq (first expr) :subscript))
       (format out "~&~10Tpha")
       (emit-6502-load-expr-into-x out (third expr) class-id)
       (format out "~&~10Tpla")
       (format out "~&~10T~a ~a + ~d, x" mnemonic (bare-data-assembly-symbol (second expr) class-id) n))
      ((slot-of-self-p expr)
       (let ((offset (slot-symbol (second (slot-of-expr expr)) class-id)))
         (format out "~&~10Tldy #~a" (if (zerop n) offset (format nil "(~a+~d)" offset n)))
         (format out "~&~10T~a (Self), y" mnemonic)))
      ((and (slot-of-expr expr) (not (slot-of-self-p expr)))
       (let* ((sof (slot-of-expr expr))
              (off (slot-symbol (second sof) class-id))
              (ptr (6502-object-pointer-label (third sof) class-id)))
         (format out "~&~10Tldy #~a" (if (zerop n) off (format nil "(~a+~d)" off n)))
         (format out "~&~10T~a (~a), y" mnemonic ptr)))
      ((and (stringp expr) (implicit-instance-slot-p expr class-id))
       (let ((off (slot-symbol expr class-id)))
         (format out "~&~10Tldy ~a" (emit-6502-immediate (if (zerop n) off (format nil "(~a+~d)" off n))))
         (format out "~&~10T~a (Self), y" mnemonic)))
      ((stringp expr)
       (format out "~&~10T~a ~a~a"
               mnemonic
               (bare-data-assembly-symbol expr class-id)
               (if (zerop n) "" (format nil " + ~d" n))))
      (t
       (format out "~&~10T~a ~a~a"
               mnemonic
               (emit-6502-value expr)
               (if (zerop n) "" (format nil " + ~d" n)))))))

(defun emit-6502-cmp-byte-n-of-expr (out expr class-id n w)
  "Compare A to byte N (0-based) of W-byte RHS EXPR (multi-byte @code{=} / @code{NOT =}).
Mirrors addressing in @code{emit-6502-load-byte-n}; @code{CMP} does not change A."
  (%emit-6502-alu-byte-n-of-expr out "cmp" expr class-id n w))

(defun emit-6502-adc-byte-n-of-expr (out expr class-id n w)
  "Add-with-carry byte N of W-byte RHS EXPR into A (same addressing as @code{emit-6502-cmp-byte-n-of-expr})."
  (%emit-6502-alu-byte-n-of-expr out "adc" expr class-id n w))

(defun emit-6502-sbc-byte-n-of-expr (out expr class-id n w)
  "Subtract-with-borrow byte N of W-byte RHS EXPR from A (same addressing as @code{emit-6502-cmp-byte-n-of-expr})."
  (%emit-6502-alu-byte-n-of-expr out "sbc" expr class-id n w))

(defun emit-6502-inc-dec-instance-or-bare (out opcode expr class-id)
  "Emit OPCODE (@code{inc} or @code{dec}) for 1-byte EXPR (implicit slot or absolute label)."
  (if (and (stringp expr) (implicit-instance-slot-p expr class-id))
      (progn
        (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol expr class-id)))
        (format out "~&~10T~a (Self), y" opcode))
      (format out "~&~10T~a ~a" opcode (bare-data-assembly-symbol expr class-id)))
  (%invalidate-6502-accumulator-a))

;;; Expression emission

(defun emit-6502-value (expr)
  "Return a 64tass expression string for EXPR.
Constants use @code{cobol-constant-to-assembly-symbol}. Non-constant bare identifiers use
@code{bare-data-assembly-symbol}. Indexed @code{:subscript} bases use @code{bare-data-assembly-symbol}
so global CartRAM labels match @code{emit-6502-load-byte-n}. @code{slot OF obj} in address context
emits @code{(slotname)} (see branch below), not the full @code{OriginClassSlot} label.

@table @asis
@item EXPR
Rvalue expression; @code{NIL} signals @code{backend-error} (avoids emitting a @code{NIL} label).
@end table"
  (when (and (not (numberp expr)) (null expr))
    (error 'backend-error
           :message "emit-6502-value: missing expression (NIL)"
           :cpu :6502
           :detail nil))
  (cond
    ((numberp expr) (format nil "~d" expr))
    ((and (listp expr) (eq (first expr) :literal))
     (format nil "~d" (second expr)))
    ((stringp expr)
     (if (constant-p expr)
         (cobol-constant-to-assembly-symbol (constant-cobol-name-for-assembly expr))
         (bare-data-assembly-symbol expr *class-id*)))
    ((eq expr :self) "Self")
    ((eq expr :null) "0")
    ((slot-of-expr expr)
     (error 'backend-error
            :message "emit-6502-value: slot OF … is not a plain address; use emit-6502-load-expr / load-byte-n"
            :cpu :6502
            :detail expr))
    ((and (listp expr) (eq (first expr) :subscript))
     ;; (:subscript base-name index) -- indexed table access, e.g. DecalYH, x
     ;; Must match emit-6502-load-byte-n / cmp: globals use CartRAM labels, not class-prefixed slot-symbol.
     (format nil "~a, x"
             (bare-data-assembly-symbol (second expr) *class-id*)))
    (t (format nil "~a" expr))))

(defun expr-is-literal-zero-p (expr)
  (or (eql expr 0)
      (equal expr "0")
      (and (listp expr) (eq (first expr) :literal) (eql (second expr) 0))))

(defun emit-6502-immediate (value)
  "Return assembly string for immediate operand: #value. Numbers use hex (#$xx)."
  (concatenate 'string (string #\#)
               (cond ((and (integerp value) (<= 0 value) (<= value 255))
                      (format nil "$~2,'0x" value))
                     ((integerp value)
                      (format nil "$~x" value))
                     (t
                      (princ-to-string value)))))

(defun expr-is-constant-p (expr)
  "True if EXPR is a numeric literal, named constant, or constant expression.
Constant expressions are arithmetic/bit ops whose operands are all constant."
  (cond
    ((numberp expr) t)
    ((and (listp expr) (eq (first expr) :literal)) t)
    ((and (stringp expr) (constant-p expr)) t)
    ((and (listp expr) (eq (first expr) :add-expr))
     (and (expr-is-constant-p (second expr)) (expr-is-constant-p (third expr))))
    ((and (listp expr) (eq (first expr) :subtract-expr))
     (and (expr-is-constant-p (second expr)) (expr-is-constant-p (third expr))))
    ((and (listp expr) (eq (first expr) :multiply-expr))
     (and (expr-is-constant-p (second expr)) (expr-is-constant-p (third expr))))
    ((and (listp expr) (eq (first expr) :divide-expr))
     (and (expr-is-constant-p (second expr)) (expr-is-constant-p (third expr))))
    ((and (listp expr) (eq (first expr) :bit-and))
     (and (expr-is-constant-p (second expr)) (expr-is-constant-p (third expr))))
    ((and (listp expr) (eq (first expr) :bit-or))
     (and (expr-is-constant-p (second expr)) (expr-is-constant-p (third expr))))
    ((and (listp expr) (eq (first expr) :bit-xor))
     (and (expr-is-constant-p (second expr)) (expr-is-constant-p (third expr))))
    ((and (listp expr) (eq (first expr) :bit-not))
     (expr-is-constant-p (second expr)))
    ((and (listp expr) (eq (first expr) :shift-left))
     (and (expr-is-constant-p (second expr))
          (or (null (third expr))
              (expr-is-constant-p (third expr)))))
    ((and (listp expr) (eq (first expr) :shift-right))
     (and (expr-is-constant-p (second expr))
          (or (null (third expr))
              (expr-is-constant-p (third expr)))))
    (t nil)))

(defun expr-constant-value (expr)
  "Return numeric value of constant EXPR. EXPR must satisfy expr-is-constant-p.
Folds constant expressions (add/subtract/multiply/divide/bit/shift) at compile time."
  (cond
    ((numberp expr) expr)
    ((and (listp expr) (eq (first expr) :literal)) (second expr))
    ((stringp expr) (constant-value expr))
    ((and (listp expr) (eq (first expr) :add-expr))
     (+ (expr-constant-value (second expr)) (expr-constant-value (third expr))))
    ((and (listp expr) (eq (first expr) :subtract-expr))
     (- (expr-constant-value (second expr)) (expr-constant-value (third expr))))
    ((and (listp expr) (eq (first expr) :multiply-expr))
     (* (expr-constant-value (second expr)) (expr-constant-value (third expr))))
    ((and (listp expr) (eq (first expr) :divide-expr))
     (truncate (expr-constant-value (second expr)) (expr-constant-value (third expr))))
    ((and (listp expr) (eq (first expr) :bit-and))
     (logand (expr-constant-value (second expr)) (expr-constant-value (third expr))))
    ((and (listp expr) (eq (first expr) :bit-or))
     (logior (expr-constant-value (second expr)) (expr-constant-value (third expr))))
    ((and (listp expr) (eq (first expr) :bit-xor))
     (logxor (expr-constant-value (second expr)) (expr-constant-value (third expr))))
    ((and (listp expr) (eq (first expr) :bit-not))
     (logand #xff (lognot (expr-constant-value (second expr)))))
    ((and (listp expr) (eq (first expr) :shift-left))
     (ash (expr-constant-value (second expr))
          (if (third expr) (expr-constant-value (third expr)) 1)))
    ((and (listp expr) (eq (first expr) :shift-right))
     (ash (expr-constant-value (second expr))
          (- (if (third expr) (expr-constant-value (third expr)) 1))))
    (t (error "Not a constant: ~s" expr))))

(defun emit-6502-immediate-operand (expr)
  "Full immediate operand for 64tass: folded numeric (@code{#$xx}) or named constant (@code{# Symbol}).

EXPR must satisfy @code{expr-is-constant-p}. Named 77/78 constants use @code{cobol-constant-to-assembly-symbol} on @code{constant-cobol-name-for-assembly} so class-qualified names map to @code{Song_Heal_ID}, not a bad @code{--} split (e.g. @code{Character-Song--Heal--ID})."
  (cond
    ((and (stringp expr) (constant-p expr))
     (format nil "# ~a" (cobol-constant-to-assembly-symbol
                           (constant-cobol-name-for-assembly expr))))
    ((expr-is-constant-p expr)
     (emit-6502-immediate (expr-constant-value expr)))
    (t (error "emit-6502-immediate-operand: not a constant ~s" expr))))

(defun power-of-two-p (n)
  "True if N is a positive power of two (1, 2, 4, 8, ...)."
  (and (integerp n) (> n 0) (zerop (logand n (1- n)))))

(defun log2 (n)
  "Return k such that 2^k = N, or NIL if N is not a power of two."
  (when (power-of-two-p n)
    (do ((k 0 (1+ k))
         (v 1 (* v 2)))
        ((>= v n) k))))

;;; Load into X (subscript index)

(defun emit-6502-load-expr-into-x (out expr class-id)
  "Load EXPR into X for subscript indexing.

When @code{*6502-family-cpu*} is :6502 (undocumented opcodes allowed), slot OF Self
and implicit instance slots use @code{lax (Self), y} instead of @code{lda (Self), y}
then @code{tax}. @var{OUT}, @var{CLASS-ID} as in @code{emit-6502-load-expr}."
  (let ((expr (%move-from-resolve-constant-of-self expr)))
  (cond
    ((and (slot-of-self-p expr) (6502-use-undocumented-p))
     (let ((so (slot-of-expr expr)))
       (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol (second so) class-id)))
       (format out "~&~10Tlax (Self), y"))
     (%invalidate-6502-accumulator-a))
    ((and (stringp expr) (implicit-instance-slot-p expr class-id) (6502-use-undocumented-p))
     (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol expr class-id)))
     (format out "~&~10Tlax (Self), y")
     (%invalidate-6502-accumulator-a))
    (t
     (emit-6502-load-expr out expr class-id)
     (format out "~&~10Ttax")
     (%invalidate-6502-accumulator-a)))))

;;; Load into A register

(defun emit-6502-load-expr (out expr class-id)
  "Emit 6502 code to load EXPR into the A register.
Constants (numeric literals and named 77/78 items) use immediate mode (#).
Constant expressions are folded at compile time and emitted as one immediate load.
Variables and slots use direct/indexed mode.
Compound bit/shift expressions are computed in-line.
Skips @code{lda} when @code{*6502-accumulator-expr*} is @code{equal} to EXPR.
When EXPR is @code{(:of Name Self)} and NAME is a 77/78 in @code{*const-table*}, treat as bare NAME (immediate @code{lda #}), not an instance slot."
  (let ((expr (%move-from-resolve-constant-of-self expr)))
  (when (and *6502-accumulator-expr*
             (equal expr *6502-accumulator-expr*))
    (return-from emit-6502-load-expr))
  (when (expr-is-constant-p expr)
    (if (and (stringp expr) (constant-p expr))
        (format out "~&~10Tlda # ~a"
                (cobol-constant-to-assembly-symbol (constant-cobol-name-for-assembly expr)))
        (format out "~&~10Tlda ~a" (emit-6502-immediate (expr-constant-value expr))))
    (setf *6502-accumulator-expr* expr)
    (return-from emit-6502-load-expr))
  (cond
    ;; Arithmetic: a + b
    ((and (listp expr) (eq (first expr) :add-expr))
     (emit-6502-load-expr out (second expr) class-id)
     (format out "~&~10Tclc")
     (emit-6502-alu-with-memory-rhs out "adc" (third expr) class-id)
     (setf *6502-accumulator-expr* expr))
    ;; Arithmetic: a - b
    ((and (listp expr) (eq (first expr) :subtract-expr))
     (emit-6502-load-expr out (second expr) class-id)
     (format out "~&~10Tsec")
     (emit-6502-alu-with-memory-rhs out "sbc" (third expr) class-id)
     (setf *6502-accumulator-expr* expr))
    ;; Arithmetic: a * k (k must be power of 2)
    ((and (listp expr) (eq (first expr) :multiply-expr))
     (let ((e1 (second expr)) (e2 (third expr)))
       (when (expr-is-constant-p e2)
         (let ((shift (log2 (expr-constant-value e2))))
           (when shift
             (emit-6502-load-expr out e1 class-id)
             (dotimes (_ shift) (format out "~&~10Tasl a"))
             (setf *6502-accumulator-expr* expr)
             (return-from emit-6502-load-expr))))
       (error 'backend-error
              :message "6502: multiply by non-power-of-2 requires software routine"
              :cpu :6502 :detail (list :multiply-expr e1 e2))))
    ;; Arithmetic: a / k (k must be power of 2)
    ((and (listp expr) (eq (first expr) :divide-expr))
     (let ((e1 (second expr)) (e2 (third expr)))
       (when (expr-is-constant-p e2)
         (let ((shift (log2 (expr-constant-value e2))))
           (when shift
             (emit-6502-load-expr out e1 class-id)
             (dotimes (_ shift) (format out "~&~10Tlsr a"))
             (setf *6502-accumulator-expr* expr)
             (return-from emit-6502-load-expr))))
       (error 'backend-error
              :message "6502: divide by non-power-of-2 requires software routine"
              :cpu :6502 :detail (list :divide-expr e1 e2))))
    ;; Subscripted array: base(index) — load index into X, then lda base, x
    ((and (listp expr) (eq (first expr) :subscript))
     (emit-6502-load-expr-into-x out (third expr) class-id)
     (format out "~&~10Tlda ~a" (emit-6502-value expr))
     (setf *6502-accumulator-expr* expr))
    ;; Slot OF Self -- indexed indirect via Self pointer
    ((slot-of-self-p expr)
     (let ((so (slot-of-expr expr)))
       (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol (second so) class-id)))
       (format out "~&~10Tlda (Self), y"))
     (setf *6502-accumulator-expr* expr))
    ;; Slot OF other object (e.g. Decal OF Current-Actor) — lda (Ptr),y
    ((and (slot-of-expr expr) (not (slot-of-self-p expr)))
     (let ((so (slot-of-expr expr)))
       (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol (second so) class-id)))
       (format out "~&~10Tlda (~a), y" (6502-object-pointer-label (third so) class-id)))
     (setf *6502-accumulator-expr* expr))
    ;; Shift left — asl A, n times
    ((and (listp expr) (eq (first expr) :shift-left))
     (let ((n (if (numberp (third expr)) (third expr) 1)))
       (emit-6502-load-expr out (second expr) class-id)
       (dotimes (_ n) (format out "~&~10Tasl a"))
       (setf *6502-accumulator-expr* expr)))
    ;; Shift right — lsr A, n times
    ((and (listp expr) (eq (first expr) :shift-right))
     (let ((n (if (numberp (third expr)) (third expr) 1)))
       (emit-6502-load-expr out (second expr) class-id)
       (dotimes (_ n) (format out "~&~10Tlsr a"))
       (setf *6502-accumulator-expr* expr)))
    ;; Bitwise AND (mask — used for bit testing too)
    ((and (listp expr) (eq (first expr) :bit-and))
     (emit-6502-load-expr out (second expr) class-id)
     (emit-6502-alu-with-memory-rhs out "and" (third expr) class-id)
     (setf *6502-accumulator-expr* expr))
    ;; Bitwise OR
    ((and (listp expr) (eq (first expr) :bit-or))
     (emit-6502-load-expr out (second expr) class-id)
     (emit-6502-alu-with-memory-rhs out "ora" (third expr) class-id)
     (setf *6502-accumulator-expr* expr))
    ;; Bitwise XOR
    ((and (listp expr) (eq (first expr) :bit-xor))
     (emit-6502-load-expr out (second expr) class-id)
     (emit-6502-alu-with-memory-rhs out "eor" (third expr) class-id)
     (setf *6502-accumulator-expr* expr))
    ;; Bitwise NOT (complement all bits)
    ((and (listp expr) (eq (first expr) :bit-not))
     (emit-6502-load-expr out (second expr) class-id)
     (format out "~&~10Teor #$ff")
     (setf *6502-accumulator-expr* expr))
    ;; Numeric literal (:literal n) from VALUE clause
    ((and (listp expr) (eq (first expr) :literal))
     (format out "~&~10Tlda ~a" (emit-6502-immediate (second expr)))
     (setf *6502-accumulator-expr* expr))
    ;; Numeric literal
    ((numberp expr)
     (format out "~&~10Tlda ~a" (emit-6502-immediate expr))
     (setf *6502-accumulator-expr* expr))
    ;; Bare data name: constants immediate; instance slots via (Self),y; else absolute (bare-data)
    ((stringp expr)
     (cond
       ((expr-is-constant-p expr)
        (format out "~&~10Tlda ~a" (emit-6502-immediate-operand expr)))
       ;; Grouped 77/78 names (Song--Hurt--ID): symbolic immediate even if *CONST-TABLE* missed merge.
       ((and (cobol-double-hyphen-grouped-name-p expr) (not (expr-is-constant-p expr)))
        (format out "~&~10Tlda # ~a" (cobol-constant-to-assembly-symbol expr)))
       ((implicit-instance-slot-p expr *class-id*)
        (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol expr *class-id*)))
        (format out "~&~10Tlda (Self), y"))
       (t (format out "~&~10Tlda ~a" (bare-data-assembly-symbol expr *class-id*))))
     (setf *6502-accumulator-expr* expr))
    (t
     (format out "~&~10Tlda ~a" (emit-6502-value expr))
     (setf *6502-accumulator-expr* expr)))))

(defun emit-6502-load-hi-byte (out ptr-expr class-id)
  "Load the high byte of a 2-byte value into A. Handles literals, constants, variables.
For :subscript, loads index into X then lda base + 1, x."
  (emit-6502-load-byte-n out ptr-expr class-id 1 2))

(defun constant-byte-value (expr n)
  "Extract byte N (0-based) from constant EXPR. Little-endian.
Uses expr-constant-value when EXPR is a constant expression."
  (when (expr-is-constant-p expr)
    (ldb (byte 8 (* n 8)) (expr-constant-value expr))))

(defun emit-6502-load-byte-n (out expr class-id n w)
  "Load byte N (0-based) of W-byte EXPR into A. N must be < W.
Handles literals, constants, variables, slot OF Self, subscript.
Named 77/78 constants with byte width 1 use @code{emit-6502-immediate-operand} (symbolic @code{# Name}), not @code{#$nn}, matching @code{emit-6502-load-expr}."
  (when (>= n w)
    (error "emit-6502-load-byte-n: n ~d >= w ~d" n w))
  (let ((expr (%move-from-resolve-constant-of-self expr)))
  (%invalidate-6502-accumulator-a)
  (cond
    ;; Single-byte copybook constant: MOVE and per-byte paths must use assembly equate name.
    ((and (stringp expr) (constant-p expr) (= w 1) (zerop n))
     (format out "~&~10Tlda ~a" (emit-6502-immediate-operand expr)))
    ((and (stringp expr) (cobol-double-hyphen-grouped-name-p expr)
          (not (expr-is-constant-p expr)) (= w 1) (zerop n))
     (format out "~&~10Tlda # ~a" (cobol-constant-to-assembly-symbol expr)))
    ((expr-is-constant-p expr)
     (let ((b (constant-byte-value expr n)))
       (format out "~&~10Tlda ~a" (emit-6502-immediate (or b 0)))))
    ((and (listp expr) (eq (first expr) :subscript))
     (emit-6502-load-expr-into-x out (third expr) class-id)
     (format out "~&~10Tlda ~a + ~d, x" (bare-data-assembly-symbol (second expr) class-id) n))
    ((slot-of-self-p expr)
     (let ((offset (slot-symbol (second (slot-of-expr expr)) class-id)))
       (format out "~&~10Tldy #~a" (if (zerop n) offset (format nil "(~a+~d)" offset n)))
       (format out "~&~10Tlda (Self), y")))
    ((and (slot-of-expr expr) (not (slot-of-self-p expr)))
     (let* ((sof (slot-of-expr expr))
            (off (slot-symbol (second sof) class-id))
            (ptr (6502-object-pointer-label (third sof) class-id)))
       (format out "~&~10Tldy #~a" (if (zerop n) off (format nil "(~a+~d)" off n)))
       (format out "~&~10Tlda (~a), y" ptr)))
    ((and (stringp expr) (string-equal expr "Self"))
     (format out "~&~10Tlda Self~a" (if (zerop n) "" (format nil " + ~d" n))))
    ((and (stringp expr) (implicit-instance-slot-p expr class-id))
     (let ((off (slot-symbol expr class-id)))
       (format out "~&~10Tldy ~a" (emit-6502-immediate (if (zerop n) off (format nil "(~a+~d)" off n))))
       (format out "~&~10Tlda (Self), y")))
    ((stringp expr)
     (format out "~&~10Tlda ~a~a"
             (bare-data-assembly-symbol expr class-id)
             (if (zerop n) "" (format nil " + ~d" n))))
    (t
     (format out "~&~10Tlda ~a~a"
             (emit-6502-value expr)
             (if (zerop n) "" (format nil " + ~d" n)))))))

(defun emit-6502-store-byte-n (out dest class-id n w &key skip-ldy)
  "Store A to byte N (0-based) of W-byte DEST. Handles slot OF Self, variable.

When SKIP-LDY is true, emit only @code{sta (Self), y} / @code{sta (Ptr), y} — Y must already hold
the slot offset from an immediately preceding @code{emit-6502-load-byte-n} to the same lvalue byte
(multi-byte ADD/SUBTRACT/COMPUTE loops)."
  (when (>= n w)
    (error "emit-6502-store-byte-n: n ~d >= w ~d" n w))
  (cond
    ((slot-of-self-p dest)
     (let ((offset (slot-symbol (second (slot-of-expr dest)) class-id)))
       (unless skip-ldy
         (format out "~&~10Tldy #~a" (if (zerop n) offset (format nil "(~a+~d)" offset n))))
       (format out "~&~10Tsta (Self), y")))
    ((and (slot-of-expr dest) (not (slot-of-self-p dest)))
     (let* ((sof (slot-of-expr dest))
            (off (slot-symbol (second sof) class-id))
            (ptr (6502-object-pointer-label (third sof) class-id)))
       (unless skip-ldy
         (format out "~&~10Tldy #~a" (if (zerop n) off (format nil "(~a+~d)" off n))))
       (format out "~&~10Tsta (~a), y" ptr)))
    ((and (stringp dest) (implicit-instance-slot-p dest class-id))
     (let ((off (slot-symbol dest class-id)))
       (unless skip-ldy
         (format out "~&~10Tldy ~a" (emit-6502-immediate (if (zerop n) off (format nil "(~a+~d)" off n)))))
       (format out "~&~10Tsta (Self), y")))
    ((stringp dest)
     (format out "~&~10Tsta ~a~a"
             (bare-data-assembly-symbol dest class-id)
             (if (zerop n) "" (format nil " + ~d" n))))
    (t
     (format out "~&~10Tsta ~a~a"
             (emit-6502-value dest)
             (if (zerop n) "" (format nil " + ~d" n)))))
  ;; After sta, A still holds the byte written. For a single-byte destination, note it so the
  ;; next load of the same lvalue can skip lda (avoids ldy/lda (Self),y after adc/sta to same slot).
  (if (and (= w 1) (zerop n))
      (cond
        ((slot-of-self-p dest)
         (%6502-note-accumulator-holds-value-of dest))
        ((and (stringp dest) (implicit-instance-slot-p dest class-id))
         (%6502-note-accumulator-holds-value-of dest))
        ((stringp dest)
         (%6502-note-accumulator-holds-value-of dest))
        (t (%invalidate-6502-accumulator-a)))
      (%invalidate-6502-accumulator-a)))

(defun 6502-dest-byte-address (dest class-id n w)
  "Return address string for byte N (0-based) of W-byte DEST, or nil if indirect (e.g. slot OF Self).
Used for STZ when MOVE ZERO to a direct-memory destination on 65c02+."
  (when (>= n w)
    (return-from 6502-dest-byte-address nil))
  (cond
    ((slot-of-self-p dest) nil)
    ((and (slot-of-expr dest) (not (slot-of-self-p dest))) nil)
    ((and (stringp dest) (implicit-instance-slot-p dest class-id)) nil)
    ((stringp dest)
     (let ((base (bare-data-assembly-symbol dest class-id)))
       (if (zerop n) base (format nil "~a + ~d" base n))))
    (t
     (let ((base (emit-6502-value dest)))
       (if (zerop n) base (format nil "~a + ~d" base n))))))

(defun emit-6502-move-two-slots-16bit-lax (out from to-dest class-id)
  "Copy 16 bits from source slot OF Self to dest slot OF Self.

Uses @code{lax (Self),y} for the low byte (A and X), @code{iny}/@code{lda (Self),y} for high,
stores high then @code{txa}/@code{sta (Self),y} for low — 6502 has no @code{stx (zp),y}.
NMOS 6502 only (@code{lax}); other CPUs use the generic byte loop."
  (let ((src (slot-symbol (second (slot-of-expr from)) class-id))
        (dst (slot-symbol (second (slot-of-expr to-dest)) class-id)))
    (format out "~&~10Tldy ~a" (emit-6502-immediate src))
    (format out "~&~10Tlax (Self), y")
    (format out "~&~10Tiny")
    (format out "~&~10Tlda (Self), y")
    (format out "~&~10Tldy ~a" (emit-6502-immediate (format nil "(~a+1)" dst)))
    (format out "~&~10Tsta (Self), y")
    (format out "~&~10Tdey")
    ;; 6502 has no stx (zp),y — only sta (zp),y; low byte is in X from lax.
    (format out "~&~10Ttxa")
    (format out "~&~10Tsta (Self), y"))
  (%invalidate-6502-accumulator-a))

(defun emit-6502-move-n-bytes (out from to-dest from-w to-w class-id &key sign-extend)
  "Copy FROM (from-w bytes) to TO-DEST (to-w bytes). Sign-extends if SIGN-EXTEND and from-w < to-w.
When FROM is literal zero and CPU has STZ (65c02+), uses stz for each byte when destination is direct."
  (flet ((emit-store-zero-byte (i)
           (let ((addr (6502-dest-byte-address to-dest class-id i to-w)))
             (if (and (6502-has-stz-p) addr)
                 (format out "~&~10Tstz ~a" addr)
                 (progn
                   (format out "~&~10Tlda ~a" (emit-6502-immediate 0))
                   (emit-6502-store-byte-n out to-dest class-id i to-w))))))
    (cond
      ((and (expr-is-literal-zero-p from) (6502-has-stz-p))
       ;; MOVE ZERO: use stz for each byte when destination has direct address
       (dotimes (i (min from-w to-w))
         (emit-store-zero-byte i))
       (when (and (< from-w to-w) (>= to-w 2))
         (if sign-extend
             (let ((lbl-zero (new-6502-label "MvZero"))
                   (lbl-done (new-6502-label "MvDone")))
               (emit-6502-load-byte-n out from class-id (1- from-w) from-w)
               (format out "~&~10Tbpl ~a" lbl-zero)
               (format out "~&~10Tlda #$ff")
               (dotimes (i (- to-w from-w))
                 (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w))
               (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-done)
               (format out "~&~a:" lbl-zero)
               (dotimes (i (- to-w from-w))
                 (emit-store-zero-byte (+ from-w i)))
               (format out "~&~a:" lbl-done))
             (dotimes (i (- to-w from-w))
               (emit-store-zero-byte (+ from-w i))))))
      ((and (= from-w 2) (= to-w 2)
            (slot-of-self-p from) (slot-of-self-p to-dest)
            (6502-use-undocumented-p))
       (emit-6502-move-two-slots-16bit-lax out from to-dest class-id))
      (t
       (dotimes (i (min from-w to-w))
         (emit-6502-load-byte-n out from class-id i from-w)
         (emit-6502-store-byte-n out to-dest class-id i to-w))
       (when (and (< from-w to-w) (>= to-w 2))
         (let ((lbl-zero (new-6502-label "MvZero"))
               (lbl-done (new-6502-label "MvDone")))
           (if sign-extend
               (progn
                 (emit-6502-load-byte-n out from class-id (1- from-w) from-w)
                 (format out "~&~10Tbpl ~a" lbl-zero)
                 (format out "~&~10Tlda #$ff")
                 (dotimes (i (- to-w from-w))
                   (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w))
                 (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-done)
                 (format out "~&~a:" lbl-zero)
                 (format out "~&~10Tlda ~a" (emit-6502-immediate 0))
                 (dotimes (i (- to-w from-w))
                   (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w))
                 (format out "~&~a:" lbl-done))
               (dotimes (i (- to-w from-w))
                 (format out "~&~10Tlda ~a" (emit-6502-immediate 0))
                 (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w)))))))))

;;; Operand width for byte/word arithmetic (PIC 99 vs PIC 9999)

(defun expr-contains-subscript-p (expr)
  "True if EXPR or any subexpression uses :subscript (array fetch).
Array fetches use X or Y; when true, avoid using X for temp storage."
  (cond
    ((and (listp expr) (eq (first expr) :subscript)) t)
    ((and (listp expr) (member (first expr) '(:add-expr :subtract-expr :multiply-expr :divide-expr
                                            :shift-left :shift-right :bit-and :bit-or :bit-xor :bit-not)))
     (or (expr-contains-subscript-p (second expr))
         (expr-contains-subscript-p (third expr))))
    ((and (listp expr) (eq (first expr) :of))
     (expr-contains-subscript-p (third expr)))
    (t nil)))

;;; MOVE statement
;;; operand-width uses *pic-width-table* (backend.lisp)

(defun compile-6502-move (out stmt class-id)
  (let* ((from (%move-from-resolve-constant-of-self (safe-getf (rest stmt) :from)))
         (to-dest (safe-getf (rest stmt) :to))
         (to-w (operand-width to-dest))
         (from-signed (operand-signed-p from))
         (to-signed (operand-signed-p to-dest))
         (sign-extend (and from-signed to-signed)))
    (flet ((load-index-into-x (idx-expr)
             "Load subscript index byte into X (see @code{emit-6502-load-expr-into-x})."
             (emit-6502-load-expr-into-x out idx-expr class-id)))
    (cond
      ;; MOVE NULL TO ptr — 6502: set pointer to NULL by zeroing high byte
      ((member from '(:null null "NULL") :test #'equal)
       (format out "~&~10Tlda ~a" (emit-6502-immediate 0))
       (emit-6502-store-byte-n out to-dest class-id (1- to-w) to-w))
      ;; Destination is subscripted: base(index) — load index into X, then store
      ((and (listp to-dest) (eq (first to-dest) :subscript))
       (load-index-into-x (third to-dest))
       (emit-6502-load-expr out from class-id)
       (format out "~&~10Tsta ~a" (emit-6502-value to-dest)))
      ;; Source is subscripted: use @code{emit-6502-move-n-bytes} (same as general case).
      ;; A dedicated @code{lda}/@code{sta} path used @code{emit-6502-value} on the destination,
      ;; which fails for @code{slot OF} non-Self (e.g. Waypoint-Y OF Current-Course): those
      ;; need @code{emit-6502-store-byte-n} and @code{sta (Ptr), y}.
      ;; Destination is slot OF Self
      ((slot-of-self-p to-dest)
       (let ((from-w (%6502-move-source-byte-width from)))
         (emit-6502-move-n-bytes out from to-dest from-w to-w class-id :sign-extend sign-extend)))
      ;; Source is slot OF Self, destination is a variable
      ((slot-of-self-p from)
       (let ((from-w (%6502-move-source-byte-width from)))
         (emit-6502-move-n-bytes out from to-dest from-w to-w class-id :sign-extend sign-extend)))
      ;; General: variable or constant to variable
      (t
       (let ((from-w (%6502-move-source-byte-width from)))
         (emit-6502-move-n-bytes out from to-dest from-w to-w class-id :sign-extend sign-extend)))))))

;;; STRING BLT (block transfer)
;;; STRING source DELIMITED BY SIZE INTO dest [LENGTH length]
;;; Source/dest may have reference modification: name(start:length)

(defun string-operand-address (operand class-id)
  "Return 64tass address expression for STRING operand (identifier or :refmod).
For :refmod, returns Base+offset for 1-based start."
  (declare (ignore class-id))
  (cond
    ((and (listp operand) (eq (first operand) :refmod))
     (let* ((base (safe-getf (rest operand) :base))
            (start (safe-getf (rest operand) :start))
            (base-str (cobol-id-to-assembly-symbol base)))
       (if (and (integerp start) (= start 1))
           base-str
           (format nil "~a+~d" base-str (1- start)))))
    (t
     (emit-6502-value operand))))

(defun string-operand-length-expr (operand stmt)
  "Return length expression for STRING: from :length clause or from :refmod."
  (or (safe-getf (rest stmt) :length)
      (when (and (listp operand) (eq (first operand) :refmod))
        (safe-getf (rest operand) :length))))

(defun resolve-length-constant (length-expr)
  "Resolve length expression to integer. Supports number, symbol (constant),
constant expression, or nil."
  (cond
    ((integerp length-expr) length-expr)
    ((and length-expr (expr-is-constant-p length-expr))
     (expr-constant-value length-expr))
    ((stringp length-expr) (constant-value length-expr))
    (t (constant-value (princ-to-string length-expr)))))

(defun compile-6502-string-blt (out stmt class-id)
  "Emit 6502 block copy loop for STRING source DELIMITED BY SIZE INTO dest."
  (let* ((source (safe-getf (rest stmt) :source))
         (dest (safe-getf (rest stmt) :dest))
         (length-expr (string-operand-length-expr source stmt))
         (len-val (resolve-length-constant length-expr)))
    (unless length-expr
      (error "EIGHTBOL: STRING DELIMITED BY SIZE requires LENGTH clause when source/dest have no reference modification"))
    (unless (and len-val (integerp len-val) (plusp len-val))
      (error "EIGHTBOL: STRING BLT length must be a positive integer constant"))
    (let ((src-addr (string-operand-address source class-id))
          (dst-addr (string-operand-address dest class-id)))
      (%invalidate-6502-accumulator-a)
      (format out "~&~10T;; STRING ~a DELIMITED BY SIZE INTO ~a (~d bytes)"
              (if (listp source) (safe-getf (rest source) :base) source)
              (if (listp dest) (safe-getf (rest dest) :base) dest)
              len-val)
      (format out "~&~10Tldy ~a" (emit-6502-immediate 0))
      (format out "~&~10T:_blt")
      (format out "~&~10Tlda ~a, y" src-addr)
      (format out "~&~10Tsta ~a, y" dst-addr)
      (format out "~&~10Tiny")
      (format out "~&~10Tcpy #~d" len-val)
      (format out "~&~10Tbne :_blt"))))

;;; GOTO and paragraph

(defun para-label (name class-id method-id)
  "Return assembly label for paragraph NAME within method.
   COBOL stabby-case (e.g. My-Para) maps to PascalCase (MyPara); underscores become part of one symbol."
  (cobol-id-to-assembly-symbol
   (substitute #\- #\_ (format nil "~a_~a_~a" class-id method-id name))))

(defun compile-6502-paragraph (out stmt class-id method-id)
  (let ((name (if (eq (first stmt) :paragraph)
                  (second stmt)
                  (or (safe-getf (rest stmt) :paragraph) (second stmt)))))
    (when name
      (format out "~&~a:" (para-label (format nil "~a" name) class-id (or method-id ""))))))

(defun compile-6502-goto (out stmt class-id method-id)
  (let ((target (safe-getf (rest stmt) :target))
        (targets (safe-getf (rest stmt) :targets))
        (dep (safe-getf (rest stmt) :depending-on)))
    (if dep
        ;; GO TO ... DEPENDING ON expr — trinary search tree (cmp/beq: equal, <, >)
        (let ((tgt-list (or targets (when target (list target)))))
          (emit-6502-load-expr out dep class-id)
          (emit-6502-goto-tristree out tgt-list 1 (length tgt-list)
                                   class-id method-id))
        ;; Simple GOTO
        (format out "~&~10T~a ~a"
                (6502-branch-always-mnemonic)
                (para-label (format nil "~a" (or target (first targets))) class-id (or method-id ""))))))

(defun emit-6502-goto-tristree (out targets lo hi class-id method-id)
  "Emit trinary search: cmp #mid; beq target_mid; blt left; bge right.
TARGETS is list of paragraph names (1-based indices). LO, HI are 1-based inclusive."
  (when (or (null targets) (> lo hi)) (return-from emit-6502-goto-tristree))
  (let ((mid (floor (+ lo hi) 2))
        (tgt-name (nth (1- mid) targets)))
    (when (null tgt-name) (return-from emit-6502-goto-tristree))
    (let ((lbl (para-label (format nil "~a" tgt-name) class-id (or method-id ""))))
      (if (= lo hi)
          ;; Leaf: single target
          (format out "~&~10Tcmp #~d~% beq ~a" mid lbl)
          (let ((lbl-less (new-6502-label "GtLess"))
                (lbl-more (new-6502-label "GtMore")))
            (format out "~&~10Tcmp #~d" mid)
            (format out "~&~10Tbeq ~a" lbl)
            (format out "~&~10Tblt ~a" lbl-less)
            (format out "~&~10Tbge ~a" lbl-more)
            (format out "~&~a:" lbl-less)
            (when (<= lo (1- mid))
              (emit-6502-goto-tristree out targets lo (1- mid) class-id method-id))
            (format out "~&~a:" lbl-more)
            (when (<= (1+ mid) hi)
              (emit-6502-goto-tristree out targets (1+ mid) hi class-id method-id)))))))

;;; EVALUATE statement

(defun compile-6502-evaluate (out stmt cpu)
  (let ((subject (safe-getf (rest stmt) :subject))
        (clauses (safe-getf (rest stmt) :when-clauses))
        (lbl-end (new-6502-label "EvalEnd")))
    (dolist (clause (ensure-list clauses))
      (cond
        ((eq (first clause) :when-other)
         ;; WHEN OTHER — fall-through, execute stmts
         (dolist (s (ensure-list (second clause)))
           (compile-statement cpu (first s) (rest s))))
        ((eq (first clause) :when)
         (let ((phrases (second clause))
               (stmts (third clause))
               (lbl-next (new-6502-label "WhenNext")))
           ;; Compare subject to phrases; if match, execute stmts and jump to end
           (cond
             ((eq phrases 'eightbol::any)
              ;; WHEN ANY — always match
              (dolist (s (ensure-list stmts))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-end))
             ((or (eq phrases 'eightbol::true) (equal phrases "TRUE"))
              (emit-6502-load-expr out subject *class-id*)
              (format out "~&~10Tbeq ~a" lbl-next)
              (dolist (s (ensure-list stmts))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-end)
              (format out "~&~a:" lbl-next))
             ((or (eq phrases 'eightbol::false) (equal phrases "FALSE"))
              (emit-6502-load-expr out subject *class-id*)
              (format out "~&~10Tbne ~a" lbl-next)
              (dolist (s (ensure-list stmts))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-end)
              (format out "~&~a:" lbl-next))
             ((and (listp phrases) (eq (first phrases) :not))
              ;; WHEN NOT expr — subject must not equal expr
              (emit-6502-load-expr out subject *class-id*)
              (if (expr-is-constant-p (second phrases))
                  (format out "~&~10Tcmp ~a" (emit-6502-immediate-operand (second phrases)))
                  (format out "~&~10Tcmp ~a" (emit-6502-value (second phrases))))
              (format out "~&~10Tbeq ~a" lbl-next)
              (dolist (s (ensure-list stmts))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-end)
              (format out "~&~a:" lbl-next))
             ((and (listp phrases) (member (first phrases) '(:through thru)))
              ;; WHEN expr1 THROUGH expr2 — subject in [lo, hi] inclusive
              (let ((lo (second phrases)) (hi (third phrases)))
                (emit-6502-load-expr out subject *class-id*)
                (if (expr-is-constant-p lo)
                    (format out "~&~10Tcmp ~a" (emit-6502-immediate-operand lo))
                    (format out "~&~10Tcmp ~a" (emit-6502-value lo)))
                (format out "~&~10Tblt ~a" lbl-next)
                ;; Upper bound: skip when subject > hi, i.e. subject >= hi + 1
                (if (and (expr-is-constant-p hi) (numberp hi))
                    (format out "~&~10Tcmp #~d" (1+ hi))
                    (progn
                      ;; subject - hi: if >= 1 then subject > hi, skip
                      (format out "~&~10Tsec")
                      (if (expr-is-constant-p hi)
                          (format out "~&~10Tsbc ~a" (emit-6502-immediate-operand hi))
                          (format out "~&~10Tsbc ~a" (emit-6502-value hi)))
                      (format out "~&~10Tcmp ~a" (emit-6502-immediate 1))))
                (format out "~&~10Tbge ~a" lbl-next)
                (%invalidate-6502-accumulator-a)
                (dolist (s (ensure-list stmts))
                  (compile-statement cpu (first s) (rest s)))
                (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-end)
                (format out "~&~a:" lbl-next)))
             (t
              ;; WHEN expr — equality
              (let ((phrase-expr (if (listp phrases) (second phrases) phrases)))
                (emit-6502-load-expr out subject *class-id*)
                (if (expr-is-constant-p phrase-expr)
                    (format out "~&~10Tcmp ~a" (emit-6502-immediate-operand phrase-expr))
                    (format out "~&~10Tcmp ~a" (emit-6502-value phrase-expr)))
                (format out "~&~10Tbne ~a" lbl-next))
              (dolist (s (ensure-list stmts))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-end)
              (format out "~&~a:" lbl-next)))))))
    (format out "~&~a:" lbl-end)))

;;; INSPECT statement

(defun compile-6502-inspect (out stmt class-id)
  (let ((target (safe-getf (rest stmt) :target))
        (tally (safe-getf (rest stmt) :tallying))
        (conv-from (safe-getf (rest stmt) :converting))
        (conv-to (safe-getf (rest stmt) :to))
        (repl-by (safe-getf (rest stmt) :by)))
    (%invalidate-6502-accumulator-a)
    (let ((tgt-sym (cobol-id-to-assembly-symbol target)))
      (cond
        (tally
         ;; INSPECT id TALLYING tally FOR CHARACTERS — add 1 to tally per character
         (let ((tally-sym (cobol-id-to-assembly-symbol tally)))
           (format out "~&~10T;; INSPECT ~a TALLYING ~a FOR CHARACTERS" target tally)
           (format out "~&~10Tldy ~a" (emit-6502-immediate 0))
           (let ((lbl (new-6502-label "TallyLoop"))
                 (lbl-done (new-6502-label "TallyDone"))
                 (lbl-skip (new-6502-label "TallySkip")))
             (format out "~&~a:" lbl)
             (format out "~&~10Tlda ~a, y" tgt-sym)
             (format out "~&~10Tbeq ~a" lbl-done)
             (format out "~&~10Tlda ~a" tally-sym)
             (format out "~&~10Tclc")
             (format out "~&~10Tadc ~a" (emit-6502-immediate 1))
             (format out "~&~10Tsta ~a" tally-sym)
             (format out "~&~10Tblt ~a" lbl-skip)
             (format out "~&~10Tinc ~a + 1" tally-sym)
             (format out "~&~a:" lbl-skip)
             (format out "~&~10Tiny")
             (format out "~&~10Tbne ~a" lbl)
             (format out "~&~a:" lbl-done))))
        (conv-from
         ;; INSPECT id CONVERTING from TO to — replace chars in string
         (format out "~&~10T;; INSPECT ~a CONVERTING" target)
         (format out "~&~10Tldy ~a" (emit-6502-immediate 0))
         (let ((lbl (new-6502-label "InspLoop"))
               (lbl-next (new-6502-label "InspNext"))
               (lbl-done (new-6502-label "InspDone")))
           (format out "~&~a:" lbl)
           (format out "~&~10Tlda ~a, y" tgt-sym)
           (format out "~&~10Tbeq ~a" lbl-done)
           (if (expr-is-constant-p conv-from)
               (format out "~&~10Tcmp ~a" (emit-6502-immediate-operand conv-from))
               (format out "~&~10Tcmp ~a" (emit-6502-value conv-from)))
           (format out "~&~10Tbne ~a" lbl-next)
           (if (expr-is-constant-p conv-to)
               (format out "~&~10Tlda ~a" (emit-6502-immediate-operand conv-to))
               (format out "~&~10Tlda ~a" (emit-6502-value conv-to)))
           (format out "~&~10Tsta ~a, y" tgt-sym)
           (format out "~&~a:" lbl-next)
           (format out "~&~10Tiny")
           (format out "~&~10Tbne ~a" lbl)
           (format out "~&~a:" lbl-done)))
        (repl-by
         ;; INSPECT id REPLACING CHARACTERS BY expr — fill string with expr
         (let ((len (or (pic-size (format nil "~a" target)) 64)))
           (format out "~&~10T;; INSPECT ~a REPLACING CHARACTERS BY ~a" target repl-by)
           (emit-6502-load-expr out repl-by class-id)
           (format out "~&~10Tldy ~a" (emit-6502-immediate 0))
           (let ((lbl (new-6502-label "InspLoop")))
             (format out "~&~a:" lbl)
             (format out "~&~10Tsta ~a, y" tgt-sym)
             (format out "~&~10Tiny")
             (format out "~&~10Tcpy #~d" len)
             (format out "~&~10Tbne ~a" lbl))))))))

;;; INVOKE statement

(defun compile-6502-invoke (out stmt class-id)
  (let ((object (safe-getf (rest stmt) :object))
        (method (safe-getf (rest stmt) :method))
        (returning (safe-getf (rest stmt) :returning))
        (tail-call-p (safe-getf (rest stmt) :tail-call-p)))
    (let ((method-sym (cobol-id-to-assembly-symbol (if (stringp method) method
                                                       (format nil "~a" method))))
          (jmp-p (and tail-call-p (null returning))))
      (cond
        ((member object '(:self "Self" self) :test #'equal)
         ;; Same OOPS path as INVOKE on a named object: Phantasia CallMethod macro (DoCallMethod).
         (format out "~&~10T.CallMethod Call~a~a, ~aClass"
                 class-id method-sym class-id))
        (t
         (let* ((obj-name (cobol-id-to-assembly-symbol object))
                (obj-class (or (var-class obj-name) "Unknown")))
           (if (string= obj-class "Unknown")
               (format out "~&~10T~a Call~aMethod" (if jmp-p "jmp" "jsr") method-sym)
               (format out "~&~10T.CallMethod Call~a~a, ~aClass, ~a"
                       obj-class method-sym obj-class obj-name))))))
    (%invalidate-6502-accumulator-a)
    (when returning
      (format out "~&~10Tsta ~a" (cobol-id-to-assembly-symbol returning)))))

;;; CALL statement

(defun sym-string (x)
  "Return PascalCase assembly symbol from a literal, identifier, or string (COBOL stabby-case)."
  (cobol-id-to-assembly-symbol (if (listp x) (second x) x)))

(defun %service-call-dispatch-symbol (item-sym)
  "Map CALL SERVICE routine stem to assembly dispatch label (@code{ServiceFoo} for LUT / .FarCall).
When ITEM-SYM already has a @code{Service} prefix (case-insensitive), return it unchanged."
  (let ((s (if (stringp item-sym) item-sym (format nil "~a" item-sym))))
    (if (and (>= (length s) 7)
             (string-equal (subseq s 0 7) "Service"))
        s
        (concatenate 'string "Service" s))))

(defun compile-6502-call (out stmt)
  (let* ((target (safe-getf (rest stmt) :target))
         (service (safe-getf (rest stmt) :service))
         (bank (safe-getf (rest stmt) :bank))
         (libraryp (safe-getf (rest stmt) :library))
         (tail-call-p (safe-getf (rest stmt) :tail-call-p))
         (item (or service target))
         (item-sym (sym-string item))
         (dispatch-sym (if service (%service-call-dispatch-symbol item-sym) item-sym))
         (resolved-bank (or bank (service-bank-table-lookup dispatch-sym)))
         (jmp-p tail-call-p))
    (cond
      ;; CALL target IN SERVICE bank. / CALL SERVICE target. — service dispatch
      (service
       (if resolved-bank
           (let ((bank-sym (sym-string resolved-bank)))
             (format out "~&~10T.FarCall ~a, ~a" dispatch-sym bank-sym))
           (error "EIGHTBOL: CALL ~a IN SERVICE requires bank (not in service-bank table)"
                  dispatch-sym)))
      ;; CALL target IN LIBRARY. — always call LastBank library thunk label.
      ;; Emits jsr Lib.<RoutineName> (e.g. CALL Move-Decal-Y IN LIBRARY
      ;; => jsr Lib.MoveDecalY), regardless of service LUT entries.
      (libraryp
       (format out "~&~10Tjsr Lib.~a" item-sym))
      ;; CALL target IN BANK bank. — bank-switched far call
      (bank
       (let ((bank-sym (sym-string bank)))
         (format out "~&~10T.FarJSR ~a, ~a" bank-sym item-sym)))
      ;; CALL ServiceName. — bare target matches *service-bank-table* (same as dispatch); far call.
      ;; Tail @code{:tail-call-p} must not become @code{jmp} — cross-bank services need @code{.FarCall}.
      ((and resolved-bank (not service) (not bank) (not libraryp))
       (format out "~&~10T.FarCall ~a, ~a" item-sym (sym-string resolved-bank)))
      ;; CALL target. — local near call (unknown label in current bank)
      (t
       (format out "~&~10T~a ~a" (if jmp-p "jmp" "jsr") item-sym)))
    (%invalidate-6502-accumulator-a)))

;;; IF / conditional compilation

(defvar *6502-label-counter* 0)

(defun new-6502-label (prefix)
  "Generate unique label with meaningful name (L prefix, letter-start for 64tass)."
  (format nil "L~a~d" prefix (incf *6502-label-counter*)))

(defun compile-6502-if (out stmt cpu)
  (let ((condition (safe-getf (rest stmt) :condition))
        (then-stmts (safe-getf (rest stmt) :then))
        (else-stmts (safe-getf (rest stmt) :else))
        (lbl-else (new-6502-label "IfElse"))
        (lbl-end (new-6502-label "IfEnd")))
    (emit-6502-condition out condition *class-id* lbl-else)
    (dolist (s (ensure-list then-stmts))
      (compile-statement cpu (first s) (rest s)))
    (when (and else-stmts (not (null else-stmts)))
      (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-end))
    (format out "~&~a:" lbl-else)
    (when (and else-stmts (not (null else-stmts)))
      (dolist (s (ensure-list else-stmts))
        (compile-statement cpu (first s) (rest s)))
      (format out "~&~a:" lbl-end))))

(defun normalize-relation-condition (condition)
  "If CONDITION is (lhs op rhs) with op in the middle, return (op lhs rhs).
If CONDITION is (lhs IS LESS THAN rhs) or (lhs LESS THAN rhs) etc. (5 elements),
return (op lhs rhs). Otherwise return CONDITION unchanged."
  (when (listp condition)
    (cond
      ;; 6-element infix negated equality: (lhs IS NOT EQUAL TO rhs)
      ((= (length condition) 6)
       (destructuring-bind (a b c d e f) condition
         (flet ((tok (x str) (string-equal (princ-to-string x) str)))
           (when (and (tok b "IS")
                      (tok c "NOT")
                      (or (tok d "=") (tok d "EQUAL"))
                      (tok e "TO"))
             (return-from normalize-relation-condition
               (list :not (list '= a f)))))))
      ;; 4-element infix negated equality: (lhs NOT = rhs) / (lhs NOT EQUAL rhs)
      ((= (length condition) 4)
       (destructuring-bind (a b c d) condition
         (flet ((tok (x str) (string-equal (princ-to-string x) str)))
           (when (and (tok b "NOT")
                      (or (tok c "=") (tok c "EQUAL")))
             (return-from normalize-relation-condition
               (list :not (list '= a d)))))))
      ;; 5-element: (expr is less than expr) or (expr less than expr), etc.
      ((= (length condition) 5)
       (destructuring-bind (a b c d e) condition
         (flet ((tok (x str) (string-equal (princ-to-string x) str)))
           (let ((op (cond ((and (tok b "IS") (tok c "LESS") (tok d "THAN")) '<)
                           ((and (tok b "LESS") (tok c "THAN")) '<)
                           ((and (tok b "IS") (tok c "GREATER") (tok d "THAN")) '>)
                           ((and (tok b "GREATER") (tok c "THAN")) '>)
                           (t nil))))
             (when op (return-from normalize-relation-condition (list op a e)))))))
      ;; 3-element: (lhs op rhs) -> (op lhs rhs)
      ((= (length condition) 3)
       (let ((a (first condition))
             (b (second condition))
             (c (third condition)))
         (when (member b '(= equal < less > greater >= ≤ ≥ <= ">=" "<=" ">" "<" "="
                            :> :< :>= :<=)
                       :test #'equal)
           (return-from normalize-relation-condition (list b a c)))))))
  condition)

(defun emit-6502-false-when-not-unsigned-greater-than-zero (out lhs class-id branch-label)
  "Emit code that jumps to BRANCH-LABEL when unsigned LHS is not greater than zero.

For rhs zero, @code{unsigned > 0} is equivalent to “any byte non-zero” for multi-byte values.
Uses @code{lda} zero flag for width 1 (no redundant @code{cmp #0}). Avoids @code{blt} after
@code{cmp #0} (unsigned nothing is below zero)."
  (let ((w (max 1 (operand-width lhs))))
    (cond
      ((= w 1)
       (emit-6502-load-expr out lhs class-id)
       (format out "~&~10Tbeq ~a" branch-label))
      (t
       (let ((lbl-then (new-6502-label "IfGT0")))
         (dotimes (i (1- w))
           (emit-6502-load-byte-n out lhs class-id i w)
           (format out "~&~10Tbne ~a" lbl-then))
         (emit-6502-load-byte-n out lhs class-id (1- w) w)
         (format out "~&~10Tbeq ~a" branch-label)
         (format out "~&~a:" lbl-then))))))

(defun emit-6502-branch-if-expr-not-all-zero (out expr class-id branch-label)
  "Jump to BRANCH-LABEL if unsigned EXPR is not all-zero bytes.

Used for @code{IF (IS ZERO X)}, @code{(= X 0)}, and @code{(= 0 X)}: the false branch
runs when any byte is non-zero. W is @code{operand-width} of EXPR; W=1 uses
@code{emit-6502-load-expr} + @code{bne}.

@table @asis
@item EXPR
Slot, identifier, or other loadable expression.
@item CLASS-ID
Current class for slot symbols.
@item BRANCH-LABEL
Label when value is not all zeros (condition false for IS-ZERO).
@end table"
  (let ((w (max 1 (operand-width expr))))
    (if (= w 1)
        (progn
          (emit-6502-load-expr out expr class-id)
          (format out "~&~10Tbne ~a" branch-label))
        (dotimes (i w)
          (emit-6502-load-byte-n out expr class-id i w)
          (format out "~&~10Tbne ~a" branch-label)))))

(defun emit-6502-branch-if-expr-all-zero (out expr class-id branch-label)
  "Jump to BRANCH-LABEL if unsigned EXPR is all-zero bytes (W-wide).

Used for @code{IS NOT ZERO} false path and @code{(NOT (= X 0))} when X is zero:
condition is false when every byte is zero. W=1 uses @code{emit-6502-load-expr} + @code{beq}.

@table @asis
@item EXPR
Slot or identifier.
@item CLASS-ID
Current class for slot symbols.
@item BRANCH-LABEL
Label when value is all zeros (IS-NOT-ZERO is false).
@end table"
  (let ((w (max 1 (operand-width expr))))
    (if (= w 1)
        (progn
          (emit-6502-load-expr out expr class-id)
          (format out "~&~10Tbeq ~a" branch-label))
        (let ((lbl-some (new-6502-label "SomeNz")))
          (dotimes (i w)
            (emit-6502-load-byte-n out expr class-id i w)
            (format out "~&~10Tbne ~a" lbl-some))
          (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) branch-label)
          (format out "~&~a:" lbl-some)))))

(defun relation-op-canonical (op)
  "If OP is a comparison operator in any form, return canonical string \">\", \"<\", \">=\", or \"<=\".
Otherwise return NIL."
  (cond ((member op '(> greater) :test #'equal) ">")
        ((member op '(< less) :test #'equal) "<")
        ((member op '(>= ≥) :test #'equal) ">=")
        ((member op '(<= ≤) :test #'equal) "<=")
        (t (let ((s (princ-to-string op)))
             (cond ((or (string= s ">") (string= s ":>")) ">")
                   ((or (string= s "<") (string= s ":<")) "<")
                   ((or (string= s ">=") (string= s ":>=")) ">=")
                   ((or (string= s "<=") (string= s ":<=")) "<=")
                   (t nil))))))

(defun emit-6502-condition (out condition class-id branch-label)
  "Emit 6502 code to evaluate CONDITION and branch to BRANCH-LABEL if false."
  (when (listp condition)
    (let* ((condition (or (normalize-relation-condition condition) condition))
          (op (first condition)))
      (cond
        ;; 3-element comparison: op as string or symbol (princ-to-string covers both)
        ((and (= (length condition) 3)
              (let ((s (princ-to-string (first condition))))
                (member s '(">" "<" ">=" "<=") :test #'string=)))
         (let ((rel (princ-to-string (first condition)))
               (lhs (second condition))
               (rhs (third condition)))
           (if (and (string= rel ">") (expr-is-literal-zero-p rhs))
               (emit-6502-false-when-not-unsigned-greater-than-zero out lhs class-id branch-label)
               (progn
                 (emit-6502-compare out lhs rhs class-id)
                 (cond ((string= rel ">")
                        (format out "~&~10Tblt ~a" branch-label)
                        (format out "~&~10Tbeq ~a" branch-label))
                       ((string= rel "<")
                        (format out "~&~10Tbge ~a" branch-label))
                       ((string= rel ">=")
                        (format out "~&~10Tblt ~a" branch-label))
                       ((string= rel "<=")
                        (let ((lbl-stay (new-6502-label "LeStay")))
                          (format out "~&~10Tbeq ~a" lbl-stay)
                          (format out "~&~10Tbge ~a" branch-label)
                          (format out "~&~a:" lbl-stay))))))))
        ;; 3-element comparison by canonical op (symbol, keyword, etc.)
        ((and (= (length condition) 3)
              (let ((rel (relation-op-canonical op))
                    (lhs (second condition))
                    (rhs (third condition)))
                (when rel
                  (if (and (string= rel ">") (expr-is-literal-zero-p rhs))
                      (emit-6502-false-when-not-unsigned-greater-than-zero out lhs class-id branch-label)
                      (progn
                        (emit-6502-compare out lhs rhs class-id)
                        (cond ((string= rel ">")
                               (format out "~&~10Tblt ~a" branch-label)
                               (format out "~&~10Tbeq ~a" branch-label))
                              ((string= rel "<")
                               (format out "~&~10Tbge ~a" branch-label))
                              ((string= rel ">=")
                               (format out "~&~10Tblt ~a" branch-label))
                              ((string= rel "<=")
                               (let ((lbl-stay (new-6502-label "LeStay")))
                                 (format out "~&~10Tbeq ~a" lbl-stay)
                                 (format out "~&~10Tbge ~a" branch-label)
                                 (format out "~&~a:" lbl-stay)))
                              (t nil))))
                  t))))
        ;; Relation with op as first element (after normalization) — legacy branch
        ((and (= (length condition) 3)
              (let ((s (princ-to-string op)))
                (or (string= s ">") (string= s "<") (string= s ">=") (string= s "<="))))
         (let ((rel-op (princ-to-string op))
               (lhs (second condition))
               (rhs (third condition)))
           (cond
             ((string= rel-op ">")
              (if (expr-is-literal-zero-p rhs)
                  (emit-6502-false-when-not-unsigned-greater-than-zero out lhs class-id branch-label)
                  (progn
                    (emit-6502-compare out lhs rhs class-id)
                    (format out "~&~10Tblt ~a" branch-label)
                    (format out "~&~10Tbeq ~a" branch-label))))
             ((string= rel-op "<")
              (emit-6502-compare out lhs rhs class-id)
              (format out "~&~10Tbge ~a" branch-label))
             ((string= rel-op ">=")
              (emit-6502-compare out lhs rhs class-id)
              (format out "~&~10Tblt ~a" branch-label))
             ((string= rel-op "<=")
              (let ((lbl-stay (new-6502-label "LeStay")))
                (emit-6502-compare out lhs rhs class-id)
                (format out "~&~10Tbeq ~a" lbl-stay)
                (format out "~&~10Tbge ~a" branch-label)
                (format out "~&~a:" lbl-stay))))))
        ;; Equality with zero (IS ZERO / IS EQUAL TO 0)
        ((and (member op '(= equal))
              (or (expr-is-literal-zero-p (second condition))
                  (expr-is-literal-zero-p (third condition))))
         (let ((slot-expr (if (expr-is-literal-zero-p (second condition))
                              (third condition)
                              (second condition))))
           (emit-6502-branch-if-expr-not-all-zero out slot-expr class-id branch-label)))
        ;; NULL pointer test — 6502: NULL = any pointer with high byte zero
        ((and (member op '(= equal))
              (or (member (second condition) '(:null null "NULL") :test #'equal)
                  (member (third condition) '(:null null "NULL") :test #'equal)))
         (let ((ptr-expr (if (member (second condition) '(:null null "NULL") :test #'equal)
                             (third condition)
                             (second condition))))
           (emit-6502-load-hi-byte out ptr-expr class-id)
           (format out "~&~10Tbne ~a" branch-label)))
        ;; IS NOT NULL — must come before Negated zero test so (:not (= x :null)) matches here
        ((and (eq op :not)
              (listp (second condition))
              (member (first (second condition)) '(= equal))
              (or (member (second (second condition)) '(:null null "NULL") :test #'equal)
                  (member (third (second condition)) '(:null null "NULL") :test #'equal)))
         (let* ((inner (second condition))
                (ptr-expr (if (member (second inner) '(:null null "NULL") :test #'equal)
                              (third inner)
                              (second inner))))
           (emit-6502-load-hi-byte out ptr-expr class-id)
           (format out "~&~10Tbeq ~a" branch-label)))
        ;; Negated zero test (IS NOT ZERO / NOT EQUAL TO 0)
        ((and (eq op :not)
              (listp (second condition))
              (member (first (second condition)) '(= equal)))
         (let ((inner (second condition)))
           (if (or (expr-is-literal-zero-p (second inner))
                   (expr-is-literal-zero-p (third inner)))
               (let ((slot-expr (if (expr-is-literal-zero-p (second inner))
                                    (third inner)
                                    (second inner))))
                 (emit-6502-branch-if-expr-all-zero out slot-expr class-id branch-label))
               (let* ((left (second inner))
                      (right (third inner))
                      (w (max (operand-width left) (operand-width right))))
                 (cond
                   ((= w 1)
                    (emit-6502-load-expr out left class-id)
                    (if (expr-is-constant-p right)
                        (format out "~&~10Tcmp ~a" (emit-6502-immediate (expr-constant-value right)))
                        (emit-6502-cmp-memory-rhs out right class-id))
                    ;; Condition is false when left == right.
                    (format out "~&~10Tbeq ~a" branch-label))
                   ((>= w 2)
                    (let ((lbl-done (new-6502-label "NotEqDone")))
                      (dotimes (i w)
                        (emit-6502-load-byte-n out left class-id i w)
                        (if (expr-is-constant-p right)
                            (format out "~&~10Tcmp ~a"
                                    (emit-6502-immediate (constant-byte-value right i)))
                            (emit-6502-cmp-byte-n-of-expr out right class-id i w))
                        ;; Any mismatch means NOT (= ...) is true, so skip false-branch.
                        (format out "~&~10Tbne ~a" lbl-done))
                      ;; All bytes equal => condition false.
                      (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) branch-label)
                      (format out "~&~a:" lbl-done)))
                   (t
                    (emit-6502-generic-condition out condition class-id branch-label)))))))
        ;; IS ZERO
        ((eq op :is-zero)
         (emit-6502-branch-if-expr-not-all-zero out (second condition) class-id branch-label))
        ;; IS NOT ZERO
        ((eq op :is-not-zero)
         (emit-6502-branch-if-expr-all-zero out (second condition) class-id branch-label))
        ;; BIT-AND — bit test: zero flag set if masked result is zero
        ((eq op :bit-and)
         ;; Reuse emit-6502-load-expr for the full bit-and expression
         (emit-6502-load-expr out condition class-id)
         (format out "~&~10Tbeq ~a" branch-label))
        ;; Unsigned greater-than (op may be symbol or string from parser)
        ((or (member op '(> greater))
             (string= (princ-to-string op) ">"))
         (if (expr-is-literal-zero-p (third condition))
             (emit-6502-false-when-not-unsigned-greater-than-zero out (second condition) class-id branch-label)
             (progn
               (emit-6502-compare out (second condition) (third condition) class-id)
               (format out "~&~10Tblt ~a" branch-label)
               (format out "~&~10Tbeq ~a" branch-label))))
        ;; Unsigned greater-than-or-equal (>=)
        ((or (member op '(>= ≥)) (string= (princ-to-string op) ">="))
         (emit-6502-compare out (second condition) (third condition) class-id)
         (format out "~&~10Tblt ~a" branch-label))
        ;; Unsigned less-than
        ((or (member op '(< less)) (string= (princ-to-string op) "<"))
         (emit-6502-compare out (second condition) (third condition) class-id)
         (format out "~&~10Tbge ~a" branch-label))
        ;; Unsigned less-than-or-equal (<=): branch when lhs > rhs
        ((or (member op '(<= ≤)) (string= (princ-to-string op) "<="))
         (let ((lbl-stay (new-6502-label "LeStay")))
           (emit-6502-compare out (second condition) (third condition) class-id)
           (format out "~&~10Tbeq ~a" lbl-stay)
           (format out "~&~10Tbge ~a" branch-label)
           (format out "~&~a:" lbl-stay)))
        ;; General equality (= lhs rhs) — load lhs, cmp rhs, bne if false
        ((member op '(= equal))
         (let ((left (second condition))
               (right (third condition)))
           (let ((w (max (operand-width left) (operand-width right))))
             (cond
               ((= w 1)
                (emit-6502-load-expr out left class-id)
                (if (expr-is-constant-p right)
                    (format out "~&~10Tcmp ~a" (emit-6502-immediate (expr-constant-value right)))
                    (emit-6502-cmp-memory-rhs out right class-id))
                (format out "~&~10Tbne ~a" branch-label))
               ((>= w 2)
                (dotimes (i w)
                  (emit-6502-load-byte-n out left class-id i w)
                  (if (expr-is-constant-p right)
                      (format out "~&~10Tcmp ~a"
                              (emit-6502-immediate (constant-byte-value right i)))
                      (emit-6502-cmp-byte-n-of-expr out right class-id i w))
                  (format out "~&~10Tbne ~a" branch-label)))
               (t
                (emit-6502-load-expr out left class-id)
                (if (expr-is-constant-p right)
                    (format out "~&~10Tcmp ~a" (emit-6502-immediate (expr-constant-value right)))
                    (emit-6502-cmp-memory-rhs out right class-id))
                (format out "~&~10Tbne ~a" branch-label))))))
        ;; AND — short-circuit: if first is false, branch; else evaluate second
        ((eq op :and)
         (emit-6502-condition out (second condition) class-id branch-label)
         (emit-6502-condition out (third condition) class-id branch-label))
        ;; OR — short-circuit: if first is true, skip to success; else evaluate second
        ((eq op :or)
         (let ((lbl-cond2 (new-6502-label "OrCond2"))
               (lbl-skip (new-6502-label "OrSkip")))
           (emit-6502-condition out (second condition) class-id lbl-cond2)
           (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-skip)
           (format out "~&~a:" lbl-cond2)
           (emit-6502-condition out (third condition) class-id branch-label)
           (format out "~&~a:" lbl-skip)))
        ;; Generic fallback
        (t (emit-6502-generic-condition out condition class-id branch-label))))))

(defun emit-6502-compare-unsigned (out lhs rhs class-id w)
  "Emit unsigned compare of LHS vs RHS for W-byte values (little-endian in memory).

Unsigned semantics: treat each W-byte value as an integer; compare from the most significant
stored byte (index @code{W-1}) to least (index @code{0}). If any byte differs, @code{bne} skips
remaining compares; the last @code{cmp} (low byte) only runs when all higher bytes were equal.
Last @code{cmp} leaves flags for @code{emit-6502-condition} (@code{blt}/@code{bge}/@code{beq} on 6502)."
  (let ((lbl-done (new-6502-label "CmpU")))
    (when (< w 2)
      (error "emit-6502-compare-unsigned: w ~d < 2" w))
    (loop for i from (1- w) downto 1 do
      (emit-6502-load-byte-n out lhs class-id i w)
      (emit-6502-cmp-byte-n-of-expr out rhs class-id i w)
      (format out "~&~10Tbne ~a" lbl-done))
    (emit-6502-load-byte-n out lhs class-id 0 w)
    (emit-6502-cmp-byte-n-of-expr out rhs class-id 0 w)
    (format out "~&~a:" lbl-done)))

(defun emit-6502-compare (out lhs rhs class-id)
  "Emit unsigned compare LHS vs RHS for relational IF branches.

When max @code{operand-width} of LHS and RHS is 1: @code{lda} LHS then @code{cmp} RHS.
When width is 2 or more: @code{emit-6502-compare-unsigned} (high byte first, then low).

@table @asis
@item LHS, RHS
Expressions; widths from @code{*pic-width-table*}.
@item CLASS-ID
Current class (slot labels).
@end table"
  (let ((w (max (operand-width lhs) (operand-width rhs))))
    (if (<= w 1)
        (progn
          (emit-6502-load-expr out lhs class-id)
          (emit-6502-cmp-memory-rhs out rhs class-id))
        (emit-6502-compare-unsigned out lhs rhs class-id w))))

(defun emit-6502-generic-condition (out condition class-id branch-label)
  "Signal error for conditions not yet implemented."
  (declare (ignore out class-id branch-label))
  (error "EIGHTBOL: condition ~s not implemented" condition))

;;; ADD / SUBTRACT / COMPUTE

(defun literal-one-p (expr)
  "True when EXPR is the integer 1, string \"1\", or (:literal 1)."
  (or (eql expr 1)
      (equal expr "1")
      (and (listp expr) (eq (first expr) :literal) (eql (second expr) 1))))

(defun compile-6502-add (out stmt class-id)
  ;; LET* so RESULT and W use GIVING / TO from this stmt (parallel LET leaves RESULT nil).
  (let* ((from (safe-getf (rest stmt) :from))
         (to-op (safe-getf (rest stmt) :to))
         (giving (safe-getf (rest stmt) :giving))
         (result (or giving to-op))
         (w (max (operand-width result)
                 (expression-operand-width from)
                 (operand-width to-op)))
         (bcd-p (when result (usage-bcd-p (expr-to-width-name result)))))
    (unless (or giving to-op)
      (error 'backend-error
             :message "ADD requires TO or GIVING"
             :cpu :6502
             :detail stmt))
    (cond
    ;; Optimise: ADD 1 TO variable (no GIVING) → inc variable (byte only)
    ((and (literal-one-p from) (not giving) (stringp to-op) (= (operand-width to-op) 1))
     (format out "~&~10Tinc ~a" (bare-data-assembly-symbol to-op class-id))
     (%invalidate-6502-accumulator-a))
    ;; Multi-byte ADD (w >= 2): result = from + to, carry propagates
    ((>= w 2)
     (when (and bcd-p (> w 2))
       (error "EIGHTBOL: BCD ADD with width ~d not yet implemented" w))
     (if (> w 2)
         (progn
           (when bcd-p (format out "~&~10Tsed"))
           (format out "~&~10Tclc")
           (dotimes (i w)
             (emit-6502-load-byte-n out from class-id i w)
             (if (expr-is-constant-p to-op)
                 (format out "~&~10Tadc ~a" (emit-6502-immediate (constant-byte-value to-op i)))
                 (emit-6502-adc-byte-n-of-expr out to-op class-id i w))
             (emit-6502-store-byte-n out result class-id i w
               :skip-ldy (and (equal from result)
                              (%6502-load-byte-n-sets-y-for-slot-store from class-id))))
           (when bcd-p (format out "~&~10Tcld")))
         (let ((use-stack (expr-contains-subscript-p from)))
           (when (or use-stack (expr-contains-subscript-p to-op) (expr-contains-subscript-p result))
             (setf use-stack t))
           ;; Store low sum at (Self),y immediately after low adc when Y is already the
           ;; destination low offset (TO and result same slot OF Self, no subscript).
           (let ((store-low-to-self-now-p
                   (and (not use-stack)
                        (not (expr-is-constant-p to-op))
                        (slot-of-self-p to-op)
                        (slot-of-self-p result)
                        (equal (slot-of-expr to-op) (slot-of-expr result)))))
             (when bcd-p (format out "~&~10Tsed"))
             (emit-6502-load-expr out from class-id)
             (format out "~&~10Tclc")
             (if (expr-is-constant-p to-op)
                 (let ((v (expr-constant-value to-op)))
                   (format out "~&~10Tadc #<~d" v))
                 (cond
                   ((slot-of-self-p to-op)
                    (let ((off (slot-symbol (second (slot-of-expr to-op)) class-id)))
                      (format out "~&~10Tldy #~a" off)
                      (format out "~&~10Tadc (Self), y")))
                   ((and (slot-of-expr to-op) (not (slot-of-self-p to-op)))
                    (emit-6502-alu-with-memory-rhs out "adc" to-op class-id))
                   (t
                    (format out "~&~10Tadc ~a" (emit-6502-value to-op)))))
             (if store-low-to-self-now-p
                 (format out "~&~10Tsta (Self), y")
                 (if use-stack
                     (format out "~&~10Tpha")
                     (format out "~&~10Ttax")))
             (emit-6502-load-hi-byte out from class-id)
             (if (expr-is-constant-p to-op)
                 (let ((v (expr-constant-value to-op)))
                   (format out "~&~10Tadc #>~d" v))
                 (cond
                   ((slot-of-self-p to-op)
                    (let ((off (slot-symbol (second (slot-of-expr to-op)) class-id)))
                      (format out "~&~10Tldy #~a" (format nil "(~a+1)" off))
                      (format out "~&~10Tadc (Self), y")))
                   ((and (slot-of-expr to-op) (not (slot-of-self-p to-op)))
                    (let* ((sof (slot-of-expr to-op))
                           (off (slot-symbol (second sof) class-id))
                           (ptr (6502-object-pointer-label (third sof) class-id)))
                      (format out "~&~10Tldy #~a" (format nil "(~a+1)" off))
                      (format out "~&~10Tadc (~a), y" ptr)))
                   (t
                    (format out "~&~10Tadc ~a + 1" (emit-6502-value to-op)))))
             (if (slot-of-self-p result)
                 (if store-low-to-self-now-p
                     ;; Y already holds high-byte index from ldy #(slot+1) before adc (Self),y.
                     (format out "~&~10Tsta (Self), y")
                     (let ((n (slot-of-expr result)))
                       (format out "~&~10Tldy #~a + 1" (slot-symbol (second n) class-id))
                       (format out "~&~10Tsta (Self), y")
                       (format out "~&~10Tdey")
                       (format out "~&~10T~a" (if use-stack "pla" "txa"))
                       (format out "~&~10Tsta (Self), y")))
                 (if (and (slot-of-expr result) (not (slot-of-self-p result)))
                     (let* ((n (slot-of-expr result))
                            (off (slot-symbol (second n) class-id))
                            (ptr (6502-object-pointer-label (third n) class-id)))
                       (format out "~&~10Tldy #~a + 1" off)
                       (format out "~&~10Tsta (~a), y" ptr)
                       (format out "~&~10Tdey")
                       (format out "~&~10T~a" (if use-stack "pla" "txa"))
                       (format out "~&~10Tsta (~a), y" ptr))
                     (let ((res-sym (emit-6502-value result)))
                       (format out "~&~10Tsta ~a + 1" res-sym)
                       (format out "~&~10T~a" (if use-stack "pla" "txa"))
                       (format out "~&~10Tsta ~a" res-sym)))))
           (when bcd-p (format out "~&~10Tcld"))
           ;; A holds the low byte after the last sta, not the full W-byte value — do not reuse as HP.
           (%invalidate-6502-accumulator-a))))
    ;; General 8-bit case
    (t
     (emit-6502-load-expr out from class-id)
     (when bcd-p (format out "~&~10Tsed"))
     (format out "~&~10Tclc")
     (if giving
         (progn
           (if (expr-is-constant-p to-op)
               (format out "~&~10Tadc ~a" (emit-6502-immediate-operand to-op))
               (cond
                 ((slot-of-self-p to-op)
                  (let ((n (slot-of-expr to-op)))
                    (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol (second n) class-id)))
                    (format out "~&~10Tadc (Self), y")))
                 ((and (slot-of-expr to-op) (not (slot-of-self-p to-op)))
                  (emit-6502-alu-with-memory-rhs out "adc" to-op class-id))
                 (t
                  (format out "~&~10Tadc ~a" (emit-6502-value to-op)))))
           (emit-6502-store-byte-n out giving class-id 0 1))
         (cond
           ((slot-of-self-p to-op)
            (let ((n (slot-of-expr to-op)))
              (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol (second n) class-id)))
              (format out "~&~10Tadc (Self), y")
              (format out "~&~10Tsta (Self), y")
              (%6502-note-accumulator-holds-value-of to-op)))
           ((and (slot-of-expr to-op) (not (slot-of-self-p to-op)))
            (emit-6502-alu-with-memory-rhs out "adc" to-op class-id)
            (emit-6502-store-byte-n out to-op class-id 0 1)
            (%6502-note-accumulator-holds-value-of to-op))
           (t
            (format out "~&~10Tadc ~a" (emit-6502-value to-op))
            (format out "~&~10Tsta ~a" (emit-6502-value to-op))
            (when (= (operand-width to-op) 1)
              (%6502-note-accumulator-holds-value-of to-op)))))
     (when bcd-p (format out "~&~10Tcld"))))))

(defun compile-6502-subtract (out stmt class-id)
  ;; LET* so RESULT and W use FROM-TARGET / GIVING from this stmt (same parallel-binding issue as ADD).
  (let* ((from (safe-getf (rest stmt) :from))
         (from-target (safe-getf (rest stmt) :from-target))
         (giving (safe-getf (rest stmt) :giving))
         (result (or giving from-target))
         (w (max (operand-width result)
                 (expression-operand-width from)
                 (operand-width from-target)))
         (bcd-p (when result (usage-bcd-p (expr-to-width-name result)))))
    (cond
    ;; Optimise: SUBTRACT 1 FROM variable (no GIVING, plain variable) → dec (byte only)
    ((and (literal-one-p from) (not giving) (stringp from-target) (= w 1))
     (format out "~&~10Tdec ~a" (bare-data-assembly-symbol from-target class-id))
     (%invalidate-6502-accumulator-a))
    ;; Multi-byte SUBTRACT (w >= 2): result = from-target - from
    ((>= w 2)
     (when (and bcd-p (> w 2))
       (error "EIGHTBOL: BCD SUBTRACT with width ~d not yet implemented" w))
     (if (> w 2)
         (progn
           (when bcd-p (format out "~&~10Tsed"))
           (format out "~&~10Tsec")
           (dotimes (i w)
             (emit-6502-load-byte-n out from-target class-id i w)
             (if (expr-is-constant-p from)
                 (format out "~&~10Tsbc ~a" (emit-6502-immediate (constant-byte-value from i)))
                 (emit-6502-sbc-byte-n-of-expr out from class-id i w))
             (emit-6502-store-byte-n out result class-id i w
               :skip-ldy (and (equal from-target result)
                              (%6502-load-byte-n-sets-y-for-slot-store from-target class-id))))
           (when bcd-p (format out "~&~10Tcld")))
         (cond
           ((%6502-subtract-2byte-inplace-eligible-p from from-target result giving class-id w bcd-p)
            (emit-6502-subtract-2byte-self-inplace out from result class-id bcd-p))
           (t
            (let ((use-stack (expr-contains-subscript-p from-target)))
              (when (or use-stack (expr-contains-subscript-p from) (expr-contains-subscript-p result))
                (setf use-stack t))
              (when bcd-p (format out "~&~10Tsed"))
              ;; Low bytes: save result_lo (tax or pha)
              (emit-6502-load-expr out from-target class-id)
              (format out "~&~10Tsec")
              (if (expr-is-constant-p from)
                  (let ((v (expr-constant-value from)))
                    (format out "~&~10Tsbc #<~d" v))
                  (cond
                    ((slot-of-self-p from)
                     (let ((off (slot-symbol (second (slot-of-expr from)) class-id)))
                       (format out "~&~10Tldy #~a" off)
                       (format out "~&~10Tsbc (Self), y")))
                    ((and (slot-of-expr from) (not (slot-of-self-p from)))
                     (emit-6502-alu-with-memory-rhs out "sbc" from class-id))
                    (t
                     (format out "~&~10Tsbc ~a" (emit-6502-value from)))))
              (if use-stack
                  (format out "~&~10Tpha")
                  (format out "~&~10Ttax"))
              ;; High bytes: result_hi = from-target_hi - from_hi - borrow
              (emit-6502-load-hi-byte out from-target class-id)
              (if (expr-is-constant-p from)
                  (let ((v (expr-constant-value from)))
                    (format out "~&~10Tsbc #>~d" v))
                  (cond
                    ((slot-of-self-p from)
                     (let ((off (slot-symbol (second (slot-of-expr from)) class-id)))
                       (format out "~&~10Tldy #~a" (format nil "(~a+1)" off))
                       (format out "~&~10Tsbc (Self), y")))
                    ((and (slot-of-expr from) (not (slot-of-self-p from)))
                     (let* ((sof (slot-of-expr from))
                            (off (slot-symbol (second sof) class-id))
                            (ptr (6502-object-pointer-label (third sof) class-id)))
                       (format out "~&~10Tldy #~a" (format nil "(~a+1)" off))
                       (format out "~&~10Tsbc (~a), y" ptr)))
                    (t
                     (format out "~&~10Tsbc ~a + 1" (emit-6502-value from)))))
              (if (slot-of-self-p result)
                  (let ((n (slot-of-expr result)))
                    (format out "~&~10Tldy #~a + 1" (slot-symbol (second n) class-id))
                    (format out "~&~10Tsta (Self), y")
                    (format out "~&~10Tdey")
                    (format out "~&~10T~a" (if use-stack "pla" "txa"))
                    (format out "~&~10Tsta (Self), y"))
                  (if (and (slot-of-expr result) (not (slot-of-self-p result)))
                      (let* ((n (slot-of-expr result))
                             (off (slot-symbol (second n) class-id))
                             (ptr (6502-object-pointer-label (third n) class-id)))
                        (format out "~&~10Tldy #~a + 1" off)
                        (format out "~&~10Tsta (~a), y" ptr)
                        (format out "~&~10Tdey")
                        (format out "~&~10T~a" (if use-stack "pla" "txa"))
                        (format out "~&~10Tsta (~a), y" ptr))
                      (let ((res-sym (emit-6502-value result)))
                        (format out "~&~10Tsta ~a + 1" res-sym)
                        (format out "~&~10T~a" (if use-stack "pla" "txa"))
                        (format out "~&~10Tsta ~a" res-sym))))
              (when bcd-p (format out "~&~10Tcld"))
              (%invalidate-6502-accumulator-a))))))
    ((slot-of-self-p from-target)
   (let* ((n (slot-of-expr from-target))
          (slot-name (second n)))
     (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol slot-name class-id)))
     (format out "~&~10Tlda (Self), y")
     (format out "~&~10Tsec")
     (if (expr-is-constant-p from)
         (format out "~&~10Tsbc ~a" (emit-6502-immediate-operand from))
         (format out "~&~10Tsbc ~a" (emit-6502-value from)))
     (format out "~&~10Tsta (Self), y")
     (%6502-note-accumulator-holds-value-of from-target)))
  (giving
   (emit-6502-load-expr out from-target class-id)
   (when bcd-p (format out "~&~10Tsed"))
   (format out "~&~10Tsec")
   (if (expr-is-constant-p from)
       (format out "~&~10Tsbc ~a" (emit-6502-immediate-operand from))
       (cond
         ((slot-of-self-p from)
          (let ((n (slot-of-expr from)))
            (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol (second n) class-id)))
            (format out "~&~10Tsbc (Self), y")))
         ((and (slot-of-expr from) (not (slot-of-self-p from)))
          (emit-6502-alu-with-memory-rhs out "sbc" from class-id))
         (t
          (format out "~&~10Tsbc ~a" (emit-6502-value from)))))
   (emit-6502-store-byte-n out giving class-id 0 1)
   (when bcd-p (format out "~&~10Tcld")))
  (t
   (emit-6502-load-expr out from-target class-id)
   (when bcd-p (format out "~&~10Tsed"))
   (format out "~&~10Tsec")
   (if (expr-is-constant-p from)
       (format out "~&~10Tsbc ~a" (emit-6502-immediate-operand from))
       (cond
         ((slot-of-self-p from)
          (let ((n (slot-of-expr from)))
            (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol (second n) class-id)))
            (format out "~&~10Tsbc (Self), y")))
         ((and (slot-of-expr from) (not (slot-of-self-p from)))
          (emit-6502-alu-with-memory-rhs out "sbc" from class-id))
         (t
          (format out "~&~10Tsbc ~a" (emit-6502-value from)))))
   (cond
     ((slot-of-self-p from-target)
      (let ((n (slot-of-expr from-target)))
        (format out "~&~10Tldy ~a" (emit-6502-immediate (slot-symbol (second n) class-id)))
        (format out "~&~10Tsta (Self), y")
        (%6502-note-accumulator-holds-value-of from-target)))
     ((and (slot-of-expr from-target) (not (slot-of-self-p from-target)))
      (emit-6502-store-byte-n out from-target class-id 0 1)
      (%6502-note-accumulator-holds-value-of from-target))
     (t
      (format out "~&~10Tsta ~a" (emit-6502-value from-target))
      (when (= (operand-width from-target) 1)
        (%6502-note-accumulator-holds-value-of from-target))))
   (when bcd-p (format out "~&~10Tcld"))))))

(defun compile-6502-compute (out stmt class-id)
  "COMPUTE target = expression. Supports arbitrary width 1–8 bytes.
For w=1, expr may be compound (add, subtract, etc.). For w>1, expr must be variable/constant."
  (let* ((target (safe-getf (rest stmt) :target))
         (expr (safe-getf (rest stmt) :expression))
         (w (max (operand-width target) (expression-operand-width expr))))
    (cond
      ((= w 1)
       (emit-6502-load-expr out expr class-id)
       (emit-6502-store-byte-n out target class-id 0 1))
      (t
       (dotimes (i w)
         (emit-6502-load-byte-n out expr class-id i w)
         (emit-6502-store-byte-n out target class-id i w
           :skip-ldy (and (equal expr target)
                          (%6502-load-byte-n-sets-y-for-slot-store expr class-id))))))))

;;; SET statement

(defun compile-6502-set (out stmt class-id)
  "SET target TO value. Supports TO expr, TO NULL, UP BY, DOWN BY, TO ADDRESS OF, TO SELF."
  (let* ((target (safe-getf (rest stmt) :target))
         (value (safe-getf (rest stmt) :value))
         (up-by (safe-getf (rest stmt) :up-by))
         (down-by (safe-getf (rest stmt) :down-by))
         (by-expr (safe-getf (rest stmt) :by))
         (address-of (safe-getf (rest stmt) :address-of))
         (to-self (safe-getf (rest stmt) :to-self))
         (w (operand-width (or target to-self up-by down-by))))
    (cond
      (up-by
       ;; SET id UP BY expr => ADD expr TO id
       (compile-6502-add out (list :add :from by-expr :to up-by :giving nil) class-id))
      (down-by
       ;; SET id DOWN BY expr => SUBTRACT expr FROM id
       (compile-6502-subtract out (list :subtract :from by-expr :from-target down-by :giving nil) class-id))
      ((and address-of target)
       ;; SET target TO ADDRESS OF source: store address of source into target (2-byte pointer).
       (let ((source-id address-of))
         (if (slot-of-self-p source-id)
             (let ((offset (slot-symbol (second (slot-of-expr source-id)) class-id)))
               (format out "~&~10Tlda #<Self")
               (format out "~&~10Tclc")
               (format out "~&~10Tadc #~a" offset)
               (emit-6502-store-byte-n out target class-id 0 2)
               (format out "~&~10Tlda #>Self")
               (format out "~&~10Tadc #0")
               (emit-6502-store-byte-n out target class-id 1 2))
             (progn
               (format out "~&~10Tlda #<~a" (emit-6502-value source-id))
               (emit-6502-store-byte-n out target class-id 0 2)
               (format out "~&~10Tlda #>~a" (emit-6502-value source-id))
               (emit-6502-store-byte-n out target class-id 1 2)))))
      (to-self
       ;; SET id TO SELF: store Self pointer (low, high) into id.
       (format out "~&~10Tlda #<Self")
       (emit-6502-store-byte-n out to-self class-id 0 2)
       (format out "~&~10Tlda #>Self")
       (emit-6502-store-byte-n out to-self class-id 1 2))
      ((member value '(:null null "NULL") :test #'equal)
       ;; 6502: NULL = high byte zero. Set pointer to NULL by zeroing high byte only.
       (format out "~&~10Tlda ~a" (emit-6502-immediate 0))
       (emit-6502-store-byte-n out target class-id (1- w) w))
      (t
       (let ((val-w (expression-operand-width value)))
         (dotimes (i (min val-w w))
           (emit-6502-load-byte-n out value class-id i val-w)
           (emit-6502-store-byte-n out target class-id i w))
         (when (< val-w w)
           (format out "~&~10Tlda ~a" (emit-6502-immediate 0))
             (dotimes (i (- w val-w))
             (emit-6502-store-byte-n out target class-id (+ val-w i) w))))))))

;;; DIVIDE statement — divisor must be constant power-of-two; emit LSR.
(defun compile-6502-divide (out stmt class-id)
  (let* ((divisor (safe-getf (rest stmt) :divisor))
         (into-id (safe-getf (rest stmt) :into))
         (giving (safe-getf (rest stmt) :giving))
         (by-dividend (safe-getf (rest stmt) :by))
         (source (if by-dividend by-dividend into-id))
         (dest (or giving into-id)))
    (unless (and (expr-is-constant-p divisor)
                 (power-of-two-p (expr-constant-value divisor)))
      (error 'backend-error
             :message "DIVIDE: divisor must be constant power-of-two (1, 2, 4, 8, ...)"
             :cpu :6502 :detail stmt))
    (let ((shift (log2 (expr-constant-value divisor)))
          (w (operand-width dest)))
      (when (zerop shift)
        (return-from compile-6502-divide))
      (if (= w 1)
          (progn
            (emit-6502-load-expr out source class-id)
            (dotimes (_ shift) (format out "~&~10Tlsr a"))
            (emit-6502-store-byte-n out dest class-id 0 1))
          (do ((i 0 (1+ i))
               (op source (if (= i 0) source dest)))
              ((>= i shift))
            (emit-6502-load-byte-n out op class-id 0 w)
            (format out "~&~10Tlsr a")
            (emit-6502-store-byte-n out dest class-id 0 w)
            (emit-6502-load-byte-n out op class-id 1 w)
            (format out "~&~10Tror a")
            (emit-6502-store-byte-n out dest class-id 1 w))))))

;;; MULTIPLY statement — multiplier must be constant power-of-two; emit ASL.
(defun compile-6502-multiply (out stmt class-id)
  (let* ((multiplier (safe-getf (rest stmt) :multiplier))
         (by-id (safe-getf (rest stmt) :by))
         (giving (safe-getf (rest stmt) :giving))
         (source (or giving by-id))
         (dest (or giving by-id)))
    (unless (and (expr-is-constant-p multiplier)
                 (power-of-two-p (expr-constant-value multiplier)))
      (error 'backend-error
             :message "MULTIPLY: multiplier must be constant power-of-two (1, 2, 4, 8, ...)"
             :cpu :6502 :detail stmt))
    (let ((shift (log2 (expr-constant-value multiplier)))
          (w (operand-width dest)))
      (when (zerop shift)
        (return-from compile-6502-multiply))
      (if (= w 1)
          (progn
            (emit-6502-load-expr out source class-id)
            (dotimes (_ shift) (format out "~&~10Tasl a"))
            (emit-6502-store-byte-n out dest class-id 0 1))
          (do ((i 0 (1+ i))
               (op source (if (= i 0) source dest)))
              ((>= i shift))
            (emit-6502-load-byte-n out op class-id 0 w)
            (format out "~&~10Tasl a")
            (emit-6502-store-byte-n out dest class-id 0 w)
            (emit-6502-load-byte-n out op class-id 1 w)
            (format out "~&~10Trol a")
            (emit-6502-store-byte-n out dest class-id 1 w))))))

;;; PERFORM statement

(defun compile-6502-perform (out stmt class-id)
  "Emit PERFORM: jsr to paragraph label (same method). Uses para-label so target matches paragraph."
  (let ((proc (safe-getf (rest stmt) :procedure))
        (times (safe-getf (rest stmt) :times))
        (until (safe-getf (rest stmt) :until))
        (target (para-label (format nil "~a" proc) class-id (or *method-id* ""))))
    (cond
      (times
       (let ((lbl-loop (new-6502-label "PerfLoop"))
             (lbl-end (new-6502-label "PerfEnd")))
         (emit-6502-load-expr out times class-id)
         (format out "~&~10Ttax")
         (format out "~&~a:" lbl-loop)
         (format out "~&~10Tjsr ~a" target)
         (%invalidate-6502-accumulator-a)
         (format out "~&~10Tdex")
         (format out "~&~10Tbne ~a" lbl-loop)
         (format out "~&~a:" lbl-end)))
      (until
          (let ((lbl-loop (new-6502-label "PerfLoop"))
                (lbl-end (new-6502-label "PerfEnd")))
            (format out "~&~a:" lbl-loop)
            (emit-6502-condition out until class-id lbl-end)
            (format out "~&~10Tjsr ~a" target)
            (%invalidate-6502-accumulator-a)
            (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) lbl-loop)
            (format out "~&~a:" lbl-end)))
      (t
       (progn
         (format out "~&~10Tjsr ~a" target)
         (%invalidate-6502-accumulator-a))))))

;;; compile-statement methods — one per (cpu, ast-node-type)
;;; Brief methods delegating to compile-6502-* helpers.

(defmacro def-6502-statement (stmt-type (ast-node-data) &body body)
  "Define compile-statement for :6502 and :65c02/:65c816/:huc6280 (same impl). Uses *output-stream*."
  `(progn
     (defmethod compile-statement ((cpu (eql :6502)) (stmt-type (eql ,stmt-type)) ,ast-node-data)
       ,@body)
     (defmethod compile-statement ((cpu (eql :65c02)) (stmt-type (eql ,stmt-type)) ,ast-node-data)
       ,@body)
     (defmethod compile-statement ((cpu (eql :65c816)) (stmt-type (eql ,stmt-type)) ,ast-node-data)
       ,@body)
     (defmethod compile-statement ((cpu (eql :huc6280)) (stmt-type (eql ,stmt-type)) ,ast-node-data)
       ,@body)))

(defun stmt (sym data) (cons sym data))

(def-6502-statement :goback (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(def-6502-statement :exit-method (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(def-6502-statement :exit-program (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(def-6502-statement :stop-run (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(def-6502-statement :exit (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(def-6502-statement :move (ast-node-data)
  (compile-6502-move *output-stream* (stmt :move ast-node-data) *class-id*))

(def-6502-statement :invoke (ast-node-data)
  (compile-6502-invoke *output-stream* (stmt :invoke ast-node-data) *class-id*))

(def-6502-statement :call (ast-node-data)
  (compile-6502-call *output-stream* (stmt :call ast-node-data)))

(def-6502-statement :if (ast-node-data)
  (compile-6502-if *output-stream* (stmt :if ast-node-data) cpu))

(def-6502-statement :add (ast-node-data)
  (compile-6502-add *output-stream* (stmt :add ast-node-data) *class-id*))

(def-6502-statement :subtract (ast-node-data)
  (compile-6502-subtract *output-stream* (stmt :subtract ast-node-data) *class-id*))

(def-6502-statement :compute (ast-node-data)
  (compile-6502-compute *output-stream* (stmt :compute ast-node-data) *class-id*))

(def-6502-statement :divide (ast-node-data)
  (compile-6502-divide *output-stream* (stmt :divide ast-node-data) *class-id*))

(def-6502-statement :multiply (ast-node-data)
  (compile-6502-multiply *output-stream* (stmt :multiply ast-node-data) *class-id*))

(def-6502-statement :set (ast-node-data)
  (compile-6502-set *output-stream* (stmt :set ast-node-data) *class-id*))

(def-6502-statement :log-fault (ast-node-data)
  (let ((code (safe-getf ast-node-data :code)))
    (format *output-stream* "~&~10T.LogFault ~a"
            (if (stringp code)
                (format nil "\"~a\"" code)
                (emit-6502-value code)))))

(def-6502-statement :debug-break (ast-node-data)
  (let ((code (safe-getf ast-node-data :code)))
    (format *output-stream* "~&~10T.DebugBreak ~a"
            (if (stringp code)
                (format nil "\"~a\"" code)
                (emit-6502-value code)))))

(def-6502-statement :perform (ast-node-data)
  (compile-6502-perform *output-stream* (stmt :perform ast-node-data) *class-id*))

(def-6502-statement :string-blt (ast-node-data)
  (compile-6502-string-blt *output-stream* (stmt :string-blt ast-node-data) *class-id*))

(def-6502-statement :goto (ast-node-data)
  (compile-6502-goto *output-stream* (stmt :goto ast-node-data) *class-id* *method-id*))

(def-6502-statement :paragraph (ast-node-data)
  (compile-6502-paragraph *output-stream* (stmt :paragraph ast-node-data) *class-id* *method-id*))

(def-6502-statement :evaluate (ast-node-data)
  (compile-6502-evaluate *output-stream* (stmt :evaluate ast-node-data) cpu))

(def-6502-statement :inspect (ast-node-data)
  (compile-6502-inspect *output-stream* (stmt :inspect ast-node-data) *class-id*))

(def-6502-statement :copy (ast-node-data)
  (error "EIGHTBOL: COPY ~s should have been expanded at lex time"
         (safe-getf ast-node-data :name)))

(def-6502-statement :service-bank (ast-node-data)
  (declare (ignore ast-node-data))
  (error "EIGHTBOL: :service-bank is copybook metadata, not a procedure statement (corrupt AST)"))
