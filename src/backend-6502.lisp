;; src-6502.lisp -- 6502 / 64tass code generation backend
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
;; Key conventions: Method labels: MethodCharacterThink: .block ...

;; {Class}Class INVOKE Var "M": .CallMethod Call{Type}M, {Type}Class,
;; Var Slot access: ldy #{OriginClass}{Slot} / lda (Self), y Array
;; access: ldx index / lda base, x (X for subscript; Y for slot)
;; Constants (78/77): lda #ConstName (immediate addressing) Variables:
;; lda VarName (direct/absolute addressing) Fault: .LogFault "code"
;; (4-char string; assembler does minifont)
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
  (if (member *6502-family-cpu* '(:65c02 :65c816 :HuC6280))
      "bra"
      "jmp"))

(defun 6502-has-stz-p ()
  "True if CPU has STZ (65c02 and derivatives)."
  (member *6502-family-cpu* '(:65c02 :65c816 :huc6280)))

(defun 6502-use-undocumented-p ()
  "True if CPU allows undocumented 6502 opcodes (lax, dcp, etc.). Only :6502."
  (eq *6502-family-cpu* :6502))

(defvar *6502-accumulator-expression* nil
  "AST for the value currently in A during 6502-family emission, or NIL if unknown.

Rebound per method. @code{emit-6502-load-expression} skips @code{lda} when @code{equal}
to the requested expression; @code{%invalidate-6502-accumulator-a} clears the
cache after subroutine calls or A-clobbering paths. After @code{sta} to a
single-byte lvalue, @code{%6502-note-accumulator-holds-value-of} records that A
still holds that value (avoids reloading after @code{adc}/@code{sta} to the same slot).")

(defun %invalidate-6502-accumulator-a ()
  "Clear @code{*6502-accumulator-expression*} so A is treated as unknown."
  (setf *6502-accumulator-expression* nil))

(defun %6502-note-accumulator-holds-value-of (expression)
  "Record that A holds the value of EXPRESSION after a store or op that leaves it there.

EXPRESSION must match what a later @code{emit-6502-load-expression} will receive (@code{equal}).
Used to omit redundant @code{lda} after @code{sta} to the same location."
  (setf *6502-accumulator-expression* expression))

(defun %6502-subtract-2byte-inplace-eligible-p (from from-target result
                                                giving class-id w bcd-p)
  "True  when  SUBTRACT can  use  one  @code{ldy}  to  the low  slot  byte,
@code{sta}/@code{iny}/@code{lda}/@code{sbc}/@code{sta} for high byte (no
@code{tax}/@code{dey} dance)."
  (declare (ignore class-id))
  (and (= w 2)
       (not bcd-p)
       (null giving)
       (equal from-target result)
       (slot-of-self-p result)
       (not (expression-contains-subscript-p from-target))
       (not (expression-contains-subscript-p from))
       (not (expression-contains-subscript-p result))))

(defun emit-6502-subtract-2byte-self-inplace (out from result class-id bcd-p)
  "Emit 16-bit SUBTRACT @code{FROM} from @code{RESULT} (same Self slot), little-endian, in place.

Sequence: @code{ldy} low offset, @code{sec}, @code{lda (Self),y}, @code{sbc} low, @code{sta (Self),y},
@code{iny}, @code{lda (Self),y}, @code{sbc} high, @code{sta (Self),y}."
  (declare (ignore class-id))
  (when bcd-p
    (error 'backend-error
           :message "EIGHTBOL/6502: BCD 2-byte in-place SUBTRACT not implemented"
           :cpu :6502
           :detail (list :subtract-inplace from result)))
  (let ((offset (apply #'slot-symbol (rest (slot-of-expression result)))))
    (format out "~&~10Tldy # ~a" offset)
    (format out "~&~10Tsec")
    (format out "~&~10Tlda (~a), y" (pascal-case (third (slot-of-expression result))))
    (if (expression-constant-p from)
        (format out "~&~10Tsbc # <~a" (expression-constant-value from))
        (format out "~&~10Tsbc ~a" (emit-6502-value from)))
    (format out "~&~10Tsta (~a), y" (pascal-case (third (slot-of-expression result))))
    (format out "~&~10Tiny")
    (format out "~&~10Tlda (~a), y" (pascal-case (third (slot-of-expression result))))
    (if (expression-constant-p from)
        (format out "~&~10Tsbc # >~d" (expression-constant-value from))
        (format out "~&~10Tsbc ~a + 1" (emit-6502-value from)))
    (format out "~&~10Tsta (~a), y" (pascal-case (third (slot-of-expression result))))
    (%invalidate-6502-accumulator-a)))

(defun emit-6502-store-zero (out addr)
  "Emit code to store zero to memory at ADDR. Uses stz for 65c02+, lda # 0 + sta for 6502/RP2A03."
  (if (6502-has-stz-p)
      (progn
        (format out "~&~10Tstz ~a" addr))
      (progn
        (format out "~&~10Tldy # 0")
        (format out "~&~10Tsty ~a" addr))))

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
  "True   if   METHOD   has   exactly    one   statement   that   is   only
a  return  (@code{:goback},  @code{:exit-method},  @code{:exit-program},
@code{:exit}, @code{:stop-run}).

@itemize @bullet
@item
METHOD — a @code{:method} AST node.
@end itemize"
  (let ((statements (method-statements-list method)))
    (and (= (length statements) 1)
         (let ((s (first statements)))
           (and (listp s)
                (not (null (member (first s)
                                   '(:goback :exit-method :exit-program :exit :stop-run)))))))))

(defun method-true-method-alias-p (method)
  "True if METHOD should be emitted as @code{MethodClassM = TrueMethod} (no @code{.block}).

Blank   methods  and   single-statement   GOBACK/EXIT*/STOP  RUN   match
Phantasia's hand-written stubs.

@itemize @bullet
@item
METHOD — a @code{:method} AST node.
@end itemize"
  (or (method-blank-p method)
      (method-trivial-return-only-p method)))

(defun method-last-statement-6502-no-trailing-rts-p (last-statement)
  "True     if    LAST-STATEMENT     already     ends     control    flow     so
@code{compile-6502-method} must not emit a trailing @code{rts}.

Covers  returns   and  tail  @code{CALL}  that   emits  @code{jmp}  when
@code{:tail-call-p} is  set (e.g.  @code{CALL …  GOBACK}). @code{INVOKE}
uses @code{.CallMethod} (@code{jsr DoCallMethod}),  so the method always
needs  a trailing  @code{rts}. Far  @code{CALL} never  uses a  bare tail
@code{jmp}.

@itemize @bullet
@item
LAST-STATEMENT — last statement in a method’s @code{:statements} list, or NIL.
@end itemize"
  (when (and last-statement (listp last-statement))
    (case (first last-statement)
      ((:goback :exit-method :exit-program :exit :stop-run) t)
      (:call
       (let ((tail (safe-getf (rest last-statement) :tail-call-p))
             (service (safe-getf (rest last-statement) :service))
             (bank (safe-getf (rest last-statement) :bank)))
         (and tail (not service) (not bank))))
      (t nil))))

(defun emit-one-6502-family-method (output-stream class-id method cpu compile-method-fn)
  "Emit one method: @code{MethodClassM = TrueMethod}, or a @code{.block} body from COMPILE-METHOD-FN.

COMPILE-METHOD-FN  must accept  @code{(METHOD  CLASS-ID  CPU)} and  emit
a    @code{.block}    body     (e.g.    @code{compile-6502-method}    or
@code{compile-rp2a03-method}). CPU  selects opcodes for the  shared 6502
statement helpers.

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
             (pascal-case class-id) (pascal-case (safe-getf (rest method) :method-id))))
    (t (funcall compile-method-fn method class-id cpu))))

(defun compile-6502-family (ast output-stream cpu)
  "Shared code generation for 6502-family CPUs (6502, 65c02, 65c816, HuC6280).

CPU    is    the    keyword    (:6502    :65c02    :65c816    :huc6280).
Binds  *6502-family-cpu*  for  opcode selection.  Trivial  methods  emit
@code{MethodClassM    =    TrueMethod};    @code{INVOKE    Self}    uses
@code{.CallMethod} in the method body."
  (let ((*6502-family-cpu* cpu)
        (cpu-label (cpu-display-name cpu))
        (*output-stream* output-stream))
    (unless (and (listp ast) (eq (first ast) :program))
      (error "EIGHTBOL/~a: expected :program AST node, got ~s" cpu-label (first ast)))
    (let ((*class-id* (safe-getf (rest ast) :class-id))
          (methods (safe-getf (rest ast) :methods)))
      (format output-stream
              ";;; ~a — generated by EIGHTBOL for ~a~%"
              (or *class-id* "Globals") cpu-label)
      (finish-output output-stream)
      (if (and *class-id* methods)
          (dolist (method (ensure-list methods))
            (when (and (listp method) (eq (first method) :method))
              (format output-stream "~2%;;; Method ~a # ~a" *class-id* (getf (rest method) :method-id))
              (handler-case
                  (emit-one-6502-family-method output-stream *class-id* method cpu #'compile-6502-method)
                #+ () (error (e)
                        (format output-stream "~%~10T.LogFault \"CMPE\"~%~10Trts")
                        (format output-stream "~{~2%;;; ~a~}" (split-sequence #\newline (princ-to-string e)))
                        (warn "Error in ~a # ~a:~%~a" *class-id*
                              (getf (rest method) :method-id) (princ-to-string e))))
              (format output-stream "~2%;;; End of method ~a # ~a" *class-id* (getf (rest method) :method-id))
              (finish-output output-stream))))
      (format output-stream ";;; end of ~a" (or *class-id* "Globals"))
      (finish-output output-stream))))

(defmethod compile-to-assembly (ast (cpu (eql :6502)) output-stream)
  (compile-6502-family ast output-stream :6502))

;;; Method compilation

(defun compile-6502-method (method class-id cpu)
  "Emit one METHOD for CLASS-ID using CPU variant keyword.

Binds  @code{*6502-family-cpu*}   to  CPU  so  opcode   selection  (e.g.
@code{lax} vs @code{lda}/@code{tax}) matches standalone compilation, not
only @code{compile-6502-family}.

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
         (method-dispatch-suffix (pascal-case *method-id*))
         (statements (method-statements-list method))
         (last-statement (car (last statements))))
    (format *output-stream* "~&Method~a~a: .block"
	  (pascal-case class-id)
	  method-dispatch-suffix)
    (let ((*6502-accumulator-expression* nil))
      (dolist (statement statements)
        (cond
          ((null statement))
          ((not (listp statement))
           (error "EIGHTBOL: malformed procedure statement (expected list): ~s" statement))
          ((first statement)
           (compile-statement cpu (first statement) (rest statement))))))
    ;; Fall-through only: returns and tail jmp/jsr paths that never reach here.
    (unless (method-last-statement-6502-no-trailing-rts-p last-statement)
      (format *output-stream* "~&~10Trts"))
    (format *output-stream* "~&~10T.bend")))

;;; Statement dispatch via generic functions (compile-statement-* methods below)

;;; Parser format  normalization Parser  produces ("HP" "OF"  "Self") or
;;; ("HP" OF "Self"); backend expects (:of slot obj).

(defun slot-of-self-p (expression)
  "True if EXPRESSION is slot OF Self in either (:of slot :self) or (slot OF Self) format."
  (and (listp expression)
       (eql (first expression) :of)
       (string-equal (third expression) "Self")))

(defun slot-of-expression (expression)
  "Return (:of slot obj) form for EXPRESSION, or NIL."
  (when (and (listp expression) (eq :of (first expression)))
    expression))

(defun slot-on-expression (expression)
  "Return (:on slot obj) form for EXPRESSION, or NIL."
  (when (and (listp expression) (eq :on (first expression)))
    expression))

(defun %move-from-resolve-constant-of-self (from)
  (if (slot-of-self-p from)
      (if (and (listp (second (slot-of-expression from)))
               (eql :subscript (first (second (slot-of-expression from)))))
          (second (slot-of-expression from))
          ;; else: no subscript
          from)
      from))

(defun %6502-move-source-byte-width (from)
  "Byte width of MOVE source: @code{max} of @code{expression-operand-width}
and @code{operand-width}.

Ensures PIC 99 sources (e.g.  @code{Character-Max-HP OF Self}) use width
2 when the copybook row is two bytes."
  (expression-operand-width from))

(defun 6502-object-pointer-label (obj-expression class-id)
  "Return 64tass label for object pointer OBJ-EXPRESSION (ZP pair), e.g. Current-Actor → CurrentActor.

@table @asis
@item OBJ-EXPRESSION
@code{:self} / @code{Self} / string data name.
@item CLASS-ID
Ignored (reserved for typed object refs).
@end table

@subsection Outputs
String usable as @code{lda (Label), y} base."
  (declare (ignore class-id))
  (cond
    ((stringp obj-expression)
     (cobol-global-data-name-to-assembly-symbol obj-expression))
    (t
     (error 'backend-error
            :message "6502: unsupported object expression in slot OF (need Self or data name)"
            :cpu :6502
            :detail obj-expression))))

(defun emit-6502-alu-with-memory-rhs (out mnemonic expression class-id)
  "Emit MNEMONIC (@code{adc}, @code{sbc}, @code{and}, @code{ora}, @code{eor}) with RHS EXPRESSION

A holds the other operand."
  (let ((expression (%move-from-resolve-constant-of-self expression)))
    (cond
      ((expression-constant-p expression)
       (format out "~&~10T~a # ~a" mnemonic (expression-constant-value expression)))
      ((slot-of-expression expression) 
       (let* ((slot-of-expression (slot-of-expression expression))
              (offset (apply #'slot-symbol (rest slot-of-expression)))
              (pointer (6502-object-pointer-label (third slot-of-expression) class-id)))
         (format out "~&~10Tldy # ~a" offset)
         (format out "~&~10T~a (~a), y" mnemonic pointer)))
      ((slot-on-expression expression) 
       (let* ((slot-on-expression (slot-on-expression expression))
              (offset (apply #'slot-symbol (rest slot-on-expression)))
              (pointer (6502-object-pointer-label (third slot-on-expression)
                                                  class-id)))
         (format out "~&~10T~a ~a + ~a, y" mnemonic pointer offset)))
      (t (format out "~&~10T~a ~a" mnemonic (emit-6502-value expression)))))
  (%invalidate-6502-accumulator-a))

(defun emit-6502-cmp-memory-rhs (out expression class-id)
  "Emit @code{cmp} for single-byte RHS EXPRESSION after A holds the left side.
Explicit   @code{Max-HP   OF  Self}   and   bare   instance  slots   use
@code{ldy}/@code{cmp (Self),y}, not @code{cmp (Max-HP)}."
  (let ((expression (%move-from-resolve-constant-of-self expression)))
    (emit-6502-alu-with-memory-rhs out "cmp" expression class-id)))

(defun %emit-6502-alu-byte-n-of-expression (out mnemonic expression class-id n w)
  "Emit  MNEMONIC (@code{cmp},  @code{adc},  or @code{sbc})  on  byte N  of
W-byte EXPRESSION. A  holds the accumulated other operand  (same addressing as
@code{emit-6502-load-byte-n} / @code{emit-6502-cmp-byte-n-of-expression})."
  (declare (type string mnemonic))
  (when (>= n w)
    (error "%emit-6502-alu-byte-n-of-expression: n ~d >= w ~d" n w))
  (let ((expression (%move-from-resolve-constant-of-self expression)))
    (cond
      ((and (stringp expression) (constant-p expression) (= w 1) (zerop n))
       (format out "~&~10T~a ~a" mnemonic (emit-6502-immediate-operand expression)))
      ((expression-constant-p expression)
       (format out "~&~10T~a # ~a" mnemonic (expression-constant-value expression)))
      ((and (listp expression) (eq (first expression) :subscript))
       (format out "~&~10Tpha")
       (emit-6502-load-expression-into-x out (third expression) class-id)
       (format out "~&~10Tpla")
       (format out "~&~10T~a ~a + ~d, x" mnemonic (second expression) n))
      ((slot-of-self-p expression)
       (let ((offset (apply #'slot-symbol (rest (slot-of-expression expression)))))
         (format out "~&~10Tldy # ~a~[~:;~:* + ~d~]" offset n)
         (format out "~&~10T~a (Self), y" mnemonic)))
      ((and (slot-of-expression expression) (not (slot-of-self-p expression)))
       (let* ((slot-of-expression (slot-of-expression expression))
              (offset (apply #'slot-symbol (rest slot-of-expression)))
              (pointer (6502-object-pointer-label (third slot-of-expression) class-id)))
         (format out "~&~10Tldy # ~a~[~:;~:* + ~d~]" offset n)
         (format out "~&~10T~a (~a), y" mnemonic pointer)))
      ((stringp expression)
       (format out "~&~10T~a ~a~[~:;~:* + ~d~]"
               mnemonic expression n))
      (t
       (format out "~&~10T~a ~a~[~:;~:* + ~d~]"
               mnemonic (emit-6502-value expression) n)))))

(defun emit-6502-cmp-byte-n-of-expression (out expression class-id n w)
  "Compare A to byte N (0-based) of W-byte RHS EXPRESSION (multi-byte @code{=} / @code{NOT =}).
Mirrors addressing in @code{emit-6502-load-byte-n}; @code{CMP} does not change A."
  (%emit-6502-alu-byte-n-of-expression out "cmp" expression class-id n w))

(defun emit-6502-adc-byte-n-of-expression (out expression class-id n w)
  "Add-with-carry byte N of W-byte RHS EXPRESSION into A (same addressing as @code{emit-6502-cmp-byte-n-of-expression})."
  (%emit-6502-alu-byte-n-of-expression out "adc" expression class-id n w))

(defun emit-6502-sbc-byte-n-of-expression (out expression class-id n w)
  "Subtract-with-borrow byte N of W-byte RHS EXPRESSION from A (same addressing as @code{emit-6502-cmp-byte-n-of-expression})."
  (%emit-6502-alu-byte-n-of-expression out "sbc" expression class-id n w))

(defun emit-6502-inc-dec-instance-or-bare (out opcode expression class-id)
  "Emit OPCODE (@code{inc} or @code{dec}) for 1-byte EXPRESSION (implicit slot or absolute label)."
  (declare (ignore class-id))
  (format out "~&~10T~a ~a" opcode expression)
  (%invalidate-6502-accumulator-a))

;;; Expression emission

(defun emit-6502-value (expression)
  "Return a 64tass expression string for EXPRESSION.
Constants use @code{cobol-constant-to-assembly-symbol}. Non-constant bare identifiers use
@code{bare-data-assembly-symbol}. Indexed @code{:subscript} bases use @code{bare-data-assembly-symbol}
so global CartRAM labels match @code{emit-6502-load-byte-n}. @code{slot OF obj} in address context
emits @code{(slotname)} (see branch below), not the full @code{OriginClassSlot} label.

@table @asis
@item EXPRESSION
Rvalue expression; @code{NIL} signals @code{backend-error} (avoids emitting a @code{NIL} label).
@end table"
  (when (null expression)
    (error 'backend-error
           :message "emit-6502-value: missing expression (NIL)"
           :cpu :6502
           :detail nil))
  (cond
    ((and (listp expression) (= 1 (length expression)))
     (emit-6502-value (first expression)))
    ((expression-constant-p expression)
     (expression-constant-value expression))
    ((eq expression :self) "Self")
    ((slot-of-expression expression)
     (error 'backend-error
            :message "emit-6502-value: slot OF … is not a plain address
use emit-6502-load-expression / load-byte-n"
            :cpu *cpu*
            :detail expression))
    ((eql :null expression)
     (error 'backend-error
            :message "should not try to assign NULL in this way"
            :cpu *cpu*
            :detail expression))
    ((and (listp expression) (eq (first expression) :subscript))
     ;; (:subscript  base-name  index)  -- indexed  table  access,  e.g.
     ;; DecalYH, x  Must match emit-6502-load-byte-n /  cmp: globals use
     ;; CartRAM labels, not class-prefixed slot-symbol.
     (error 'backend-error
            :message "emit-6502-value: array subscript is not a plain address
use emit-6502-load-expression / load-byte-n"
            :cpu *cpu*
            :detail expression))
    ((and (listp expression)
          (string= "(" (first expression))
          (string= ")" (lastcar expression)))
     (format nil "(~a)" (emit-6502-value (subseq expression 1 (1- (length expression))))))
    (t (error "Unclear how to make assembler understand: ~s" expression))))

(defun expression-literal-zero-p (expression)
  (when (expression-constant-p expression)
    (let ((val (expression-constant-value expression)))
      (and (numberp val) (zerop val)))))

(defun emit-6502-immediate (value)
  "Return assembly string for immediate operand: #value. Numbers use hex (#$xx)."
  (concatenate 'string (string #\#)
               (cond ((and (integerp value) (<= 0 value) (<= value 255))
                      (format nil "$~2,'0x" value))
                     ((integerp value)
                      (format nil "$~x" value))
                     (t
                      (princ-to-string value)))))

(defun expression-constant-p (expression)
  "True if EXPRESSION is a numeric literal, named constant, or constant expression.
Constant expressions are arithmetic/bit ops whose operands are all constant."
  (cond
    ((numberp expression) t)
    ((when-let (var (gethash expression *working-storage*))
       (getf var :value))
     t)
    ((and (listp expression) (eq (first expression) :literal)) t)
    ((and (listp expression)
          (string= "(" (first expression))
          (string= ")" (lastcar expression)))
     (every #'expression-constant-p (subseq expression 1 (1- (length expression)))))
    ((and (stringp expression) (constant-p expression)) t)
    ((and (listp expression) (eq (first expression) :add))
     (and (expression-constant-p (getf (rest expression) :from))
          (expression-constant-p (getf (rest expression) :to))))
    ((and (listp expression) (eq (first expression) :subtract))
     (and (expression-constant-p (getf (rest expression) :from))
          (expression-constant-p (getf (rest expression) :subtrahend))))
    ((and (listp expression) (eq (first expression) :multiply))
     (and (expression-constant-p (getf (rest expression) :multiplier))
          (expression-constant-p (getf (rest expression) :by))))
    ((and (listp expression) (eq (first expression) :divide))
     (and (expression-constant-p (getf (rest expression) :numerator))
          (expression-constant-p (getf (rest expression) :denominator))))
    ((and (listp expression) (eq (first expression) :bit-and))
     (and (expression-constant-p (second expression)) (expression-constant-p (third expression))))
    ((and (listp expression) (eq (first expression) :bit-or))
     (and (expression-constant-p (second expression)) (expression-constant-p (third expression))))
    ((and (listp expression) (eq (first expression) :low))
     (expression-constant-p (second expression)) )
    ((and (listp expression) (eq (first expression) :high))
     (expression-constant-p (second expression)))
    ((and (listp expression) (eq (first expression) :bit-xor))
     (and (expression-constant-p (second expression)) (expression-constant-p (third expression))))
    ((and (listp expression) (eq (first expression) :bit-not))
     (expression-constant-p (second expression)))
    ((and (listp expression) (eq (first expression) :shift-left))
     (and (expression-constant-p (second expression))
          (or (null (third expression))
              (expression-constant-p (third expression)))))
    ((and (listp expression) (eq (first expression) :shift-right))
     (and (expression-constant-p (second expression))
          (or (null (third expression))
              (expression-constant-p (third expression)))))
    (t nil)))

(defun expression-constant-value (expression)
  "Return numeric value of constant EXPRESSION. EXPRESSION must satisfy expression-constant-p.
Folds constant expressions (add/subtract/multiply/divide/bit/shift) at compile time."
  (cond
    ((numberp expression) expression)
    ((and (listp expression)
          (string= "(" (first expression))
          (string= ")" (lastcar expression)))
     (format nil "( ~{~a~^ ~} )"
             (mapcar #'expression-constant-value
                     (subseq expression 1 (1- (length expression))))))
    ((stringp expression) (pascal-case expression))
    ((and (listp expression) (eq (first expression) :low))
     (format nil "< ~a" (expression-constant-value (second expression))))
    ((and (listp expression) (eq (first expression) :high))
     (format nil "> ~a" (expression-constant-value (second expression))))
    ((and (listp expression) (or (not (find :giving (rest expression)))
                                 (null (getf (rest expression) :giving))))
     (ecase (first expression)
       (:literal (rest expression))
       (:add (format nil "(~a + ~a)"
                     (emit-6502-value (getf (rest expression) :from))
                     (emit-6502-value (getf (rest expression) :to))))
       (:subtract (format nil "(~a - ~a)"
                          (emit-6502-value (getf (rest expression) :from))
                          (emit-6502-value (getf (rest expression) :subtrahend))))
       (:multiply (format nil "(~a * ~a)"
                          (emit-6502-value (getf (rest expression) :multiplier))
                          (emit-6502-value (getf (rest expression) :by))))
       (:divide (format nil "(~a / ~a)"
                        (emit-6502-value (getf (rest expression) :numerator))
                        (emit-6502-value (getf (rest expression) :denominator))))
       (:bit-or (format nil "(~{~a~^ |~})"
                        (mapcar #'emit-6502-value (rest expression))))
       (:bit-and (format nil "(~{~a~^ &~})"
                         (mapcar #'emit-6502-value (rest expression))))
       (:bit-xor (format nil "(~{~a~^ ^~})"
                         (mapcar #'emit-6502-value (rest expression))))
       (:shift-left (format nil "(~a << ~a)"
                            (emit-6502-value (second expression))
                            (emit-6502-value (third expression))))
       (:shift-right (format nil "(~a >> ~a)"
                             (emit-6502-value (second expression))
                             (emit-6502-value (third expression))))))
    (t (error "Not a constant: ~s" expression))))

(defun power-of-two-p (n)
  "True if N is a positive power of two (1, 2, 4, 8, ...)."
  (and (integerp n) (> n 0) (zerop (logand n (1- n)))))

(defun log2 (n)
  "Return k such that 2^k = N, or NIL if N is not a power of two."
  (when (power-of-two-p n)
    ;; ROUND is actually unnecessary but it helps coerce the type.y
    (round (log n 2))))

;;; Load into X (subscript index)

(defun emit-6502-load-expression-into-x (out expression class-id)
  "Load EXPRESSION into X for subscript indexing.

When @code{*6502-family-cpu*}  is :6502 (undocumented  opcodes allowed),
object slots  use @code{lax (Self),  y} instead of @code{lda  (Self), y}
then      @code{tax}.      @var{OUT},     @var{CLASS-ID}      as      in
@code{emit-6502-load-expression}."
  (let ((expression (%move-from-resolve-constant-of-self expression)))
    (cond
      ((and (slot-of-expression expression) (6502-use-undocumented-p))
       (let ((so (slot-of-expression expression)))
         (format out "~&~10Tldy # ~a" (apply #'slot-symbol (rest so)))
         (format out "~&~10Tlax (Self), y"))
       (%invalidate-6502-accumulator-a))
      ((numberp expression)
       (format out "~&~10Tldx # $~2,'0x" expression))
      ((not (listp expression))
       (let ((var (gethash expression *working-storage*)))
         (if-let (value (getf var :value))
           (format out "~&~10Tldx # ~a" value)
           (format out "~&~10Tldx ~a" (pascal-case expression)))))
      (t
       (emit-6502-load-expression out expression class-id)
       (%invalidate-6502-accumulator-a)
       (format out "~&~10Ttax")))))

;;; Load into A register

(defun emit-6502-load-expression (out expression class-id)
  "Emit 6502 code to load EXPRESSION into the A register.
Constants  (numeric  literals  and  named  77/78  items)  use  immediate
mode (#). Constant expressions are folded at compile time and emitted as
one  immediate  load.  Variables  and  slots  use  direct/indexed  mode.
Compound bit/shift  expressions are  computed in-line.  Skips @code{lda}
when @code{*6502-accumulator-expression*}  is @code{equal} to EXPRESSION.  When EXPRESSION
is @code{(:of Name  Self)} and NAME is a  77/78 in @code{*const-table*},
treat as bare NAME (immediate @code{lda #}), not an instance slot."
  (cond
    ((and *6502-accumulator-expression*
          (equalp expression *6502-accumulator-expression*))
     (return-from emit-6502-load-expression))
    
    ((expression-constant-p expression)
     (format out "~&~10Tlda # ~a" (expression-constant-value expression))
     (setf *6502-accumulator-expression* expression)
     (return-from emit-6502-load-expression))
    
    ((and (listp expression) (eq (first expression) :bit-or))
     (emit-6502-load-expression out (second expression) class-id)
     (emit-6502-alu-with-memory-rhs out "ora" (third expression) class-id)
     (setf *6502-accumulator-expression* expression))
    
    ((and (listp expression) (eq (first expression) :bit-and))
     (emit-6502-load-expression out (second expression) class-id)
     (emit-6502-alu-with-memory-rhs out "and" (third expression) class-id)
     (setf *6502-accumulator-expression* expression))
    
    ((and (listp expression) (eq (first expression) :bit-xor))
     (emit-6502-load-expression out (second expression) class-id)
     (emit-6502-alu-with-memory-rhs out "xor" (third expression) class-id)
     (setf *6502-accumulator-expression* expression))
    
    ;; Arithmetic: a + b
    ((and (listp expression) (eq (first expression) :add))
     (emit-6502-load-expression out (getf (rest expression) :to) class-id)
     (format out "~&~10Tclc")
     (emit-6502-alu-with-memory-rhs out "adc" (getf (rest expression) :from) class-id)
     (setf *6502-accumulator-expression* expression))
    
    ;; Arithmetic: a - b
    ((and (listp expression) (eq (first expression) :subtract))
     (emit-6502-load-expression out (getf (rest expression) :from) class-id)
     (format out "~&~10Tsec")
     (emit-6502-alu-with-memory-rhs out "sbc" (getf (rest expression) :to) class-id)
     (setf *6502-accumulator-expression* expression))
    
    ;; Arithmetic: a * k (k must be power of 2)
    ((and (listp expression) (eq (first expression) :multiply))
     (let ((e1 (getf (rest expression) :multiplier)) (e2 (getf (rest expression) :by)))
       (cond
         ((and (expression-constant-p e2)
               (power-of-two-p (expression-constant-value e2)))
          (let ((shift (log2 (expression-constant-value e2))))
            (emit-6502-load-expression out e1 class-id)
            (dotimes (_ shift) (format out "~&~10Tasl a"))
            (setf *6502-accumulator-expression* expression)))
         ((and (expression-constant-p e1)
               (power-of-two-p (expression-constant-value e1)))
          (let ((shift (log2 (expression-constant-value e1))))
            (emit-6502-load-expression out e2 class-id)
            (dotimes (_ shift) (format out "~&~10Tasl a"))
            (setf *6502-accumulator-expression* expression)))
         (t (error 'backend-error
                   :message "6502: multiply by non-power-of-2 requires software routine"
                   :cpu :6502 :detail (list :multiply e1 e2))))))
    
    ;; Arithmetic: a / k (k must be power of 2)
    ((and (listp expression) (eq (first expression) :divide))
     (let ((e1 (getf (rest expression) :numerator)) (e2 (getf (rest expression) :denominator)))
       (when (expression-constant-p e2)
         (let ((shift (log2 (expression-constant-value e2))))
           (when (and shift (not (zerop shift)) (integerp shift))
             (emit-6502-load-expression out e1 class-id)
             (dotimes (_ shift) (format out "~&~10Tlsr a"))
             (setf *6502-accumulator-expression* expression)
             (return-from emit-6502-load-expression))))
       (error 'backend-error
              :message "6502: divide by non-power-of-2 requires software routine"
              :cpu :6502 :detail (list :divide e1 e2))))
    
    ;; Subscripted array: base(index) — load index into X, then lda base, x
    ((and (listp expression) (eq (first expression) :subscript))
     (emit-6502-load-expression-into-x out (third expression) class-id)
     (format out "~&~10Tlda ~a, x" (emit-6502-value (second expression)))
     (setf *6502-accumulator-expression* expression))
    
    ;; Slot OF Object -- indexed indirect via pointer
    ((slot-of-expression expression)
     #+ () (format *trace-output* "~&~10T ~s is slot-of expression" expression)
     (let ((so (slot-of-expression expression)))
       (format out "~&~10Tldy # ~a" (apply #'slot-symbol (rest so)))
       (format out "~&~10Tlda (~a), y" (third so)))
     (setf *6502-accumulator-expression* expression))
    
    ;; Shift left — asl A, n times
    ((and (listp expression) (eq (first expression) :shift-left))
     (let ((n (if (numberp (third expression)) (third expression) 1)))
       (emit-6502-load-expression out (second expression) class-id)
       (dotimes (_ n) (format out "~&~10Tasl a"))
       (setf *6502-accumulator-expression* expression)))
    
    ;; Shift right — lsr A, n times
    ((and (listp expression) (eq (first expression) :shift-right))
     (let ((n (if (numberp (third expression)) (third expression) 1)))
       (emit-6502-load-expression out (second expression) class-id)
       (dotimes (_ n) (format out "~&~10Tlsr a"))
       (setf *6502-accumulator-expression* expression)))
    
    ;; Bitwise AND (mask — used for bit testing too)
    ((and (listp expression) (eq (first expression) :bit-and))
     (emit-6502-load-expression out (second expression) class-id)
     (emit-6502-alu-with-memory-rhs out "and" (third expression) class-id)
     (setf *6502-accumulator-expression* expression))
    
    ;; Bitwise OR
    ((and (listp expression) (eq (first expression) :bit-or))
     (emit-6502-load-expression out (second expression) class-id)
     (emit-6502-alu-with-memory-rhs out "ora" (third expression) class-id)
     (setf *6502-accumulator-expression* expression))
    
    ;; Bitwise XOR
    ((and (listp expression) (eq (first expression) :bit-xor))
     (emit-6502-load-expression out (second expression) class-id)
     (emit-6502-alu-with-memory-rhs out "eor" (third expression) class-id)
     (setf *6502-accumulator-expression* expression))
    
    ;; Bitwise NOT (complement all bits)
    ((and (listp expression) (eq (first expression) :bit-not))
     (emit-6502-load-expression out (second expression) class-id)
     (format out "~&~10Teor #$ff")
     (setf *6502-accumulator-expression* expression))
    
    ;; Low value
    ((and (listp expression) (eq (first expression) :low))
     (format out "~&~10Tlda # <~a" (pascal-case (second expression)))
     (setf *6502-accumulator-expression* expression))
    
    ;; High value
    ((and (listp expression) (eq (first expression) :high))
     (format out "~&~10Tlda # >~a" (pascal-case (second expression)))
     (setf *6502-accumulator-expression* expression))
    
    ;; literal expression (constant for assembler to evaluate)
    ((and (listp expression) (eq (first expression) :literal))
     (format out "~&~10Tlda ~a" (second expression))
     (setf *6502-accumulator-expression* expression))
    
    ;; Numeric literal
    ((numberp expression)
     (format out "~&~10Tlda # $~2,'0x" expression)
     (setf *6502-accumulator-expression* expression))
    
    ;; Bare data name: constants immediate; instance slots via (Self),y; else absolute (bare-data)
    ((stringp expression)
     (break "?? ~s" expression)
     (if (expression-constant-p expression)
         (format out "~&~10Tlda # ~a" (expression-constant-value expression))
         
         (format out "~&~10Tlda ~a" expression))
     (setf *6502-accumulator-expression* expression))
    
    (t
     (format out "~&~10Tlda ~a" (emit-6502-value expression))
     (setf *6502-accumulator-expression* expression))))

(defun emit-6502-load-hi-byte (out pointer-expression class-id)
  "Load the high byte of a 2-byte value into A. Handles literals, constants, variables.
For :subscript, loads index into X then lda base + 1, x."
  (emit-6502-load-byte-n out pointer-expression class-id 1 2))

(defun constant-byte-value (expression n)
  "Extract byte N (0-based) from constant EXPRESSION. Little-endian.
Uses expression-constant-value when EXPRESSION is a constant expression."
  (when (expression-constant-p expression)
    (ldb (byte 8 (* n 8)) (expression-constant-value expression))))

(defun emit-6502-load-byte-n (out expression class-id n w)
  "Load byte N (0-based) of W-byte EXPRESSION into A. N must be < W.
Handles  literals,  constants,  variables,   slot  OF  Self,  subscript.
Named     77/78     constants     with      byte     width     1     use
@code{emit-6502-immediate-operand}   (symbolic   @code{#   Name}),   not
@code{#$nn}, matching @code{emit-6502-load-expression}."
  (when (>= n w)
    (error "emit-6502-load-byte-n: n ~d >= w ~d" n w))
  (cond
    ;; Single-byte copybook constant: MOVE and per-byte paths must use assembly equate name.
    ((and (stringp expression) (constant-p expression) (= w 1) (zerop n))
     (format out "~&~10Tlda # ~a" (expression-constant-value expression))
     (%6502-note-accumulator-holds-value-of expression))
    ((and (expression-constant-p expression) (zerop n))
     (format out "~&~10Tlda # ~a" (expression-constant-value expression))
     (%6502-note-accumulator-holds-value-of expression))
    ((expression-constant-p expression) 
     (format out "~&~10Tlda # ( ~a >> ~d ) & $ff"
             (expression-constant-value expression) (* 8 n))
     (%6502-note-accumulator-holds-value-of expression))
    ((and (listp expression) (eq (first expression) :subscript))
     (emit-6502-load-expression-into-x out (third expression) class-id)
     (format out "~&~10Tlda ~a~[~:;~:* + ~d~], x" (second expression) n))
    ((slot-of-expression expression)
     (let* ((slot-of-expression (slot-of-expression expression))
            (offset (apply #'slot-symbol (rest expression)))
            (pointer (pascal-case (third slot-of-expression))))
       (format out "~&~10Tldy # ~a~[~:;~:* + ~d~]" offset n)
       (format out "~&~10Tlda (~a), y" pointer)))
    ((and (stringp expression) (string-equal expression "Self"))
     (format out "~&~10Tlda Self~[~:;~:* + ~d~]" n))
    ((and (stringp expression) (expression-constant-p expression))
     (if (= n 0)
         (format out "~&~10Tlda # ( ~a & $ff )" (expression-constant-value expression))
         (format out "~&~10Tlda # ( ~a << ~d & $ff )"
                 (expression-constant-value expression)
                 (* 8 n))))
    ((stringp expression)
     (format out "~&~10Tlda ~a~[~:;~:* + ~d~]"
             (pascal-case expression) n))
    ((and (listp expression) (keywordp (first expression)))
     (ecase (first expression)

       (:bit-or
        (format out "~&~10Tlda ~a + ~d" (second expression) n)
        (format out "~&~10Tora ~a + ~d" (third expression) n))))
    (t
     (format out "~&~10Tlda ~a~[~:;~:* + ~d~]"
             (emit-6502-value expression) n))))

(defun emit-6502-store-byte-n (out dest class-id n w &key skip-ldy)
  "Store A to byte N (0-based) of W-byte DEST. Handles slot OF Self, variable.

When SKIP-LDY is true, emit only @code{sta (Self), y} / @code{sta (Pointer), y} — Y must already hold
the slot offset from an immediately preceding @code{emit-6502-load-byte-n} to the same lvalue byte
(multi-byte ADD/SUBTRACT/COMPUTE loops)."
  (when (>= n w)
    (error "emit-6502-store-byte-n: index byte # ~d ≥ variable width ~d byte~:p" n w))
  (cond
    ((and (listp dest) (eql :subscript (first dest)))
     (emit-6502-load-expression-into-x out (third dest) nil)
     (format out "~&~10Tsta ~a, x" (emit-6502-value (second dest))))
    ((slot-of-expression dest)
     (let* ((offset (apply #'slot-symbol (rest dest)))
            (pointer (6502-object-pointer-label (third dest) class-id)))
       (unless skip-ldy
         (format out "~&~10Tldy # ~a~[~:;~:* + ~d~]" offset n))
       (format out "~&~10Tsta (~a), y" pointer)))
    ((stringp dest)
     (format out "~&~10Tsta ~a~[~:;~:* + ~d~]"
             (pascal-case dest) n))
    (t
     (format out "~&~10Tsta ~a~[~:;~:* + ~d~]"
             (emit-6502-value dest) n)))
  ;; After sta, A still holds the byte written. For a single-byte destination, note it so the
  ;; next load of the same lvalue can skip lda (avoids ldy/lda (Self),y after adc/sta to same slot).
  (if (= n (1- w))
      (cond
        ((slot-of-self-p dest)
         (%6502-note-accumulator-holds-value-of dest))
        ((stringp dest)
         (%6502-note-accumulator-holds-value-of dest))
        (t (%invalidate-6502-accumulator-a)))
      (%invalidate-6502-accumulator-a)))

(defun 6502-dest-byte-address (dest class-id n w)
  "Return address string for byte N (0-based) of W-byte DEST, or nil if indirect (e.g. slot OF Self).
Used for STZ when MOVE ZERO to a direct-memory destination on 65c02+."
  (declare (ignore class-id))
  (cond
    ((>= n w) nil)
    ((slot-of-expression dest) nil)
    ((stringp dest) (format nil "~a~[~:;~:* + ~d~]") dest n)
    (t (format nil "~a~[~:;~:* + ~d~]" (emit-6502-value dest) n))))

(defun
    emit-6502-move-two-slots-16bit-lax (out from to-dest &rest _)
  "Copy 16 bits from source slot OF Self to dest slot OF Self.

Uses @code{lax (Self),y} for the low byte (A and X), @code{iny}/@code{lda (Self),y} for high,
stores high then @code{txa}/@code{sta (Self),y} for low — 6502 has no @code{stx (zp),y}.
NMOS 6502 only (@code{lax}); other CPUs use the generic byte loop."
  (declare (ignore _))
  (let ((src (apply #'slot-symbol (rest (slot-of-expression from))))
        (source-object (third from))
        (dest (apply #'slot-symbol (rest (slot-of-expression to-dest))))
        (dest-object (third from)))
    (format out "~&~10Tldy ~a" (emit-6502-immediate src))
    (ecase (first from)
      (:of (format out "~&~10Tlax (~a), y" source-object))
      (:on (format out "~&~10Tlax ~a, y" source-object)))
    (format out "~&~10Tiny")
    (ecase (first from)
      (:of (format out "~&~10Tlda (~a), y" source-object))
      (:on (format out "~&~10Tlda ~a, y" source-object)))
    (format out "~&~10Tldy ~a" (emit-6502-immediate (format nil "(~a+1)" dest)))
    (ecase (first from)
      (:of (format out "~&~10Tsta (~a), y" dest-object))
      (:on (format out "~&~10Tsta ~a, y" dest-object)))
    (format out "~&~10Tdey")
    ;; 6502 has no stx (zp),y — only sta (zp),y; low byte is in X from lax.
    (format out "~&~10Ttxa")
    (ecase (first from)
      (:of (format out "~&~10Tsta (~a), y" dest-object))
      (:on (format out "~&~10Tsta ~a, y" dest-object))))
  (%invalidate-6502-accumulator-a))

(defun emit-6502-move-n-bytes (out from to-dest from-w to-w class-id
                               &key sign-extend)
  "Copy  FROM  (from-w bytes)  to  TO-DEST  (to-w bytes).  Sign-extends  if
SIGN-EXTEND and  from-w < to-w.  When FROM is  literal zero and  CPU has
STZ (65c02+), uses stz for each byte when destination is direct."
  (flet ((emit-store-zero-byte (i)
           (let ((addr (6502-dest-byte-address to-dest class-id i to-w)))
             (if (and (6502-has-stz-p) addr)
                 (format out "~&~10Tstz ~a" addr)
                 (progn
                   (format out "~&~10Tlda # 0")
                   (%6502-note-accumulator-holds-value-of 0)
                   (emit-6502-store-byte-n out to-dest class-id i to-w))))))
    (cond

      ((and (expression-literal-zero-p from) (6502-has-stz-p))
       ;; MOVE ZERO: use stz for each byte when destination has direct address
       (dotimes (i (min from-w to-w))
         (emit-store-zero-byte i))
       (when (and (< from-w to-w) (>= to-w 2))
         (if sign-extend
             (let ((label-zero (new-6502-label "MvZero"))
                   (label-done (new-6502-label "MvDone")))
               (emit-6502-load-byte-n out from class-id (1- from-w) from-w)
               (format out "~&~10Tbpl ~a" label-zero)
               (format out "~&~10Tlda #$ff")
               (dotimes (i (- to-w from-w))
                 (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w))
               (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-done)
               (format out "~&~a:" label-zero)
               (dotimes (i (- to-w from-w))
                 (emit-store-zero-byte (+ from-w i)))
               (format out "~&~a:" label-done))
             (dotimes (i (- to-w from-w))
               (emit-store-zero-byte (+ from-w i))))))
      
      ((and (= from-w 2) (= to-w 2)
            (slot-of-self-p from) (slot-of-self-p to-dest)
            (6502-use-undocumented-p))
       (emit-6502-move-two-slots-16bit-lax out from to-dest class-id))
      
      ((and (= from-w 2) (= to-w 2)
            (stringp from) (stringp to-dest))
       (format out "~&~
~10Tlda ~a
~10Tsta ~a~0@*
~10Tlda ~a + 1
~10Tsta ~a + 1"
               (pascal-case from) (pascal-case to-dest)))
      
      (t
       (dotimes (i (min from-w to-w))
         (emit-6502-load-byte-n out from class-id i from-w)
         (emit-6502-store-byte-n out to-dest class-id i to-w))
       (when (and (< from-w to-w) (>= to-w 2))
         (let ((label-zero (new-6502-label "MvZero"))
               (label-done (new-6502-label "MvDone")))
           (if sign-extend
               (progn
                 (emit-6502-load-byte-n out from class-id (1- from-w) from-w)
                 (format out "~&~10Tbpl ~a" label-zero)
                 (format out "~&~10Tlda #$ff")
                 (dotimes (i (- to-w from-w))
                   (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w))
                 (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-done)
                 (format out "~&~a:" label-zero)
                 (format out "~&~10Tlda ~a" (emit-6502-immediate 0))
                 (dotimes (i (- to-w from-w))
                   (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w))
                 (format out "~&~a:" label-done))
               ;; no sign extension
               (dotimes (i (- to-w from-w))
                 (format out "~&~10Tlda # 0")
                 (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w)))))))))

;;; Operand width for byte/word arithmetic (PIC 99 vs PIC 9999)

(defun expression-contains-subscript-p (expression)
  "True if EXPRESSION or any subexpression uses :subscript (array fetch).
Array fetches use X or Y; when true, avoid using X for temp storage."
  (cond
    ((and (listp expression) (eq (first expression) :subscript)) t)
    ((and (listp expression) (member (first expression) '(:add :subtract :multiply :divide
                                                          :shift-left :shift-right :bit-and :bit-or :bit-xor :bit-not)))
     (or (expression-contains-subscript-p (second expression))
         (expression-contains-subscript-p (third expression))))
    ((and (listp expression) (eq (first expression) :of))
     (expression-contains-subscript-p (third expression)))
    (t nil)))

;;; MOVE statement
;;; operand-width uses *pic-width-table* (backend.lisp)

(defun compile-6502-move (out statement class-id)
  (let* ((from (%move-from-resolve-constant-of-self (safe-getf (rest statement) :from)))
         (to-dest (safe-getf (rest statement) :to))
         (to-w (expression-operand-width to-dest))
         (from-signed (expression-operand-signed-p from))
         (to-signed (expression-operand-signed-p to-dest))
         (sign-extend (and from-signed to-signed)))
    (flet ((load-index-into-x (index-expression)
             (format *trace-output* "~2% X ← ~s" index-expression)
             (emit-6502-load-expression-into-x out index-expression class-id)))
      (cond
        ;; MOVE NULL TO pointer — 6502: set pointer to NULL by zeroing high byte
        ((eql from :null)
         (format out "~&~10Tlda # 0")
         (emit-6502-store-byte-n out to-dest class-id (1- to-w) to-w))
        ;; Destination is subscripted: base(index) — load index into X, then store
        ((and (listp to-dest) (eq (first to-dest) :subscript))
         (emit-6502-load-expression out from class-id)
         (load-index-into-x (third to-dest))
         (format out "~&~10Tsta ~a, x" (emit-6502-value (second to-dest))))
        ;; Source  is   subscripted:  use  @code{emit-6502-move-n-bytes}
        ;; (same  as general  case).  A dedicated  @code{lda}/@code{sta}
        ;; path  used @code{emit-6502-value}  on the  destination, which
        ;; fails  for  @code{slot  OF}   non-Self  (e.g.  Waypoint-Y  OF
        ;; Current-Course): those need @code{emit-6502-store-byte-n} and
        ;; @code{sta (Pointer), y}. Destination is slot OF Self
        ((slot-of-expression to-dest)
         (let ((from-w (%6502-move-source-byte-width from)))
           (emit-6502-move-n-bytes out from to-dest from-w to-w class-id :sign-extend sign-extend)))
        ;; Source is slot OF Self, destination is a variable
        ((slot-of-expression from)
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
            (base-str (pascal-case base)))
       (if (and (integerp start) (= start 1))
           base-str
           (format nil "~a+~d" base-str (1- start)))))
    (t
     (emit-6502-value operand))))

(defun string-operand-length-expression (operand statement)
  "Return length expression for STRING: from :length clause or from :refmod."
  (or (safe-getf (rest statement) :length)
      (when (and (listp operand) (eq (first operand) :refmod))
        (safe-getf (rest operand) :length))))

(defun resolve-length-constant (length-expression)
  "Resolve length expression to integer. Supports number, symbol (constant),
constant expression, or nil."
  (cond
    ((integerp length-expression) length-expression)
    ((and length-expression (expression-constant-p length-expression))
     (expression-constant-value length-expression))
    ((stringp length-expression) (constant-value length-expression))
    (t (constant-value (princ-to-string length-expression)))))

(defun compile-6502-string-blt (out statement class-id)
  "Emit 6502 block copy loop for STRING source DELIMITED BY SIZE INTO dest."
  (let* ((source (safe-getf (rest statement) :source))
         (dest (safe-getf (rest statement) :dest))
         (length-expression (string-operand-length-expression source statement))
         (len-val (resolve-length-constant length-expression)))
    (unless length-expression
      (error "EIGHTBOL: STRING DELIMITED BY SIZE requires LENGTH clause when source/dest have no reference modification"))
    (unless (and len-val (integerp len-val) (plusp len-val))
      (error "EIGHTBOL: STRING BLT length must be a positive integer constant"))
    (let ((src-addr (string-operand-address source class-id))
          (dst-addr (string-operand-address dest class-id))
          (label (string (new-6502-label "BLT_"))))
      (%invalidate-6502-accumulator-a)
      (format out "~&~10T;; STRING ~a DELIMITED BY SIZE INTO ~a (~d bytes)"
              (if (listp source) (safe-getf (rest source) :base) source)
              (if (listp dest) (safe-getf (rest dest) :base) dest)
              len-val)
      (format out "~&~10Tldy ~a" (emit-6502-immediate 0))
      (format out "~&~10T~a:" label)
      (format out "~&~10Tlda ~a, y" src-addr)
      (format out "~&~10Tsta ~a, y" dst-addr)
      (format out "~&~10Tiny")
      (format out "~&~10Tcpy #~d" len-val)
      (format out "~&~10Tbne ~a" label))))

;;; GOTO and paragraph

(defun para-label (name &rest _)
  "Return assembly label for paragraph NAME within method.
   COBOL stabby-case (e.g. My-Para) maps to PascalCase (MyPara); underscores become part of one symbol."
  (declare (ignore _))
  (pascal-case name))

(defun compile-6502-paragraph (out statement class-id method-id)
  (let ((name (if (eq (first statement) :paragraph)
                  (second statement)
                  (or (safe-getf (rest statement) :paragraph) (second statement)))))
    (when name
      (format out "~&~a:" (para-label (format nil "~a" name) class-id (or method-id ""))))))

(defun compile-6502-goto (out statement class-id method-id)
  (let ((target (safe-getf (rest statement) :target))
        (targets (safe-getf (rest statement) :targets))
        (dep (safe-getf (rest statement) :depending-on)))
    (if dep
        ;; GO TO ... DEPENDING ON expression — trinary search tree (cmp/beq: equal, <, >)
        (let ((target-list (or targets (when target (list target)))))
          (emit-6502-load-expression out dep class-id)
          (emit-6502-goto-tristree out target-list 1 (length target-list)
                                   class-id method-id))
        ;; Simple GOTO
        (format out "~&~10T~a ~a"
                (6502-branch-always-mnemonic)
                (para-label (format nil "~a" (or target (first targets))) class-id (or method-id ""))))))

(defun emit-6502-goto-tristree (out targets low high class-id method-id)
  "Emit trinary search: cmp #mid; beq target_mid; blt left; bge right.
TARGETS is list of paragraph names (1-based indices). LOW, HIGH are 1-based inclusive."
  (when (or (null targets) (> low high)) (return-from emit-6502-goto-tristree))
  (let* ((mid (floor (+ low high) 2))
         (target-name (nth (1- mid) targets)))
    (when (null target-name) (return-from emit-6502-goto-tristree))
    (let ((label (para-label (format nil "~a" target-name) class-id (or method-id ""))))
      (if (= low high)
          ;; Leaf: single target
          (format out "~&~10Tcmp #~d~% beq ~a" mid label)
          (let ((label-less (new-6502-label "GtLess"))
                (label-more (new-6502-label "GtMore")))
            (format out "~&~10Tcmp # ~d" mid)
            (format out "~&~10Tbeq ~a" label)
            (format out "~&~10Tblt ~a" label-less)
            (format out "~&~10Tbge ~a" label-more)
            (format out "~&~a:" label-less)
            (when (<= low (1- mid))
              (emit-6502-goto-tristree out targets low (1- mid) class-id method-id))
            (format out "~&~a:" label-more)
            (when (<= (1+ mid) high)
              (emit-6502-goto-tristree out targets (1+ mid) high class-id method-id)))))))

;;; EVALUATE statement

(defun compile-6502-evaluate (out statement cpu)
  (let ((subject (safe-getf (rest statement) :subject))
        (clauses (safe-getf (rest statement) :when-clauses))
        (label-end (new-6502-label "EvalEnd")))
    (dolist (clause (ensure-list clauses))
      (cond
        ((eq (first clause) :when-other)
         ;; WHEN OTHER — fall-through, execute statements
         (dolist (s (ensure-list (second clause)))
           (compile-statement cpu (first s) (rest s))))
        ((eq (first clause) :when)
         (let ((phrases (second clause))
               (statements (third clause))
               (label-next (new-6502-label "WhenNext")))
           ;; Compare subject to phrases; if match, execute statements and jump to end
           (cond
             
             ((and (listp phrases) (string-equal (first phrases) "not"))
              ;; WHEN NOT expression — subject must not equal expression
              (emit-6502-load-expression out subject *class-id*)
              (if (expression-constant-p (second phrases))
                  (format out "~&~10Tcmp # ~a" (pascal-case (second phrases)))
                  (format out "~&~10Tcmp ~a" (pascal-case (second phrases))))
              (format out "~&~10Tbeq ~a" label-next)
              (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-end)
              (format out "~&~a:" label-next))
             
             ((and (listp phrases) (member (first phrases) '(through thru) :test #'string-equal))
              ;; WHEN expression1 THROUGH expression2 — subject in [lo, hi] inclusive
              (let ((low (second phrases)) (high (third phrases)))
                (emit-6502-load-expression out subject *class-id*)
                (if (expression-constant-p low)
                    (format out "~&~10Tcmp ~a" (emit-6502-immediate-operand low))
                    (format out "~&~10Tcmp ~a" (emit-6502-value low)))
                (format out "~&~10Tblt ~a" label-next)
                ;; Upper bound: skip when subject > high, i.e. subject >= high + 1
                (if (and (expression-constant-p high) (numberp high))
                    (format out "~&~10Tcmp #~d" (1+ high))
                    (progn
                      ;; subject - high: if >= 1 then subject > high, skip
                      (format out "~&~10Tsec")
                      (if (expression-constant-p high)
                          (format out "~&~10Tsbc ~a" (emit-6502-immediate-operand high))
                          (format out "~&~10Tsbc ~a" (emit-6502-value high)))
                      (format out "~&~10Tcmp ~a" (emit-6502-immediate 1))))
                (format out "~&~10Tbge ~a" label-next)
                (%invalidate-6502-accumulator-a)
                (dolist (s (ensure-list statements))
                  (compile-statement cpu (first s) (rest s)))
                (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-end)
                (format out "~&~a:" label-next)))
             
             ((string-equal phrases "Any")
              ;; WHEN ANY — always match
              (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-end))
             
             ((string-equal phrases "TRUE")
              (emit-6502-load-expression out subject *class-id*)
              (format out "~&~10Tbeq ~a" label-next)
              (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-end)
              (format out "~&~a:" label-next))
             
             ((string-equal phrases "FALSE")
              (emit-6502-load-expression out subject *class-id*)
              (format out "~&~10Tbne ~a" label-next)
              (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-end)
              (format out "~&~a:" label-next))
             
             (t
              ;; WHEN expression — equality
              (let ((phrase-expression (if (listp phrases) (second phrases) phrases)))
                (emit-6502-load-expression out subject *class-id*)
                (if (expression-constant-p phrase-expression)
                    (format out "~&~10Tcmp # ~a" (pascal-case phrase-expression))
                    (format out "~&~10Tcmp ~a" (emit-6502-value phrase-expression)))
                (format out "~&~10Tbne ~a" label-next))
              (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
              (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-end)
              (format out "~&~a:" label-next)))))))
    (format out "~&~a:" label-end)))

;;; INSPECT statement

(defun compile-6502-inspect (out statement class-id)
  (let ((target (safe-getf (rest statement) :target))
        (tally (safe-getf (rest statement) :tallying))
        (conv-from (safe-getf (rest statement) :converting))
        (conv-to (safe-getf (rest statement) :to))
        (repl-by (safe-getf (rest statement) :by)))
    (%invalidate-6502-accumulator-a)
    (let ((target-sym (pascal-case target)))
      (cond
        (tally
         ;; INSPECT id TALLYING tally FOR CHARACTERS — add 1 to tally per character
         (let ((tally-sym (pascal-case tally)))
           (format out "~&~10T;; INSPECT ~a TALLYING ~a FOR CHARACTERS" target tally)
           (format out "~&~10Tldy ~a" (emit-6502-immediate 0))
           (let ((label (new-6502-label "TallyLoop"))
                 (label-done (new-6502-label "TallyDone"))
                 (label-skip (new-6502-label "TallySkip")))
             (format out "~&~a:" label)
             (format out "~&~10Tlda ~a, y" target-sym)
             (format out "~&~10Tbeq ~a" label-done)
             (format out "~&~10Tlda ~a" tally-sym)
             (format out "~&~10Tclc")
             (format out "~&~10Tadc ~a" (emit-6502-immediate 1))
             (format out "~&~10Tsta ~a" tally-sym)
             (format out "~&~10Tblt ~a" label-skip)
             (format out "~&~10Tinc ~a + 1" tally-sym)
             (format out "~&~a:" label-skip)
             (format out "~&~10Tiny")
             (format out "~&~10Tbne ~a" label)
             (format out "~&~a:" label-done))))
        (conv-from
         ;; INSPECT id CONVERTING from TO to — replace chars in string
         (format out "~&~10T;; INSPECT ~a CONVERTING" target)
         (format out "~&~10Tldy ~a" (emit-6502-immediate 0))
         (let ((label (new-6502-label "InspLoop"))
               (label-next (new-6502-label "InspNext"))
               (label-done (new-6502-label "InspDone")))
           (format out "~&~a:" label)
           (format out "~&~10Tlda ~a, y" target-sym)
           (format out "~&~10Tbeq ~a" label-done)
           (if (expression-constant-p conv-from)
               (format out "~&~10Tcmp ~a" (emit-6502-immediate-operand conv-from))
               (format out "~&~10Tcmp ~a" (emit-6502-value conv-from)))
           (format out "~&~10Tbne ~a" label-next)
           (if (expression-constant-p conv-to)
               (format out "~&~10Tlda ~a" (emit-6502-immediate-operand conv-to))
               (format out "~&~10Tlda ~a" (emit-6502-value conv-to)))
           (format out "~&~10Tsta ~a, y" target-sym)
           (format out "~&~a:" label-next)
           (format out "~&~10Tiny")
           (format out "~&~10Tbne ~a" label)
           (format out "~&~a:" label-done)))
        (repl-by
         ;; INSPECT id REPLACING CHARACTERS BY expression — fill string with expression
         (let ((len (operand-width target)))
           (format out "~&~10T;; INSPECT ~a REPLACING CHARACTERS BY ~a" target repl-by)
           (emit-6502-load-expression out repl-by class-id)
           (format out "~&~10Tldy # 0")
           (let ((label (new-6502-label "InspLoop")))
             (format out "~&~a:" label)
             (format out "~&~10Tsta ~a, y" target-sym)
             (format out "~&~10Tiny")
             (format out "~&~10Tcpy # ~d" len)
             (format out "~&~10Tbne ~a" label))))))))

;;; INVOKE statement

(defun compile-6502-invoke (out statement class-id)
  (let ((object (safe-getf (rest statement) :object))
        (method (safe-getf (rest statement) :method))
        (returning (safe-getf (rest statement) :returning)))
    (let ((method-sym (pascal-case (string method))))
      (cond
        ((string-equal object "Self")
         ;; Same  OOPS  path as  INVOKE  on  a named  object:  Phantasia
         ;; CallMethod macro (DoCallMethod).
         (format out "~&~10T.CallMethod Call~a~a, ~aClass"
                 (pascal-case (method-class class-id method-sym))
	       (pascal-case method-sym)
	       (pascal-case class-id)))
        (t
         (let* ((unknown '#:Unknown)
                (obj-class (or (var-class object) unknown)))
           (if (eq obj-class Unknown)
               (error "Unknown class of method ~a" method-sym)
               (format out "~&~10T.CallMethod Call~a~a, ~aClass, ~a"
                       (pascal-case obj-class) (pascal-case method-sym)
		   (pascal-case obj-class) (pascal-case object)))))))
    (%invalidate-6502-accumulator-a)
    (when returning
      (format out "~&~10Tsta ~a" (pascal-case returning)))))

;;; CALL statement

(defun sym-string (x)
  "Return PascalCase assembly symbol from a literal, identifier, or string (COBOL stabby-case)."
  (pascal-case (if (listp x) (second x) x)))

(defun %service-call-dispatch-symbol (item-sym)
  "Map CALL SERVICE routine stem to assembly dispatch label (@code{ServiceFoo} for LUT / .FarCall).
When ITEM-SYM already has a @code{Service} prefix (case-insensitive), return it unchanged."
  (let ((s (if (stringp item-sym) item-sym (format nil "~a" item-sym))))
    (if (and (>= (length s) 7)
             (string-equal (subseq s 0 7) "Service"))
        s
        (concatenate 'string "Service" s))))

(defun compile-6502-call (out statement)
  (let* ((target (safe-getf (rest statement) :target))
         (service (safe-getf (rest statement) :service))
         (bank (safe-getf (rest statement) :bank))
         (libraryp (safe-getf (rest statement) :library))
         (tail-call-p (safe-getf (rest statement) :tail-call-p))
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
           (error "EIGHTBOL: CALL SERVICE ~a requires bank (not in service-bank table)"
                  service)))
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
  (format nil "L8_~a_~d" prefix (incf *6502-label-counter*)))

(defun compile-6502-if (out statement cpu)
  (let ((condition (safe-getf (rest statement) :condition))
        (then-statements (remove nil (safe-getf (rest statement) :then)))
        (else-statements (remove nil (safe-getf (rest statement) :else)))
        (label-else (new-6502-label "IfElse"))
        (label-end (new-6502-label "IfEnd")))
    (emit-6502-condition out condition *class-id* label-else)
    (dolist (s (ensure-list then-statements))
      (compile-statement cpu (first s) (rest s)))
    (when (and else-statements (not (null else-statements)))
      (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-end))
    (format out "~&~a:" label-else)
    (when (and else-statements (not (null else-statements)))
      (dolist (s (ensure-list else-statements))
        (compile-statement cpu (first s) (rest s)))
      (format out "~&~a:" label-end))))

(defun normalize-relation-condition (condition)
  "If CONDITION is (lhs op rhs) with op in the middle, return (op lhs rhs).
If CONDITION is (lhs IS LESS THAN rhs) or (lhs LESS THAN rhs) etc. (5 elements),
return (op lhs rhs). Otherwise return CONDITION unchanged."
  (when (listp condition)
    (cond
      ;; 6-element infix negated equality: (lhs IS NOT EQUAL TO rhs)
      ((= (length condition) 6)
       (destructuring-bind (a b c d e f) condition
         (flet ((token (x str) (string-equal (princ-to-string x) str)))
           (when (and (token b "IS")
                      (token c "NOT")
                      (or (token d "=") (token d "EQUAL"))
                      (token e "TO"))
             (return-from normalize-relation-condition
               (list :not (list '= a f)))))))
      ;; 4-element infix negated equality: (lhs NOT = rhs) / (lhs NOT EQUAL rhs)
      ((= (length condition) 4)
       (destructuring-bind (a b c d) condition
         (flet ((token (x str) (string-equal (princ-to-string x) str)))
           (when (and (token b "NOT")
                      (or (token c "=") (token c "EQUAL")))
             (return-from normalize-relation-condition
               (list :not (list := a d)))))))
      ;; 5-element: (expression is less than expression) or (expression less than expression), etc.
      ((or (member (length condition) '(4 5)))
       (destructuring-bind (a b c d e) condition
         (flet ((token (x str) (string-equal (princ-to-string x) str)))
           (let ((op (cond ((and (token b "IS") (token c "LESS") (token d "THAN")) '<)
                           ((and (token b "LESS") (token c "THAN")) '<)
                           ((and (token b "IS") (token c "GREATER") (token d "THAN")) '>)
                           ((and (token b "GREATER") (token c "THAN")) '>)
                           (t nil))))
             (when op (return-from normalize-relation-condition (list op a e)))))))
      ;; 3-element: (lhs op rhs) -> (op lhs rhs)
      ((= (length condition) 3)
       (let ((a (first condition))
             (b (second condition))
             (c (third condition)))
         (when (member (princ-to-string a) '(/= ≠ <>) :test #'string-equal)
	   (return-from normalize-relation-condition (list 'not (list '= b c))))
         (when (member (princ-to-string b) '(/= ≠ <>) :test #'string-equal)
	   (return-from normalize-relation-condition (list 'not (list '= a c))))
         (when (member (princ-to-string b) '(= equal < less > greater >= ≤ ≥ <=)
                       :test #'string-equal)
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
       (emit-6502-load-expression out lhs class-id)
       (format out "~&~10Tbeq ~a" branch-label))
      (t
       (let ((label-then (new-6502-label "IfGT0")))
         (dotimes (i (1- w))
           (emit-6502-load-byte-n out lhs class-id i w)
           (format out "~&~10Tbne ~a" label-then))
         (emit-6502-load-byte-n out lhs class-id (1- w) w)
         (format out "~&~10Tbeq ~a" branch-label)
         (format out "~&~a:" label-then))))))

(defun emit-6502-branch-if-expression-not-all-zero (out expression class-id branch-label)
  "Jump to BRANCH-LABEL if unsigned EXPRESSION is not all-zero bytes.

Used for @code{IF (IS ZERO X)}, @code{(= X 0)}, and @code{(= 0 X)}: the false branch
runs when any byte is non-zero. W is @code{operand-width} of EXPRESSION; W=1 uses
@code{emit-6502-load-expression} + @code{bne}.

@table @asis
@item EXPRESSION
Slot, identifier, or other loadable expression.
@item CLASS-ID
Current class for slot symbols.
@item BRANCH-LABEL
Label when value is not all zeros (condition false for IS-ZERO).
@end table"
  (let ((w (max 1 (expression-operand-width expression))))
    (if (= w 1)
        (progn
          (emit-6502-load-expression out expression class-id)
          (format out "~&~10Tbne ~a" branch-label))
        (dotimes (i w)
          (emit-6502-load-byte-n out expression class-id i w)
          (format out "~&~10Tbne ~a" branch-label)))))

(defun emit-6502-branch-if-expression-all-zero (out expression class-id branch-label)
  "Jump to BRANCH-LABEL if unsigned EXPRESSION is all-zero bytes (W-wide).

Used for @code{IS NOT ZERO} false path and @code{(NOT (= X 0))} when X is zero:
condition is false when every byte is zero. W=1 uses @code{emit-6502-load-expression} + @code{beq}.

@table @asis
@item EXPRESSION
Slot or identifier.
@item CLASS-ID
Current class for slot symbols.
@item BRANCH-LABEL
Label when value is all zeros (IS-NOT-ZERO is false).
@end table"
  (let ((w (max 1 (expression-operand-width expression))))
    (if (= w 1)
        (progn
          (emit-6502-load-expression out expression class-id)
          (format out "~&~10Tbeq ~a" branch-label))
        (let ((label-some (new-6502-label "SomeNz")))
          (dotimes (i w)
            (emit-6502-load-byte-n out expression class-id i w)
            (format out "~&~10Tbne ~a" label-some))
          (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) branch-label)
          (format out "~&~a:" label-some)))))

(defun relation-op-canonical (op)
  "If OP is a comparison operator in any form, return canonical string \">\", \"<\", \">=\", \"/=\", or \"<=\".
Otherwise return NIL."
  (cond ((member op '(> greater) :test #'string-equal) ">")
        ((member op '(< less) :test #'string-equal) "<")
        ((member op '(>= ≥) :test #'string-equal) ">=")
        ((member op '(<= ≤) :test #'string-equal) "<=")
        ((member op '(/= ≠) :test #'string-equal) "/=")
        (t (let ((s (princ-to-string op)))
             (cond ((or (string= s ">") (string= s ":>")) ">")
                   ((or (string= s "<") (string= s ":<")) "<")
                   ((or (string= s ">=") (string= s ":>=")) ">=")
                   ((or (string= s "<=") (string= s ":<=")) "<=")
	         ((or (string= s "/=") (string= s ":/=")) "/=")
                   (t nil))))))

(defun emit-6502-condition (out condition class-id branch-label)
  "Emit 6502 code to evaluate CONDITION and branch to BRANCH-LABEL if false."
  (when (listp condition)
    (let* ((condition (or (normalize-relation-condition condition) condition))
           (op (first condition)))
      (cond
        
        ;; 3-element comparison: op as string or symbol (princ-to-string covers both)
        ((and (= (length condition) 3)
              (member (princ-to-string op) '(">" "<" ">=" "<=" "=" "/=") :test #'string=))
         (let ((lhs (second condition))
               (rhs (third condition)))
           (if (and (string= op ">=") (expression-literal-zero-p rhs))
               (emit-6502-false-when-not-unsigned-greater-than-zero out lhs class-id branch-label)
               (progn
                 (emit-6502-compare out lhs rhs class-id)
                 (cond ((string= op ">")
                        (format out "~&~10Tblt ~a" branch-label)
                        (format out "~&~10Tbeq ~a" branch-label))
                       ((string= op "<")
                        (format out "~&~10Tbge ~a" branch-label))
                       ((string= op ">=")
                        (format out "~&~10Tblt ~a" branch-label))
		   ((string= op "=")
		    (cond ((or (expression-literal-zero-p (second condition))
                                   (expression-literal-zero-p (third condition)))
                               (let ((slot-expression (if (expression-literal-zero-p (second condition))
                                                    (third condition)
                                                    (second condition))))
                                 (emit-6502-branch-if-expression-not-all-zero out slot-expression class-id branch-label)))
                              ((or (string-equal :null (princ-to-string (second condition)))
                                   (string-equal :null (princ-to-string (third condition))))
                               (let ((pointer-expression (if (string-equal :null
                                                                     (princ-to-string (second condition)))
                                                       (third condition)
                                                       (second condition))))
                                 (emit-6502-load-hi-byte out pointer-expression class-id)
                                 (format out "~&~10Tbne ~a" branch-label)))
                              (t
                               (format out "~&~10tbeq ~a" branch-label)))                        )
                       ((string= op "/=")
		    (cond ((or (expression-literal-zero-p (second condition))
                                   (expression-literal-zero-p (third condition)))
                               (let ((slot-expression (if (expression-literal-zero-p (second condition))
                                                    (third condition)
                                                    (second condition))))
                                 (emit-6502-branch-if-expression-all-zero out slot-expression class-id branch-label)))
                              ((or (string-equal :null (princ-to-string (second condition)))
                                   (string-equal :null (princ-to-string (third condition))))
                               (let ((pointer-expression (if (string-equal :null
                                                                     (princ-to-string (second condition)))
                                                       (third condition)
                                                       (second condition))))
                                 (emit-6502-load-hi-byte out pointer-expression class-id)
                                 (format out "~&~10Tbeq ~a" branch-label)))
                              (t
                               (format out "~&~10tbne ~a" branch-label))))
                       ((string= op "<=")
                        (let ((label-stay (new-6502-label "LeStay")))
                          (format out "~&~10Tbeq ~a" label-stay)
                          (format out "~&~10Tbge ~a" branch-label)
                          (format out "~&~a:" label-stay))))))))
        
        ;; IS NOT NULL — must come before Negated zero test so (:not (= x :null)) matches here
        ((and (string-equal op :not)
              (listp (second condition))
              (member (first (second condition)) '(= equal) :test #'string-equal)
              (or (string-equal (princ-to-string (second condition)) :null)
                  (string-equal (princ-to-string (third condition)) :null)))
         (let* ((inner (second condition))
                (pointer-expression (if (string-equal (princ-to-string (second condition)) :null)
                                  (third inner)
                                  (second inner))))
           (emit-6502-load-hi-byte out pointer-expression class-id)
           (format out "~&~10Tbeq ~a" branch-label)))
        
        ;; Negated zero test (IS NOT ZERO / NOT EQUAL TO 0)
        ((and (string-equal op :not)
              (listp (second condition))
              (member (first (second condition)) '(= equal) :test #'string-equal))
         (let ((inner (second condition)))
           (if (or (expression-literal-zero-p (second inner))
                   (expression-literal-zero-p (third inner)))
               (let ((slot-expression (if (expression-literal-zero-p (second inner))
                                    (third inner)
                                    (second inner))))
                 (emit-6502-branch-if-expression-all-zero out slot-expression class-id branch-label))
	     ;; else the compare does not have a literal zero on either side
               (let* ((left (second inner))
                      (right (third inner))
                      (w (min (operand-width left) (operand-width right)))
		  (label-done (new-6502-label "NotEqDone")))
	       (dotimes (i w)
                   (emit-6502-load-byte-n out left class-id i w)
                   (if (expression-constant-p right)
                       (format out "~&~10Tcmp ~a"
                               (emit-6502-immediate (constant-byte-value right i)))
                       (emit-6502-cmp-byte-n-of-expression out right class-id i w))
                   ;; Any mismatch means NOT (= ...) is true, so skip false-branch.
                   (format out "~&~10Tbne ~a" label-done))
	       ;; Any remaining, higher bytes must be zeroes
	       (let ((rem (- (operand-width left) (operand-width right)))
		   (bigger left))
	         (unless (zerop rem)
		 (when (minusp rem) (setf bigger right))
		 (dotimes (i rem)
		   (emit-6502-load-byte-n out bigger class-id (+ i w) (+ rem w))
		   ;; and higher non-zero byte is not equal, ergo ≠ is true, so skip to label-done.
		   (format out "~&~10Tbne ~a" label-done))))
	       ;; All bytes equal ⇒ condition false.
	       (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) branch-label)
	       (format out "~&~a:" label-done)))))
        
        ;; BIT-AND — bit test: zero flag set if masked result is zero
        ((eq op :bit-and)
         ;; Reuse emit-6502-load-expression for the full bit-and expression
         (emit-6502-load-expression out condition class-id)
         (format out "~&~10Tbeq ~a" branch-label))
        
        ;; AND — short-circuit: if first is false, branch; else evaluate second
        ((eq op :and)
         (emit-6502-condition out (second condition) class-id branch-label)
         (emit-6502-condition out (third condition) class-id branch-label))
        
        ;; OR — short-circuit: if first is true, skip to success; else evaluate second
        ((eq op :or)
         (let ((label-cond2 (new-6502-label "OrCond2"))
               (label-skip (new-6502-label "OrSkip")))
           (emit-6502-condition out (second condition) class-id label-cond2)
           (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-skip)
           (format out "~&~a:" label-cond2)
           (emit-6502-condition out (third condition) class-id branch-label)
           (format out "~&~a:" label-skip)))
        
        ((eq op :null)
         (emit-6502-load-byte-n out (second condition) class-id 1 2)
         (format out "~&~10Tbne ~a" branch-label))
        
        ((eq op :not-null)
         (emit-6502-load-byte-n out (second condition) class-id 1 2)
         (format out "~&~10Tbeq ~a" branch-label))
        
        ;; Generic fallback
        (t (error "Unhandled comparison operation ~s" condition))))))

(defun emit-6502-compare-unsigned (out lhs rhs class-id w)
  "Emit unsigned compare of LHS vs RHS for W-byte values (little-endian in memory).

Unsigned semantics: treat each W-byte value as an integer; compare from the most significant
stored byte (index @code{W-1}) to least (index @code{0}). If any byte differs, @code{bne} skips
remaining compares; the last @code{cmp} (low byte) only runs when all higher bytes were equal.
Last @code{cmp} leaves flags for @code{emit-6502-condition} (@code{blt}/@code{bge}/@code{beq} on 6502)."
  (let ((label-done (new-6502-label "CmpU")))
    (loop for i from (1- w) downto 1 do
      (emit-6502-load-byte-n out lhs class-id i w)
      (emit-6502-cmp-byte-n-of-expression out rhs class-id i w)
      (format out "~&~10Tbne ~a" label-done))
    (emit-6502-load-byte-n out lhs class-id 0 w)
    (emit-6502-cmp-byte-n-of-expression out rhs class-id 0 w)
    (format out "~&~a:" label-done)))

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
  (cond
    ((and (not (operand-signed-p lhs))
          (not (operand-signed-p rhs)))
     (emit-6502-compare-unsigned out lhs rhs class-id (max (expression-operand-width lhs)
                                                           (expression-operand-width rhs))))
    (t (error "Signed logic not implemented"))))

;;; ADD / SUBTRACT / COMPUTE

(defun literal-one-p (expression)
  "True when EXPRESSION is the integer 1, string \"1\", or (:literal 1)."
  (or (eql expression 1)
      (equalp expression '(:literal 1))))

(defun compile-6502-add (out statement class-id)
  ;; LET* so RESULT and W use GIVING / TO from this statement (parallel LET leaves RESULT nil).
  (let* ((from (safe-getf (rest statement) :from))
         (to-op (safe-getf (rest statement) :to))
         (giving (safe-getf (rest statement) :giving))
         (result (or giving to-op))
         (w (max (operand-width result)
                 (expression-operand-width from)
                 (operand-width to-op)))
         (bcd-p (when result (operand-bcd-p result))))
    (unless (or giving to-op)
      (error 'backend-error
             :message "ADD requires TO or GIVING"
             :cpu :6502
             :detail statement))
    (cond
      ;; Optimise: ADD 1 TO variable (no GIVING) → inc variable (byte only)
      ((and (literal-one-p from) (not giving) (stringp to-op) (= (operand-width to-op) 1))
       (format out "~&~10Tinc ~a" to-op)
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
               (if (expression-constant-p to-op)
                   (format out "~&~10Tadc ~a" (emit-6502-immediate (constant-byte-value to-op i)))
                   (emit-6502-adc-byte-n-of-expression out to-op class-id i w))
               (emit-6502-store-byte-n out result class-id i w
                                       :skip-ldy (and (equal from result)
                                                      (%6502-load-byte-n-sets-y-for-slot-store from class-id))))
             (when bcd-p (format out "~&~10Tcld")))
           (let ((use-stack (expression-contains-subscript-p from)))
             (when (or use-stack (expression-contains-subscript-p to-op) (expression-contains-subscript-p result))
               (setf use-stack t))
             ;; Store low sum at (Self),y immediately after low adc when Y is already the
             ;; destination low offset (TO and result same slot OF Self, no subscript).
             (let ((store-low-to-self-now-p
                     (and (not use-stack)
                          (not (expression-constant-p to-op))
                          (slot-of-self-p to-op)
                          (slot-of-self-p result)
                          (equal (slot-of-expression to-op) (slot-of-expression result)))))
               (when bcd-p (format out "~&~10Tsed"))
               (emit-6502-load-expression out from class-id)
               (format out "~&~10Tclc")
               (if (expression-constant-p to-op)
                   (let ((v (expression-constant-value to-op)))
                     (format out "~&~10Tadc #<~d" v))
                   (cond
                     ((and (listp to-op) (eql :on (first to-op)))
                      (emit-6502-alu-with-memory-rhs out "adc" to-op class-id))
                     ((slot-of-expression to-op)
                      (emit-6502-alu-with-memory-rhs out "adc" to-op class-id))
                     (t
                      (format out "~&~10Tadc ~a" (emit-6502-value to-op)))))
               (if store-low-to-self-now-p
                   (format out "~&~10Tsta (Self), y")
                   (if use-stack
                       (format out "~&~10Tpha")
                       (format out "~&~10Ttax")))
               (emit-6502-load-hi-byte out from class-id)
               (if (expression-constant-p to-op)
                   (let ((v (expression-constant-value to-op)))
                     (format out "~&~10Tadc #>~d" v))
                   (cond
                     ((slot-of-expression to-op)
                      (let* ((slot-of-expression (slot-of-expression to-op))
                             (offset (apply #'slot-symbol (rest slot-of-expression)))
                             (pointer (6502-object-pointer-label (third slot-of-expression) class-id)))
                        (format out "~&~10Tldy #~a" (format nil "(~a+1)" offset))
                        (format out "~&~10Tadc (~a), y" pointer)))
                     (t
                      (format out "~&~10Tadc ~a + 1" (emit-6502-value to-op)))))
               (cond ((slot-of-expression result) 
                      (let* ((n (slot-of-expression result))
                             (offset (apply #'slot-symbol (rest n)))
                             (pointer (6502-object-pointer-label (third n) class-id)))
                        (format out "~&~10Tldy #~a + 1" offset)
                        (format out "~&~10Tsta (~a), y" pointer)
                        (format out "~&~10Tdey")
                        (format out "~&~10T~a" (if use-stack "pla" "txa"))
                        (format out "~&~10Tsta (~a), y" pointer)))
                     ((slot-on-expression result) 
                      (let* ((n (slot-on-expression result))
                             (offset (apply #'slot-symbol (rest n)))
                             (pointer (6502-object-pointer-label (third n) class-id)))
                        (format out "~&~10Tldy #~a + 1" offset)
                        (format out "~&~10Tsta ~a, y" pointer)
                        (format out "~&~10Tdey")
                        (format out "~&~10T~a" (if use-stack "pla" "txa"))
                        (format out "~&~10Tsta ~a, y" pointer)))
                     (t (let ((res-sym (emit-6502-value result)))
                          (format out "~&~10Tsta ~a + 1" res-sym)
                          (format out "~&~10T~a" (if use-stack "pla" "txa"))
                          (format out "~&~10Tsta ~a" res-sym)))))
             (when bcd-p (format out "~&~10Tcld"))
             ;; A holds the low byte after the last sta, not the full W-byte value — do not reuse as HP.
             (%invalidate-6502-accumulator-a))))
      ;; General 8-bit case
      (t
       (emit-6502-load-expression out from class-id)
       (when bcd-p (format out "~&~10Tsed"))
       (format out "~&~10Tclc")
       (if giving
           (progn
             (if (expression-constant-p to-op)
                 (format out "~&~10Tadc ~a" (emit-6502-immediate-operand to-op))
                 (cond
                   ((slot-of-self-p to-op)
                    (let ((n (slot-of-expression to-op)))
                      (format out "~&~10Tldy ~a" (emit-6502-immediate (apply #'slot-symbol (rest n))))
                      (format out "~&~10Tadc (Self), y")))
                   ((and (slot-of-expression to-op) (not (slot-of-self-p to-op)))
                    (emit-6502-alu-with-memory-rhs out "adc" to-op class-id))
                   (t
                    (format out "~&~10Tadc ~a" (emit-6502-value to-op)))))
             (emit-6502-store-byte-n out giving class-id 0 1))
           (cond
             ((slot-of-expression to-op) 
              (emit-6502-alu-with-memory-rhs out "adc" to-op class-id)
              (emit-6502-store-byte-n out to-op class-id 0 1)
              (%6502-note-accumulator-holds-value-of to-op))
             (t
              (format out "~&~10Tadc ~a" (emit-6502-value to-op))
              (format out "~&~10Tsta ~a" (emit-6502-value to-op))
              (when (= (operand-width to-op) 1)
                (%6502-note-accumulator-holds-value-of to-op)))))
       (when bcd-p (format out "~&~10Tcld"))))))

(defun compile-6502-subtract (out statement class-id)
  ;; LET* so RESULT and W use FROM / GIVING from this statement (same parallel-binding issue as ADD).
  (let* ((subtrahend (safe-getf (rest statement) :subtrahend))
         (from (safe-getf (rest statement) :from))
         (giving (safe-getf (rest statement) :giving))
         (result (or giving from))
         (w (max (operand-width result)
                 (expression-operand-width subtrahend)
                 (operand-width from)))
         (bcd-p (when result (usage-bcd-p (expression-to-width-name result)))))
    (cond
      ;; Optimise: SUBTRACT 1 SUBTRAHEND variable (no GIVING, plain variable) → dec (byte only)
      ((and (literal-one-p subtrahend) (not giving) (stringp from) (= w 1))
       (format out "~&~10Tdec ~a" from)
       (%invalidate-6502-accumulator-a))
      ;; Multi-byte SUBTRACT (w >= 2): result = from - subtrahend
      ((>= w 2)
       (when (and bcd-p (> w 2))
         (error "EIGHTBOL: BCD SUBTRACT with width ~d not yet implemented" w))
       (if (> w 2)
           (progn
             (when bcd-p (format out "~&~10Tsed"))
             (format out "~&~10Tsec")
             (dotimes (i w)
               (emit-6502-load-byte-n out from class-id i w)
               (if (expression-constant-p subtrahend)
                   (format out "~&~10Tsbc ~a" (emit-6502-immediate (constant-byte-value subtrahend i)))
                   (emit-6502-sbc-byte-n-of-expression out subtrahend class-id i w))
               (emit-6502-store-byte-n out result class-id i w
                                       :skip-ldy (and (equal from result)
                                                      (%6502-load-byte-n-sets-y-for-slot-store from class-id))))
             (when bcd-p (format out "~&~10Tcld")))
           (cond
             ((%6502-subtract-2byte-inplace-eligible-p subtrahend from result giving class-id w bcd-p)
              (emit-6502-subtract-2byte-self-inplace out subtrahend result class-id bcd-p))
             (t
              (let ((use-stack (expression-contains-subscript-p from)))
                (when (or use-stack (expression-contains-subscript-p subtrahend) (expression-contains-subscript-p result))
                  (setf use-stack t))
                (when bcd-p (format out "~&~10Tsed"))
                ;; Low bytes: save result_lo (tax or pha)
                (emit-6502-load-expression out from class-id)
                (format out "~&~10Tsec")
                (if (expression-constant-p subtrahend)
                    (let ((v (expression-constant-value subtrahend)))
                      (format out "~&~10Tsbc #<~d" v))
                    (cond
                      ((slot-of-expression subtrahend) 
                       (emit-6502-alu-with-memory-rhs out "sbc" subtrahend class-id))
                      (t
                       (format out "~&~10Tsbc ~a" (emit-6502-value subtrahend)))))
                (if use-stack
                    (format out "~&~10Tpha")
                    (format out "~&~10Ttax"))
                ;; High bytes: result_hi = from_hi - subtrahend_hi - borrow
                (emit-6502-load-hi-byte out from class-id)
                (if (expression-constant-p subtrahend)
                    (let ((v (expression-constant-value subtrahend)))
                      (format out "~&~10Tsbc #>~d" v))
                    (cond
                      ((slot-of-expression subtrahend) 
                       (let* ((slot-of-expression (slot-of-expression subtrahend))
                              (offset (apply #'slot-symbol (rest slot-of-expression)))
                              (pointer (6502-object-pointer-label (third slot-of-expression) class-id)))
                         (format out "~&~10Tldy #~a" (format nil "(~a+1)" offset))
                         (format out "~&~10Tsbc (~a), y" pointer)))
                      (t
                       (format out "~&~10Tsbc ~a + 1" (emit-6502-value subtrahend)))))
                (if (slot-of-expression result)
                    (let* ((n (slot-of-expression result))
                           (offset (apply #'slot-symbol (rest n)))
                           (pointer (6502-object-pointer-label (third n) class-id)))
                      (format out "~&~10Tldy #~a + 1" offset)
                      (format out "~&~10Tsta (~a), y" pointer)
                      (format out "~&~10Tdey")
                      (format out "~&~10T~a" (if use-stack "pla" "txa"))
                      (format out "~&~10Tsta (~a), y" pointer))
                    (let ((res-sym (emit-6502-value result)))
                      (format out "~&~10Tsta ~a + 1" res-sym)
                      (format out "~&~10T~a" (if use-stack "pla" "txa"))
                      (format out "~&~10Tsta ~a" res-sym)))
                (when bcd-p (format out "~&~10Tcld"))
                (%invalidate-6502-accumulator-a))))))
      ((slot-of-expression from)
       (let* ((n (slot-of-expression from))
              (offset (apply #'slot-symbol (rest n)))
              (pointer (6502-object-pointer-label (third n) class-id)))
         (format out "~&~10Tldy # ~a" offset)
         (format out "~&~10Tlda (~a), y" pointer)
         (format out "~&~10Tsec")
         (if (expression-constant-p subtrahend)
             (format out "~&~10Tsbc # ~a" subtrahend)
             (format out "~&~10Tsbc ~a" (emit-6502-value subtrahend)))
         (format out "~&~10Tsta (~a), y" pointer)
         (%6502-note-accumulator-holds-value-of from)))
      (giving
       (emit-6502-load-expression out from class-id)
       (when bcd-p (format out "~&~10Tsed"))
       (format out "~&~10Tsec")
       (if (expression-constant-p subtrahend)
           (format out "~&~10Tsbc ~a" (emit-6502-immediate-operand subtrahend))
           (cond
             ((slot-of-self-p subtrahend)
              (let ((n (slot-of-expression subtrahend)))
                (format out "~&~10Tldy ~a" (emit-6502-immediate (apply #'slot-symbol (rest n))))
                (format out "~&~10Tsbc (Self), y")))
             ((and (slot-of-expression subtrahend) (not (slot-of-self-p subtrahend)))
              (emit-6502-alu-with-memory-rhs out "sbc" subtrahend class-id))
             (t
              (format out "~&~10Tsbc ~a" (emit-6502-value subtrahend)))))
       (emit-6502-store-byte-n out giving class-id 0 1)
       (when bcd-p (format out "~&~10Tcld")))
      (t
       (emit-6502-load-expression out from class-id)
       (when bcd-p (format out "~&~10Tsed"))
       (format out "~&~10Tsec")
       (if (expression-constant-p subtrahend)
           (format out "~&~10Tsbc ~a" (emit-6502-immediate-operand subtrahend))
           (cond
             ((slot-of-self-p subtrahend)
              (let ((n (slot-of-expression subtrahend)))
                (format out "~&~10Tldy ~a" (emit-6502-immediate (apply #'slot-symbol (rest n))))
                (format out "~&~10Tsbc (Self), y")))
             ((and (slot-of-expression subtrahend) (not (slot-of-self-p subtrahend)))
              (emit-6502-alu-with-memory-rhs out "sbc" subtrahend class-id))
             (t
              (format out "~&~10Tsbc ~a" (emit-6502-value subtrahend)))))
       (cond
         ((slot-of-self-p from)
          (let ((n (slot-of-expression from)))
            (format out "~&~10Tldy ~a" (emit-6502-immediate (apply #'slot-symbol (rest n))))
            (format out "~&~10Tsta (Self), y")
            (%6502-note-accumulator-holds-value-of from)))
         ((and (slot-of-expression from) (not (slot-of-self-p from)))
          (emit-6502-store-byte-n out from class-id 0 1)
          (%6502-note-accumulator-holds-value-of from))
         (t
          (format out "~&~10Tsta ~a" (emit-6502-value from))
          (when (= (operand-width from) 1)
            (%6502-note-accumulator-holds-value-of from))))
       (when bcd-p (format out "~&~10Tcld"))))))

(defun compile-6502-compute (out statement class-id)
  "COMPUTE target = expression. Supports arbitrary width 1–8 bytes.
For w=1, expression may be compound (add, subtract, etc.). For w>1, expression must be variable/constant."
  (let* ((target (safe-getf (rest statement) :target))
         (expression (safe-getf (rest statement) :expression))
         (w (max (operand-width target) (expression-operand-width expression))))
    (cond
      ((= w 1)
       (emit-6502-load-expression out expression class-id)
       (emit-6502-store-byte-n out target class-id 0 1))
      (t
       (dotimes (i w)
         (emit-6502-load-byte-n out expression class-id i w)
         (emit-6502-store-byte-n out target class-id i w
                                 :skip-ldy (and (equal expression target)
                                                (%6502-load-byte-n-sets-y-for-slot-store expression class-id))))))))

;;; SET statement

(defun compile-6502-set (out statement class-id)
  "SET target TO value. Supports TO expression, TO NULL, UP BY, DOWN BY, TO ADDRESS OF, TO SELF."
  (let* ((target (safe-getf (rest statement) :target))
         (value (safe-getf (rest statement) :value))
         (up-by (safe-getf (rest statement) :up-by))
         (down-by (safe-getf (rest statement) :down-by))
         (by-expression (safe-getf (rest statement) :by))
         (address-of (safe-getf (rest statement) :address-of))
         (to-self (safe-getf (rest statement) :to-self))
         (w (operand-width (or target to-self up-by down-by))))
    (cond
      (up-by
       ;; SET id UP BY expression => ADD expression TO id
       (compile-6502-add out (list :add :from by-expression :to up-by :giving nil) class-id))
      (down-by
       ;; SET id DOWN BY expression => SUBTRACT expression FROM id
       (compile-6502-subtract out (list :subtract :from by-expression :from-target down-by :giving nil) class-id))
      ((and address-of target)
       ;; SET target TO ADDRESS OF source: store address of source into target (2-byte pointer).
       (let ((source-id address-of))
         (if (slot-of-self-p source-id)
             (let ((offset (apply #'slot-symbol (rest (slot-of-expression source-id)))))
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
      ((string-equal value "null")
       ;; 6502: NULL = high byte zero. Set pointer to NULL by zeroing high byte only.
       (format out "~&~10Tlda #0")
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
(defun compile-6502-divide (out statement class-id)
  (let* ((divisor (safe-getf (rest statement) :divisor))
         (into-id (safe-getf (rest statement) :into))
         (giving (safe-getf (rest statement) :giving))
         (by-dividend (safe-getf (rest statement) :by))
         (source (if by-dividend by-dividend into-id))
         (dest (or giving into-id)))
    (unless (and (expression-constant-p divisor)
                 (power-of-two-p (expression-constant-value divisor)))
      (error 'backend-error
             :message "DIVIDE: divisor must be constant power-of-two (1, 2, 4, 8, ...)"
             :cpu :6502 :detail statement))
    (let ((shift (log2 (expression-constant-value divisor)))
          (w (operand-width dest)))
      (when (zerop shift)
        (return-from compile-6502-divide))
      (if (= w 1)
          (progn
            (emit-6502-load-expression out source class-id)
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
(defun compile-6502-multiply (out statement class-id)
  (let* ((multiplier (safe-getf (rest statement) :multiplier))
         (by-id (safe-getf (rest statement) :by))
         (giving (safe-getf (rest statement) :giving))
         (source (or giving by-id))
         (dest (or giving by-id)))
    (unless (and (expression-constant-p multiplier)
                 (power-of-two-p (expression-constant-value multiplier)))
      (error 'backend-error
             :message "MULTIPLY: multiplier must be constant power-of-two (1, 2, 4, 8, ...)"
             :cpu :6502 :detail statement))
    (let ((shift (log2 (expression-constant-value multiplier)))
          (w (operand-width dest)))
      (when (zerop shift)
        (return-from compile-6502-multiply))
      (if (= w 1)
          (progn
            (emit-6502-load-expression out source class-id)
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

(defun compile-6502-perform (out statement class-id)
  "Emit PERFORM: jsr to paragraph label (same method). Uses para-label so target matches paragraph."
  (destructuring-bind
      (&key procedure varying from by until times &allow-other-keys)
      (rest statement)
    (cond
      
      ((and varying procedure times)
       (let ((label-loop (new-6502-label "PerfLoop"))
             (label-end (new-6502-label "PerfEnd")))
         (format out "~&~10Tlda # ~d" (or from 0))
         (format out "~&~10Tsta ~a" (pascal-case varying))
         (format out "~&~a:" label-loop)
         (format out "~&~10Tjsr ~a" (pascal-case procedure))
         (%invalidate-6502-accumulator-a)
         (format out "~&~10Tlda ~a" (pascal-case varying))
         (format out "~&~10Tclc")
         (format out "~&~10Tadc # ~d" (or by 1))
         (format out "~&~10Tsta ~a" (pascal-case varying))
         (format out "~&~10Tcmp # ~d" (* by times))
         (format out "~&~10Tbne ~a" label-loop)
         (format out "~&~a:" label-end)))
      
      ((and varying procedure until)
       (let ((label-loop (new-6502-label "PerfLoop"))
             (label-end (new-6502-label "PerfEnd")))
         (format out "~&~10Tlda # ~d" (or from 0))
         (format out "~&~10Tsta ~a" (pascal-case varying))
         (format out "~&~a:" label-loop)
         (format out "~&~10Tjsr ~a" (pascal-case procedure))
         (%invalidate-6502-accumulator-a)
         (format out "~&~10Tlda ~a" (pascal-case varying))
         (format out "~&~10Tclc")
         (format out "~&~10Tadc # ~d" (or by 1))
         (format out "~&~10Tsta ~a" (pascal-case varying))
         (emit-6502-condition out until class-id label-end)
         (format out "~&~10T~a ~a" (6502-branch-always-mnemonic) label-loop)
         (format out "~&~a:" label-end)))

      (procedure (format out "~&~10Tjsr ~a" (pascal-case procedure))
                 (%invalidate-6502-accumulator-a))
      
      ((or times until)
       (error "PERFORM TIMES or UNTIL require VARYING and procedure paragraph name."))
      
      (t (error "PERFORM requires procedure paragraph name.")))))

;;; compile-statement methods — one per (cpu, ast-node-type)
;;; Brief methods delegating to compile-6502-* helpers.

(defmacro define-6502-statement (statement-type (ast-node-data) &body body)
  "Define compile-statement for :6502 and :65c02/:65c816/:huc6280 (same impl). Uses *output-stream*."
  `(progn
     (defmethod compile-statement ((cpu (eql :6502)) (statement-type (eql ,statement-type)) ,ast-node-data)
       ,@body)
     (defmethod compile-statement ((cpu (eql :65c02)) (statement-type (eql ,statement-type)) ,ast-node-data)
       ,@body)
     (defmethod compile-statement ((cpu (eql :rp2a03)) (statement-type (eql ,statement-type)) ,ast-node-data)
       ,@body)
     (defmethod compile-statement ((cpu (eql :65c816)) (statement-type (eql ,statement-type)) ,ast-node-data)
       ,@body)
     (defmethod compile-statement ((cpu (eql :huc6280)) (statement-type (eql ,statement-type)) ,ast-node-data)
       ,@body)))

(defun statement (sym data) (cons sym data))

(define-6502-statement :comment (ast-node-data)
  (format *output-stream* "~&~10T;; ~a"
          (if (listp ast-node-data)
              (format nil "~{~a~%~10T;; ~}"
                      (mapcar (curry #'split-sequence #\newline) ast-node-data))
              (split-sequence #\Newline ast-node-data))))

(define-6502-statement :goback (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(define-6502-statement :exit-method (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(define-6502-statement :exit-program (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(define-6502-statement :stop-run (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(define-6502-statement :exit (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~&~10Trts"))

(define-6502-statement :move (ast-node-data)
  (compile-6502-move *output-stream* (statement :move ast-node-data) *class-id*))

(define-6502-statement :invoke (ast-node-data)
  (compile-6502-invoke *output-stream* (statement :invoke ast-node-data) *class-id*))

(define-6502-statement :call (ast-node-data)
  (compile-6502-call *output-stream* (statement :call ast-node-data)))

(define-6502-statement :if (ast-node-data)
  (compile-6502-if *output-stream* (statement :if ast-node-data) cpu))

(define-6502-statement :add (ast-node-data)
  (compile-6502-add *output-stream* (statement :add ast-node-data) *class-id*))

(define-6502-statement :subtract (ast-node-data)
  (compile-6502-subtract *output-stream* (statement :subtract ast-node-data) *class-id*))

(define-6502-statement :compute (ast-node-data)
  (compile-6502-compute *output-stream* (statement :compute ast-node-data) *class-id*))

(define-6502-statement :divide (ast-node-data)
  (compile-6502-divide *output-stream* (statement :divide ast-node-data) *class-id*))

(define-6502-statement :multiply (ast-node-data)
  (compile-6502-multiply *output-stream* (statement :multiply ast-node-data) *class-id*))

(define-6502-statement :set (ast-node-data)
  (compile-6502-set *output-stream* (statement :set ast-node-data) *class-id*))

(define-6502-statement :log-fault (ast-node-data)
  (let ((code (safe-getf ast-node-data :code)))
    (format *output-stream* "~&~10T.LogFault ~a"
            (if (stringp code)
                (format nil "\"~a\"" code)
                (emit-6502-value code)))))

(define-6502-statement :debug-break (ast-node-data)
  (let ((code (safe-getf ast-node-data :code)))
    (format *output-stream* "~&~10T.DebugBreak ~a"
            (if (stringp code)
                (format nil "\"~a\"" code)
                (emit-6502-value code)))))

(define-6502-statement :perform (ast-node-data)
  (compile-6502-perform *output-stream* (statement :perform ast-node-data) *class-id*))

(define-6502-statement :string-blt (ast-node-data)
  (compile-6502-string-blt *output-stream* (statement :string-blt ast-node-data) *class-id*))

(define-6502-statement :goto (ast-node-data)
  (compile-6502-goto *output-stream* (statement :goto ast-node-data) *class-id* *method-id*))

(define-6502-statement :paragraph (ast-node-data)
  (compile-6502-paragraph *output-stream* (statement :paragraph ast-node-data) *class-id* *method-id*))

(define-6502-statement :evaluate (ast-node-data)
  (compile-6502-evaluate *output-stream* (statement :evaluate ast-node-data) cpu))

(define-6502-statement :inspect (ast-node-data)
  (compile-6502-inspect *output-stream* (statement :inspect ast-node-data) *class-id*))

(define-6502-statement :copy (ast-node-data)
  (error "EIGHTBOL: COPY ~s should have been expanded at lex time"
         (safe-getf ast-node-data :name)))

(define-6502-statement :invoke-super (ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "~10Tjmp Call~a~a"
          (pascal-case (gethash *class-id* *parent-classes*))
          (pascal-case *method-id*)))

(define-6502-statement :service-bank (ast-node-data)
  (declare (ignore ast-node-data))
  (error "EIGHTBOL: :service-bank is copybook metadata, not a procedure statement (corrupt AST)"))
