;; backend-6502.lisp -- 6502 / 64tass code generation backend
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

;; {Class}Class  INVOKE Var  "M": .CallMethod  Call{Type}M, {Type}Class,
;; Var  Slot access:  ldy  #{OriginClass}{Slot} /  lda  (Self), y  Array
;; access:  ldx index  /  lda base,  x  (X for  subscript;  Y for  slot)
;; Constants (78/77):  lda #ConstName (immediate  addressing) Variables:
;; lda  VarName  (direct/absolute  addressing) Fault:  .LogFault  "code"
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
  (member *6502-family-cpu* '(:65c02 :65c816 :HuC6280)))

(defun 6502-use-undocumented-p ()
  "True if CPU allows undocumented 6502 opcodes (lax, dcp, etc.). Only :6502."
  (eq *6502-family-cpu* :6502))

(defvar *6502-accumulator-expression* :trash/init
  "AST for the value currently in A during 6502-family emission, or NIL if unknown.")
(defvar *6502-x-index-expression* :trash/init
  "AST for the value currently in X during 6502-family emission, or NIL if unknown.")
(defvar *6502-y-index-expression* :trash/init
  "AST for the value currently in Y during 6502-family emission, or NIL if unknown.")
(defvar *6502-accumulator-lru-time* 0
  "Last time the accumulator register was used (for LRU tracking).")
(defvar *6502-x-lru-time* 0
  "Last time the X register was used (for LRU tracking).")
(defvar *6502-y-lru-time* 0
  "Last time the Y register was used (for LRU tracking).")
(defvar *6502-lru-counter* 0
  "Global counter for LRU timestamping.")

(defun touch-register (reg)
  "Update LRU time for REG (:a :x :y) and increment counter."
  (incf *6502-lru-counter*)
  (case reg
    (:a (setf *6502-accumulator-lru-time* *6502-lru-counter*))
    (:x (setf *6502-x-lru-time* *6502-lru-counter*))
    (:y (setf *6502-y-lru-time* *6502-lru-counter*))))

(defvar *6502-pic-scale-seq* 0
  "Incremented for unique @code{PicS*} scratch labels when emitting PIC implied-decimal scaling.")

(defvar *output-stream* *standard-output*
  "Stream for 6502 family assembly output; rebound during compilation.")

(defun %6502-subtract-2byte-inplace-eligible-p (from from-target result
                                                giving class-id w bcd-p)
  "True  when   SUBTRACT  can   use  one   ldy  to   the  low   slot  byte,
sta/iny/lda/sbc/sta for high byte (no tax/dey)."
  (declare (ignore class-id))
  (and (= w 2)
       (not bcd-p)
       (null giving)
       (equal from-target result)
       (slot-of-self-p result)
       (not (expression-contains-subscript-p from-target))
       (not (expression-contains-subscript-p from))
       (not (expression-contains-subscript-p result))))

(defun to-identifier (token)
  (let ((token (string token)))
    (if (search "--" token)
        (format nil "~{~a~^_~}" (mapcar (lambda (seg)
                                          (if (string-equal seg "ID")
                                              "ID"
                                              (pascal-case seg)))
                                        (cl-ppcre:split  "--" token)))
        (pascal-case token))))

(defmacro with-accumulator-value ((value) &body body)
  (let ((v (gensym "VALUE-"))
        (cv (gensym "CONST-VALUE-")))
    `(let ((,v ,value))
       (cond
         ((equalp ,v *6502-accumulator-expression*)
          (format out "~%~10T;; accumulator still valid"))
         ((equalp ,v *6502-x-index-expression*)
          (format out "~%~10Ttxa"))
         (t (prog1
                (progn ,@body)
              (setf *6502-accumulator-expression*
                    (if (and (expression-constant-p ,v)
                             (let ((,cv (expression-constant-value ,v)))
                               (and (numberp ,cv) (zerop ,cv))))
                        0
                        ,v))))))))

(defun emit-6502-subtract-2byte-self-inplace (out from result
                                              class-id bcd-p)
  "Emit 16-bit SUBTRACT FROM from RESULT (same slot), little-endian, in place."
  (declare (ignore class-id))
  (when bcd-p
    (error 'backend-error
           :message "EIGHTBOL/6502: BCD 2-byte in-place SUBTRACT not implemented"
           :cpu :6502
           :detail (list :subtract-inplace from result)))
  (let ((offset (apply #'slot-symbol (rest (slot-of-expression result)))))
    (format out "~%~10Tldy # ~a" offset)
    (format out "~%~10Tsec")
    (with-accumulator-value ((slot-of-expression result))
      (format out "~%~10Tlda (~a), y" (to-identifier (third (slot-of-expression result)))))
    (if (expression-constant-p from)
        (format out "~%~10Tsbc # <~a" (expression-constant-value from))
        (format out "~%~10Tsbc ~a" (emit-6502-value from)))
    (format out "~%~10Tsta (~a), y" (to-identifier (third (slot-of-expression result))))
    (format out "~%~10Tiny")
    (with-accumulator-value ((slot-of-expression result))
      (format out "~%~10Tlda (~a), y" (to-identifier (third (slot-of-expression result)))))
    (if (expression-constant-p from)
        (format out "~%~10Tsbc # >~d" (expression-constant-value from))
        (format out "~%~10Tsbc ~a + 1" (emit-6502-value from)))
    (format out "~%~10Tsta (~a), y" (to-identifier (third (slot-of-expression result)))))
  (setf *6502-accumulator-expression* :trash/subtraction))

(defun emit-6502-store-zero (out addr)
  "Emit code to store zero to memory at ADDR.

Uses stz for 65c02+, lda # 0 + sta for 6502/RP2A03."
  (if (6502-has-stz-p)
      (progn
        (format out "~%~10Tstz ~a" addr))
      (progn
        (format out "~%~10Tldy # 0")
        (format out "~%~10Tsty ~a" addr))))

;;; Top-level entry point

(defun method-statements-list (method)
  "Return METHOD's :statements as a list with NIL placeholders removed.
Parser or optimization must not leave junk entries, but filtering keeps shape predicates robust."
  (remove nil (ensure-list (safe-getf (rest method) :statements))))

(defun method-blank-p (method)
  "True if METHOD has no statements (blank procedure division).

@itemize @bullet
@item
:statements is absent or nil.
@end itemize"
  (null (nth-value 1 (split-method-leading-assembly-entry
                      (getf (rest method) :statements)))))

(defun method-trivial-return-only-p (method)
  "True   if   METHOD   has   exactly    one   statement   that   is   only
a  return  (:goback,  :exit-method,  :exit-program,
:exit, :stop-run).

@itemize @bullet
@item
METHOD — a :method AST node.
@end itemize"
  (let ((statements (nth-value 1 (split-method-leading-assembly-entry
                                  (getf (rest method) :statements)))))
    (and statements
         (= (length statements) 1)
         (let ((s (first statements)))
           (and (listp s)
                (not (null (member (first s)
                                   '(:goback :exit-method :exit-program
                                     :exit :stop-run :comment)))))))))

(defun method-true-method-alias-p (method)
  "True if METHOD should be emitted as MethodClassM = TrueMethod (no .block).

Blank   methods  and   single-statement   GOBACK/EXIT*/STOP  RUN   match
Phantasia's hand-written stubs.

@itemize @bullet
@item
METHOD — a :method AST node.
@end itemize"
  (when (nth-value 0 (split-method-leading-assembly-entry
                      (getf (rest method) :statements)))
    (return-from method-true-method-alias-p nil))
  (or (method-blank-p method)
      (method-trivial-return-only-p method)))

(defun method-last-statement-6502-no-trailing-rts-p (last-statement)
  "True     if    LAST-STATEMENT     already     ends     control    flow     so
compile-6502-method must not emit a trailing rts.

Covers  returns   and  tail  CALL  that   emits  jmp  when
:tail-call-p is  set (e.g.  CALL …  GOBACK). INVOKE
uses .CallMethod (jsr DoCallMethod),  so the method always
needs  a trailing  rts. Far  CALL never  uses a  bare tail
jmp.

@itemize @bullet
@item
LAST-STATEMENT — last statement in a method’s :statements list, or NIL.
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

(defun emit-one-6502-family-method (output-stream class-id method
                                    cpu compile-method-fn)
  "Emit one method: MethodClassM = TrueMethod, or a .block body from COMPILE-METHOD-FN.

COMPILE-METHOD-FN  must accept  (METHOD  CLASS-ID  CPU) and  emit
a    .block    body     (e.g.    compile-6502-method    or
compile-rp2a03-method). CPU  selects opcodes for the  shared 6502
statement helpers.

@table @asis
@item OUTPUT-STREAM
Assembly destination.
@item CLASS-ID
Compiling class id string (e.g. \"Character\").
@item METHOD
:method AST plist.
@item CPU
Target keyword (:6502}, :rp2a03, @dots{).
@item COMPILE-METHOD-FN
Function of three arguments used when the method is not a TrueMethod alias.
@end table"
  (cond
    ((method-true-method-alias-p method)
     (format output-stream "~2%Method~a~a = TrueMethod"
             (to-identifier class-id) (to-identifier (safe-getf (rest method) :method-id))))
    (t (funcall compile-method-fn method class-id cpu))))

(defun compile-6502-family (ast output-stream cpu)
  "Shared code generation for 6502-family CPUs (6502, 65c02, 65c816, HuC6280).

CPU    is    the    keyword    (:6502    :65c02    :65c816    :huc6280).
Binds  *6502-family-cpu*  for  opcode selection.  Trivial  methods  emit
MethodClassM    =    TrueMethod;    INVOKE    Self    uses
.CallMethod in the method body.
When the AST has :program-id but no :class-id, emits a standalone subroutine
instead of a class with methods."
  (let ((*6502-family-cpu* cpu)
        (cpu-label (cpu-display-name cpu))
        (*standard-output* output-stream))
    (unless (and (listp ast) (eq (first ast) :program))
      (error "EIGHTBOL/~a: expected :program AST node, got ~s" cpu-label (first ast)))
    (let ((*class-id* (safe-getf (rest ast) :class-id))
          (methods (safe-getf (rest ast) :methods))
          (program-id (safe-getf (rest ast) :program-id)))
      (cond
        ((and *class-id* methods)
         ;; Full class with methods — emit each as a separate .block
         (format output-stream
                 ";;; ~a — generated by EIGHTBOL for ~a~%~{~%;;; ~a: ~a~}"
                 *class-id* cpu-label
                 (getf (rest ast) :identification))
         (finish-output output-stream)
         (dolist (method (ensure-list methods))
           (when (and (listp method) (eq (first method) :method))
             (format output-stream "~2%;;; Method ~a # ~a"
                     *class-id* (getf (rest method) :method-id))
             (emit-one-6502-family-method
              output-stream *class-id* method cpu #'compile-6502-method)
             (format output-stream "~2%;;; End of method ~a # ~a" *class-id*
                     (getf (rest method) :method-id))
             (finish-output output-stream))))
        (program-id
         ;; Standalone subroutine (PROGRAM-ID)
         (let* ((label (to-identifier program-id))
                (statements (safe-getf (rest ast) :statements))
                (last-statement (car (last statements))))
           (format output-stream
                   ";;; ~a — generated by EIGHTBOL for ~a~%"
                   label cpu-label)
           (format output-stream "~%~a: .block" label)
           (let ((*6502-accumulator-expression* :trash/method-top)
                 (*6502-x-index-expression* :trash/method-top))
             (dolist (statement statements)
               (cond
                 ((null statement))
                 ((not (listp statement))
                  (error "malformed procedure statement (expected list): ~s" statement))
                 ((first statement)
                  (compile-statement cpu (first statement) (rest statement))))))
           (unless (method-last-statement-6502-no-trailing-rts-p last-statement)
             (format output-stream "~%~10Trts~%"))
           (format output-stream "~%~10T.bend")))
        ;; non-class, non-program AST (e.g. COPY book data) — emit nothing
        (t (format output-stream "~%;; (Nothing to see here)")))
      (finish-output output-stream))))

(defmethod compile-to-assembly (ast (cpu (eql :6502)) output-stream)
  (compile-6502-family ast output-stream :6502))

;;; Method compilation

(defun compile-6502-method (method class-id cpu)
  "Emit one METHOD for CLASS-ID using CPU variant keyword.

Binds  *6502-family-cpu*   to  CPU  so  opcode   selection  (e.g.
lax vs lda/tax) matches standalone compilation, not
only compile-6502-family.

When the method's statements begin with @code{(:assembly-entry :label …)},
the @code{.block} opens on that label (for external @code{jsr}), and
@code{MethodClassMethod = …} is emitted after @code{.bend} so dispatch
still matches @code{Method~a~a}.

@table @asis
@item METHOD
:method AST node.
@item CLASS-ID
Compiling class string (e.g. \"Character\").
@item CPU
:6502, :65c02, :65c816, or :huc6280.
@end table"
  (let* ((*6502-family-cpu* cpu)
         (*method-id* (safe-getf (rest method) :method-id))
         (method-dispatch-suffix (to-identifier *method-id*))
         (dispatch-label (format nil "Method~a~a"
                                 (to-identifier class-id)
                                 method-dispatch-suffix))
         (custom-entry (nth-value 0 (split-method-leading-assembly-entry
			       (getf (rest method) :statements))))
         (block-label (if custom-entry
		      (to-identifier custom-entry)
		      dispatch-label))
         (statements (nth-value 1 (split-method-leading-assembly-entry
			     (getf (rest method) :statements))))
         (last-statement (car (last statements))))
    (format *standard-output* "~%~a: .block" block-label)
    (let ((*6502-accumulator-expression* :trash/method-top)
	(*6502-x-index-expression* :trash/method-top))
      (dolist (statement statements)
        (cond
	((null statement))
	((not (listp statement))
	 (error "malformed procedure statement (expected list): ~s" statement))
	((first statement)
	 (compile-statement cpu (first statement) (rest statement))))))
    ;; Fall-through only: returns and tail jmp/jsr paths that never reach here.
    (unless (method-last-statement-6502-no-trailing-rts-p last-statement)
      (format *standard-output* "~%~10Trts~%"))
    (format *standard-output* "~%~10T.bend")
    (when custom-entry
      (format *standard-output* "~%~a = ~a" dispatch-label block-label))))

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
  (when (and (listp expression)
	   (symbolp (first expression))
	   (string-equal (symbol-name (first expression)) "OF"))
    expression))

(defun slot-on-expression (expression)
  "Return (:on slot obj) form for EXPRESSION, or NIL."
  (when (and (listp expression)
	   (symbolp (first expression))
	   (string-equal (symbol-name (first expression)) "ON"))
    expression))

(defun 6502-object-pointer-label (obj-expression class-id)
  "Return 64tass label for object pointer OBJ-EXPRESSION (ZP pair), e.g. Current-Actor → CurrentActor.

@table @asis
@item OBJ-EXPRESSION
:self / Self / string data name.
@item CLASS-ID
Ignored (reserved for typed object refs).
@end table

@subsection Outputs
String usable as lda (Label), y base."
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
  "Emit MNEMONIC (adc, sbc, and, ora, eor) with RHS EXPRESSION

A holds the other operand."
  (cond
    ((expression-constant-p expression)
     (format out "~%~10T~a # ~a" mnemonic (expression-constant-value expression)))

    ((and (listp expression) (eql :subscript (first expression)))
     (let* ((pointer (6502-object-pointer-label (second expression) class-id))
	  (offset (third expression)))
       (emit-6502-load-expression-into-x out offset class-id)
       (format out "~%~10T~a ~a, x" mnemonic pointer)))

    ((slot-of-expression expression)
     (let* ((slot-of-expression (slot-of-expression expression))
	  (offset (apply #'slot-symbol (rest slot-of-expression)))
	  (pointer (6502-object-pointer-label (third slot-of-expression) class-id)))
       (format out "~%~10Tldy # ~a" offset)
       (format out "~%~10T~a (~a), y" mnemonic pointer)))

    ((slot-on-expression expression)
     (let* ((slot-on-expression (slot-on-expression expression))
	  (offset (apply #'slot-symbol (rest slot-on-expression)))
	  (pointer (6502-object-pointer-label (third slot-on-expression)
                                                class-id)))
       (format out "~%~10T~a ~a + ~a" mnemonic pointer offset)))

    ((and (listp expression) (member (first expression)
			       '(:bit-and :bit-or :bit-xor
			         :add :subtract :multiply :divide
			         :shift-left :shift-right)))
     (format out "~%~10Tpha")
     (emit-6502-load-expression out expression class-id)
     (format out "~%~10Tsta WorkALU")
     (format out "~%~10Tpla")
     (format out "~%~10T~a WorkALU" mnemonic))

    (t
     (format out "~%~10T~a ~a" mnemonic (emit-6502-value expression))))
  (setf *6502-accumulator-expression* :trash/alu))

(defun emit-6502-cmp-memory-rhs (out expression class-id)
  "Emit cmp for single-byte RHS EXPRESSION after A holds the left side.
Explicit   Max-HP   OF  Self   and   bare   instance  slots   use
ldy/cmp (Self),y, not cmp (Max-HP)."
  (emit-6502-alu-with-memory-rhs out "cmp" expression class-id))

(defun %emit-6502-alu-byte-n-of-expression (out mnemonic expression
				    class-id n w)
  "Emit  MNEMONIC (cmp,  adc,  or sbc)  on  byte N  of
W-byte EXPRESSION. A  holds the accumulated other operand  (same addressing as
emit-6502-load-byte-n / emit-6502-cmp-byte-n-of-expression)."
  (declare (type string mnemonic))
  (when (>= n w)
    (error "%emit-6502-alu-byte-n-of-expression: n ~d >= w ~d" n w))
  (cond
    ((expression-constant-p expression)
     (if (and (= w 1) (= n 0))
         (format out "~%~10T~a # ~a" mnemonic (expression-constant-value expression))
         (format out "~%~10T~a # $ff & ~a~[~:;~:* >> ~d~]"
                 mnemonic (expression-constant-value expression) (* 8 n))))

    ((and (listp expression) (eq :subscript (first expression)))
     (format out "~%~10Tpha")
     (emit-6502-load-expression-into-x out (third expression) class-id)
     (format out "~%~10Tpla")
     (format out "~%~10T~a ~a + ~d, x" mnemonic (second expression) n))

    ((slot-of-expression expression)
     (let* ((slot-of-expression (slot-of-expression expression))
	  (offset (apply #'slot-symbol (rest slot-of-expression)))
	  (pointer (6502-object-pointer-label (third slot-of-expression) class-id)))
       (format out "~%~10Tldy # ~a~[~:;~:* + ~d~]" offset n)
       (format out "~%~10T~a (~a), y" mnemonic pointer)))

    ((slot-on-expression expression)
     (let* ((slot-on-expression (slot-on-expression expression))
	  (offset (apply #'slot-symbol (rest slot-on-expression)))
	  (pointer (6502-object-pointer-label (third slot-on-expression) class-id)))
       (format out "~%~10T~a ~a + ~a~[~:;~:* + ~d~]" mnemonic pointer offset n)))

    ((stringp expression)
     (format out "~%~10T~a ~a~[~:;~:* + ~d~]"
	   mnemonic (emit-6502-value expression) n))
    (t
     (format out "~%~10T~a ~a~[~:;~:* + ~d~]"
	   mnemonic (emit-6502-value expression) n))))


