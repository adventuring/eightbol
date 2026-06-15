(in-package :eightbol)
(defun emit-6502-cmp-byte-n-of-expression (out expression class-id n w)
  "Compare A to byte N (0-based) of W-byte RHS EXPRESSION (multi-byte = / NOT =).
Mirrors addressing in emit-6502-load-byte-n; CMP does not change A."
  (%emit-6502-alu-byte-n-of-expression out "cmp" expression class-id n w))

(defun emit-6502-adc-byte-n-of-expression (out expression class-id n w)
  "Add-with-carry byte N of W-byte RHS EXPRESSION into A (same addressing as emit-6502-cmp-byte-n-of-expression)."
  (%emit-6502-alu-byte-n-of-expression out "adc" expression class-id n w))

(defun emit-6502-sbc-byte-n-of-expression (out expression class-id n w)
  "Subtract-with-borrow byte N of W-byte RHS EXPRESSION from A (same addressing as emit-6502-cmp-byte-n-of-expression)."
  (%emit-6502-alu-byte-n-of-expression out "sbc" expression class-id n w))

(defun emit-6502-inc-dec-instance-or-bare (out opcode expression class-id)
  "Emit OPCODE (inc or dec) for 1-byte EXPRESSION (implicit slot or absolute label)."
  (declare (ignore class-id))
  (format out "~%~10T~a ~a" opcode (emit-6502-value expression))
  (setf *6502-accumulator-expression* :trash/inc/dec))

;;; Expression emission

(defun emit-6502-value (expression)
  "Return a 64tass expression string for EXPRESSION.
Constants use cobol-constant-to-assembly-symbol. Non-constant bare identifiers use
bare-data-assembly-symbol. Indexed :subscript bases use bare-data-assembly-symbol
so global CartRAM labels match emit-6502-load-byte-n. slot OF obj in address context
emits (slotname) (see branch below), not the full OriginClassSlot label.

@table @asis
@item EXPRESSION
Rvalue expression; NIL signals backend-error (avoids emitting a NIL label).
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
     (let ((value (expression-constant-value expression)))
       (if (numberp value)
	 (format nil "$~2,'0x" value)
	 value)))

    ((eql :self expression) "Self")

    ((eql :null expression)
     (error 'backend-error
	  :message "should not try to assign NULL in this way"
	  :cpu *cpu*
	  :detail expression))

    ((and (listp expression) (member (first expression) '(:subscript :on :of)))
     (error 'backend-error
	  :message (format nil "emit-6502-value: complex access is not a plain address
use emit-6502-load-expression / load-byte-n for ~s" expression)
	  :cpu *cpu*
	  :detail expression))

    ((and (listp expression)
	(stringp (first expression))
	(string= "(" (first expression))
	(stringp (lastcar expression))
	(string= ")" (lastcar expression)))
     (if (= 3 (length expression))
         (emit-6502-value (second expression))
         (format nil "+(~{~a~^ ~})" (mapcar #'emit-6502-value
				    (subseq expression 1 (1- (length expression)))))))

    ((stringp expression)
     (if (gethash expression *working-storage*)
         (to-identifier expression)
         (cobol-global-data-name-to-assembly-symbol expression)))

    (t (error "Unclear how to make assembler understand: ~s" expression))))

(defun expression-literal-zero-p (expression)
  (when (expression-constant-p expression)
    (let ((val (expression-constant-value expression)))
      (and (numberp val) (zerop val)))))

;;; Per-expression-type helpers for expression-constant-p

(defun expression-constant-p-on (expression)
  (declare (ignore expression)) nil)

(defun expression-constant-p-of (expression)
  (declare (ignore expression)) nil)

(defun expression-constant-p-subscript (expression)
  (declare (ignore expression)) nil)

(defun expression-constant-p-address-of (expression)
  (when-let (of-what (gethash (second expression) *working-storage*))
    (not (getf of-what :value))))

(defun expression-constant-p-add (expression)
  (and (expression-constant-p (getf (rest expression) :from))
       (expression-constant-p (getf (rest expression) :to))))

(defun expression-constant-p-subtract (expression)
  (and (expression-constant-p (getf (rest expression) :from))
       (expression-constant-p (getf (rest expression) :subtrahend))))

(defun expression-constant-p-multiply (expression)
  (and (expression-constant-p (getf (rest expression) :multiplier))
       (expression-constant-p (getf (rest expression) :by))))

(defun expression-constant-p-divide (expression)
  (and (expression-constant-p (getf (rest expression) :numerator))
       (expression-constant-p (getf (rest expression) :denominator))))

(defun expression-constant-p-bit-and (expression)
  (and (expression-constant-p (second expression))
       (expression-constant-p (third expression))))

(defun expression-constant-p-bit-or (expression)
  (and (expression-constant-p (second expression))
       (expression-constant-p (third expression))))

(defun expression-constant-p-low (expression)
  (expression-constant-p (second expression)))

(defun expression-constant-p-high (expression)
  (expression-constant-p (second expression)))

(defun expression-constant-p-bit-xor (expression)
  (and (expression-constant-p (second expression))
       (expression-constant-p (third expression))))

(defun expression-constant-p-bit-not (expression)
  (expression-constant-p (second expression)))

(defun expression-constant-p-shift-left (expression)
  (and (expression-constant-p (second expression))
       (expression-constant-p (third expression))))

(defun expression-constant-p-shift-right (expression)
  (and (expression-constant-p (second expression))
       (expression-constant-p (third expression))))

(defparameter *expression-constant-p-handlers*
  (let ((table (make-hash-table)))
    (setf (gethash :on table) #'expression-constant-p-on)
    (setf (gethash :of table) #'expression-constant-p-of)
    (setf (gethash :subscript table) #'expression-constant-p-subscript)
    (setf (gethash :address-of table) #'expression-constant-p-address-of)
    (setf (gethash :add table) #'expression-constant-p-add)
    (setf (gethash :subtract table) #'expression-constant-p-subtract)
    (setf (gethash :multiply table) #'expression-constant-p-multiply)
    (setf (gethash :divide table) #'expression-constant-p-divide)
    (setf (gethash :bit-and table) #'expression-constant-p-bit-and)
    (setf (gethash :bit-or table) #'expression-constant-p-bit-or)
    (setf (gethash :low table) #'expression-constant-p-low)
    (setf (gethash :high table) #'expression-constant-p-high)
    (setf (gethash :bit-xor table) #'expression-constant-p-bit-xor)
    (setf (gethash :bit-not table) #'expression-constant-p-bit-not)
    (setf (gethash :shift-left table) #'expression-constant-p-shift-left)
    (setf (gethash :shift-right table) #'expression-constant-p-shift-right)
    table))

(defun expression-constant-p (expression)
  "True if EXPRESSION is a numeric literal, named constant, or constant expression.
Constant expressions are arithmetic/bit ops whose operands are all constant.

@table @asis
@item EXPRESSION
Any AST fragment.  Robust to atypical shapes: a 1-element list (e.g. parser
wrap @code{((:OF \"State\" \"Self\"))}) is treated as its sole element; lists
whose head is a list, or whose head is not one of the recognised expression
keywords, are non-constant rather than triggering an @code{ecase} failure
or a @code{string=} type error on the head.
@end table"
  (cond
    ((numberp expression) t)
    ((stringp expression)
     (or (when-let (var (gethash expression *working-storage*))
	 (getf var :value))
         (constant-p expression)))
    ((and (listp expression) (eq (first expression) :literal)) t)
    ;; Defensive: 1-element list — unwrap (cl-yacc wraps eval-subject &c.).
    ((and (listp expression) (= 1 (length expression)))
     (expression-constant-p (first expression)))
    ((and (listp expression)
	(stringp (first expression))
	(string= "(" (first expression))
	(stringp (lastcar expression))
	(string= ")" (lastcar expression)))
     (every #'expression-constant-p (subseq expression 1 (1- (length expression)))))
    ((not (listp expression))
     nil)
    ((null (first expression)) nil)
    ;; Defensive: when head is a list or non-keyword, the expression is not constant.
    ((not (keywordp (first expression))) nil)
    (t (let ((handler (gethash (first expression) *expression-constant-p-handlers*)))
         (unless handler
           (error "expression-constant-p: unknown expression type ~s" (first expression)))
         (funcall handler expression)))))

;;; Per-expression-type helpers for expression-constant-value

(defun expression-constant-value-literal (expression)
  (rest expression))

(defun expression-constant-value-address-of (expression)
  (emit-6502-value (second expression)))

(defun expression-constant-value-add (expression)
  (format nil "(~a + ~a)"
          (emit-6502-value (getf (rest expression) :from))
          (emit-6502-value (getf (rest expression) :to))))

(defun expression-constant-value-subtract (expression)
  (format nil "(~a - ~a)"
          (emit-6502-value (getf (rest expression) :from))
          (emit-6502-value (getf (rest expression) :subtrahend))))

(defun expression-constant-value-multiply (expression)
  (format nil "(~a * ~a)"
          (emit-6502-value (getf (rest expression) :multiplier))
          (emit-6502-value (getf (rest expression) :by))))

(defun expression-constant-value-divide (expression)
  (format nil "(~a / ~a)"
          (emit-6502-value (getf (rest expression) :numerator))
          (emit-6502-value (getf (rest expression) :denominator))))

(defun expression-constant-value-bit-or (expression)
  (format nil "(~{~a~^ | ~})"
          (mapcar #'emit-6502-value (rest expression))))

(defun expression-constant-value-bit-and (expression)
  (format nil "(~{~a~^ & ~})"
          (mapcar #'emit-6502-value (rest expression))))

(defun expression-constant-value-bit-xor (expression)
  (format nil "(~{~a~^ ^ ~})"
          (mapcar #'emit-6502-value (rest expression))))

(defun expression-constant-value-shift-left (expression)
  (format nil "(~a << ~a)"
          (emit-6502-value (second expression))
          (emit-6502-value (third expression))))

(defun expression-constant-value-shift-right (expression)
  (format nil "(~a >> ~a)"
          (emit-6502-value (second expression))
          (emit-6502-value (third expression))))

(defparameter *expression-constant-value-handlers*
  (let ((table (make-hash-table)))
    (setf (gethash :literal table) #'expression-constant-value-literal)
    (setf (gethash :address-of table) #'expression-constant-value-address-of)
    (setf (gethash :add table) #'expression-constant-value-add)
    (setf (gethash :subtract table) #'expression-constant-value-subtract)
    (setf (gethash :multiply table) #'expression-constant-value-multiply)
    (setf (gethash :divide table) #'expression-constant-value-divide)
    (setf (gethash :bit-or table) #'expression-constant-value-bit-or)
    (setf (gethash :bit-and table) #'expression-constant-value-bit-and)
    (setf (gethash :bit-xor table) #'expression-constant-value-bit-xor)
    (setf (gethash :shift-left table) #'expression-constant-value-shift-left)
    (setf (gethash :shift-right table) #'expression-constant-value-shift-right)
    table))

(defun expression-constant-value (expression)
  "Return numeric value of constant EXPRESSION. EXPRESSION must satisfy expression-constant-p.
Folds constant expressions (add/subtract/multiply/divide/bit/shift) at compile time."
  (cond
    ((numberp expression) expression)
    ;; Defensive: 1-element list — unwrap (parser wraps eval-subject &c.).
    ((and (listp expression) (= 1 (length expression)))
     (expression-constant-value (first expression)))
    ((and (listp expression)
	(stringp (first expression))
	(string= "(" (first expression))
	(stringp (lastcar expression))
	(string= ")" (lastcar expression)))
     (if (= 3 (length expression))
         (expression-constant-value (second expression))
         (format nil "( ~{~a~^ ~} )"
                 (mapcar #'expression-constant-value
                         (subseq expression 1 (1- (length expression)))))))
    ((and (stringp expression) (constant-p expression))
     (cobol-constant-to-assembly-symbol
      (constant-cobol-name-for-assembly expression)))
    ((stringp expression) (to-identifier expression))
    ((and (listp expression) (eq (first expression) :low))
     (format nil "< ~a" (expression-constant-value (second expression))))
    ((and (listp expression) (eq (first expression) :high))
     (format nil "> ~a" (expression-constant-value (second expression))))
    ((and (listp expression) (or (not (find :giving (rest expression)))
                                 (null (getf (rest expression) :giving))))
     (let ((handler (gethash (first expression) *expression-constant-value-handlers*)))
       (unless handler
         (error "expression-constant-value: unknown expression type ~s" (first expression)))
       (funcall handler expression)))
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

When *6502-family-cpu*  is :6502 (undocumented  opcodes allowed),
object slots  use lax (Self),  y instead of lda  (Self), y
then      tax}.      @var{OUT},     @var{CLASS-ID      as      in
emit-6502-load-expression."
  (cond
    ((and expression (equalp expression *6502-x-index-expression*))
     (format out "~%~10T;; x still valid"))
    ((and expression (equalp expression *6502-accumulator-expression*))
     (format out "~%~10Ttax"))
    ((and (slot-of-expression expression) (6502-use-undocumented-p))
     (let ((so (slot-of-expression expression)))
       (format out "~%~10Tldy # ~a" (apply #'slot-symbol (rest so)))
       (format out "~%~10Tlax (Self), y")
       (setf *6502-accumulator-expression* so
	   *6502-x-index-expression* so)))
    ((numberp expression)
     (format out "~%~10Tldx # $~2,'0x" expression)
     (setf *6502-x-index-expression* expression))
    ((not (listp expression))
     (let ((var (gethash expression *working-storage*)))
       (if-let (value (getf var :value))
         (format out "~%~10Tldx # ~a" (expression-constant-value expression))
         (format out "~%~10Tldx ~a" (to-identifier expression)))
       (setf *6502-x-index-expression* expression)))
    (t
     (emit-6502-load-expression out expression class-id)
     (format out "~%~10Ttax")
     (setf *6502-x-index-expression* expression))))

;;; Load into A register

(defun emit-6502-load-expression (out expression &optional class-id)
  "Emit 6502 code to load EXPRESSION into the A register.
Constants  (numeric  literals  and  named  77/78  items)  use  immediate
mode (#). Constant expressions are folded at compile time and emitted as
one  immediate  load.  Variables  and  slots  use  direct/indexed  mode.
Compound bit/shift  expressions are  computed in-line.  Skips lda
when *6502-accumulator-expression*  is equal to EXPRESSION.  When EXPRESSION
is (:of Name  Self) and NAME is a  77/78 in *const-table*,
treat as bare NAME (immediate lda #), not an instance slot."
  (block nil
    (cond
      ((equalp expression *6502-accumulator-expression*)
       (return))

      ;; Defensive: unwrap a 1-element list (cl-yacc wraps EVALUATE eval-subject
      ;; and WHEN evaluate-phrases as ((:OF ...)) or ("HP")).  Mirrors the
      ;; corresponding unwrap in EMIT-6502-VALUE.
      ((and (listp expression) (= 1 (length expression)))
       (emit-6502-load-expression out (first expression) class-id)
       (return-from emit-6502-load-expression))

      ((and (expression-constant-p expression) (= 1 (expression-operand-width expression)))
       (with-accumulator-value (expression)
         (format out "~%~10Tlda # ~a" (expression-constant-value expression)))
       (return-from emit-6502-load-expression))

      ((and (listp expression) (eq (first expression) :bit-or))
       (with-accumulator-value (expression)
         (emit-6502-load-expression out (second expression))
         (emit-6502-alu-with-memory-rhs out "ora" (third expression) class-id)))

      ((and (listp expression) (eq (first expression) :bit-and))
       (with-accumulator-value (expression)
         (emit-6502-load-expression out (second expression))
         (emit-6502-alu-with-memory-rhs out "and" (third expression) class-id)))

      ((and (listp expression) (eq (first expression) :bit-xor))
       (with-accumulator-value (expression)
         (emit-6502-load-expression out (second expression))
         (emit-6502-alu-with-memory-rhs out "eor" (third expression) class-id)))

      ;; Arithmetic: a + b
      ((and (listp expression) (eq (first expression) :add))
       (with-accumulator-value (expression)
         (emit-6502-load-expression out (getf (rest expression) :to) class-id)
         (format out "~%~10Tclc")
         (emit-6502-alu-with-memory-rhs out "adc" (getf (rest expression) :from) class-id)))

      ;; Arithmetic: a - b — AST is (:SUBTRACT :FROM minuend :SUBTRAHEND subtrahend [:GIVING …]).
      ((and (listp expression) (eq (first expression) :subtract))
       (with-accumulator-value (expression)
         (emit-6502-load-expression out (getf (rest expression) :from) class-id)
         (format out "~%~10Tsec")
         (emit-6502-alu-with-memory-rhs out "sbc"
                                        (getf (rest expression) :subtrahend)
                                        class-id)))

      ;; Arithmetic: a * k (k must be power of 2)
      ((and (listp expression) (eq (first expression) :multiply))
       (let ((e1 (getf (rest expression) :multiplier)) (e2 (getf (rest expression) :by)))
         (cond
	 ((and (expression-constant-p e2)
                 (power-of-two-p (expression-constant-value e2)))
	  (with-accumulator-value (expression)
	    (let ((shift (log2 (expression-constant-value e2))))
                (emit-6502-load-expression out e1 class-id)
                (dotimes (_ shift) (format out "~%~10Tasl a")))))
	 ((and (expression-constant-p e1)
                 (power-of-two-p (expression-constant-value e1)))
	  (with-accumulator-value (expression)
	    (let ((shift (log2 (expression-constant-value e1))))
                (emit-6502-load-expression out e2 class-id)
                (dotimes (_ shift) (format out "~%~10Tasl a")))))
	 (t (error 'backend-error
		 :message "6502: multiply by non-power-of-2 requires software routine"
		 :cpu :6502 :detail (list :multiply e1 e2))))))

      ;; Arithmetic: a / k (k must be power of 2)
      ((and (listp expression) (eq (first expression) :divide))
       (let ((e1 (getf (rest expression) :numerator)) (e2 (getf (rest expression) :denominator)))
         (when (expression-constant-p e2)
	 (let ((shift (log2 (expression-constant-value e2))))
	   (when (and shift (not (zerop shift)) (integerp shift))
	     (with-accumulator-value (expression)
                 (emit-6502-load-expression out e1 class-id)
                 (dotimes (_ shift) (format out "~%~10Tlsr a")))
	     (return-from emit-6502-load-expression))))
         (error 'backend-error
                :message "6502: divide by non-power-of-2 requires software routine"
                :cpu :6502 :detail (list :divide e1 e2))))

      ;; Subscripted array: base(index) — load index into X, then lda base, x
      ((and (listp expression) (eq (first expression) :subscript))
       (emit-6502-load-expression-into-x out (third expression) class-id)
       (with-accumulator-value (expression)
         (format out "~%~10Tlda ~a, x" (emit-6502-value (second expression)))))

      ;; Slot OF Object -- indexed indirect via pointer
      ((slot-of-expression expression)
       (let ((so (slot-of-expression expression)))
         (format out "~%~10Tldy # ~a" (apply #'slot-symbol (rest so)))
         (with-accumulator-value (so)
	 (format out "~%~10Tlda (~a), y" (to-identifier (third so))))))

      ((slot-on-expression expression)
       (let ((so (slot-on-expression expression)))
         (with-accumulator-value (so)
	 (format out "~%~10Tlda ~a + ~a"
	         (to-identifier (third so))
	         (apply #'slot-symbol (rest so))))))

      ;; Shift left — asl A, n times
      ((and (listp expression) (eq (first expression) :shift-left))
       (with-accumulator-value (expression)
         (let ((n (if (numberp (third expression)) (third expression) 1)))
	 (emit-6502-load-expression out (second expression) class-id)
	 (dotimes (_ n) (format out "~%~10Tasl a")))))

      ;; Shift right — lsr A, n times
      ((and (listp expression) (eq (first expression) :shift-right))
       (with-accumulator-value (expression)
         (let ((n (if (numberp (third expression)) (third expression) 1)))
	 (emit-6502-load-expression out (second expression) class-id)
	 (dotimes (_ n) (format out "~%~10Tlsr a")))))

      ;; Bitwise AND (mask — used for bit testing too)
      ((and (listp expression) (eq (first expression) :bit-and))
       (with-accumulator-value (expression)
         (emit-6502-load-expression out (second expression) class-id)
         (emit-6502-alu-with-memory-rhs out "and" (third expression) class-id)))

      ;; Bitwise OR
      ((and (listp expression) (eq (first expression) :bit-or))
       (with-accumulator-value (expression)
         (emit-6502-load-expression out (second expression) class-id)
         (emit-6502-alu-with-memory-rhs out "ora" (third expression) class-id)))

      ;; Bitwise XOR
      ((and (listp expression) (eq (first expression) :bit-xor))
       (with-accumulator-value (expression)
         (emit-6502-load-expression out (second expression) class-id)
         (emit-6502-alu-with-memory-rhs out "eor" (third expression) class-id)))

      ;; Bitwise NOT (complement all bits)
      ((and (listp expression) (eq (first expression) :bit-not))
       (with-accumulator-value (expression)
         (emit-6502-load-expression out (second expression) class-id)
         (format out "~%~10Teor #$ff")))

      ;; Low value
      ((and (listp expression) (eq (first expression) :low))
       (if (and (listp (second expression)) (eql :address-of (first (second expression))))
	 (with-accumulator-value (expression)
	   (format out "~%~10Tlda # <~a" (to-identifier (second (second expression)))))
	 (with-accumulator-value (expression)
	   (format out "~%~10Tlda # <~a" (to-identifier (second expression))))))

      ;; High value
      ((and (listp expression) (eq (first expression) :high))
       (if (and (listp (second expression)) (eql :address-of (first (second expression))))
	 (with-accumulator-value (expression)
	   (format out "~%~10Tlda # >~a" (to-identifier (second (second expression)))))
	 (with-accumulator-value (expression)
	   (format out "~%~10Tlda # >~a" (to-identifier (second expression))))))

      ;; literal expression (constant for assembler to evaluate)
      ((and (listp expression) (eq (first expression) :literal))
       (with-accumulator-value ((second expression))
         (format out "~%~10Tlda ~a" (second expression))))

      ((and (listp expression)
	  (stringp (first expression))
	  (string= "(" (first expression))
	  (stringp (lastcar expression))
	  (string= ")" (lastcar expression)))
       (mapcar (lambda (expr)
                 (emit-6502-load-expression out expr class-id))
	     (subseq expression 1 (1- (length expression)))))

      ;; Numeric literal
      ((numberp expression)
       (with-accumulator-value (expression)
         (format out "~%~10Tlda # $~2,'0x" expression)))

      ;; Bare data name: constants immediate; instance slots via (Self),y; else absolute (bare-data)
      ((stringp expression)
       (if (expression-constant-p expression)
	 (with-accumulator-value (expression)
	   (format out "~%~10Tlda # ~a" (expression-constant-value expression)))
	 (with-accumulator-value (expression)
	   (format out "~%~10Tlda ~a" (to-identifier expression)))))

      (t
       (with-accumulator-value (expression)
         (format out "~%~10Tlda ~a" (emit-6502-value expression)))))))


