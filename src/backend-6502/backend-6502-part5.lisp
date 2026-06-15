(in-package :eightbol)
(defun emit-6502-condition (out condition class-id branch-label)
  "Emit 6502 code to evaluate CONDITION and branch to BRANCH-LABEL if false."
  (assert (listp condition))
  (let* ((condition (or (normalize-relation-condition condition)
                        condition))
         (op (relation-op-canonical (first condition))))
    (cond

      ;; 3-element comparison: op as string or symbol (princ-to-string covers both)
      ((and (= (length condition) 3)
	  (member op '(> < ≥ ≤ = ≠) :test #'string=))
       (let ((lhs (second condition))
	   (rhs (third condition)))
         (if (and (string= op "≥") (expression-literal-zero-p rhs))
	   (emit-6502-false-when-not-unsigned-greater-than-zero
	    out lhs class-id branch-label)
	   (progn
	     (emit-6502-compare out lhs rhs class-id)
	     (cond ((string= op ">")
		  (format out "~%~10Tblt ~a~%" branch-label)
		  (format out "~%~10Tbeq ~a~%" branch-label))
		 ((string= op "<")
		  (format out "~%~10Tbge ~a~%" branch-label))
		 ((string= op "≥")
		  (format out "~%~10Tblt ~a~%" branch-label))
		 ((string= op "≤")
		  (let ((label-stay (new-6502-label "LeStay")))
                        (format out "~%~10Tbeq ~a~%" label-stay)
                        (format out "~%~10Tbge ~a~%" branch-label)
                        (format out "~%~a:" label-stay)
                        (setf *6502-accumulator-expression* :trash/≤
			*6502-x-index-expression* :trash/≤)))
		 ((string= op "≠")
		  (format out "~%~10tbeq ~a~%" branch-label))
		 ((string= op "=")
		  (format out "~%~10tbne ~a~%" branch-label)))))))

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
         (format out "~%~10Tbeq ~a~%" branch-label)))

      ;; Negated zero test (IS NOT ZERO / NOT EQUAL TO 0)
      ((and (string-equal op :not)
	  (listp (second condition))
	  (member (first (second condition)) '(= equal) :test #'string-equal))
       (let ((inner (second condition)))
         (cond ((or (expression-literal-zero-p (second inner))
		(expression-literal-zero-p (third inner)))
                (let ((var (if (expression-literal-zero-p (second inner))
			 (third inner)
			 (second inner))))
	        (emit-6502-branch-if-expression-all-zero out var class-id branch-label)))
	     ((or (eql :null (second inner))
		(eql :null (third inner)))
                (let ((var (if (eql :null (second inner))
			 (third inner)
			 (second inner))))
	        (emit-6502-load-expression out (list :subscript var 1))
	        (format out "~%~10Tbeq ~a~%" branch-label)))
	     ;; else the compare does not have a literal zero on either side
	     (t (let* ((left (second inner))
                         (right (third inner))
                         (w (min (operand-width left) (operand-width right)))
		     (label-done (new-6502-label "NotEqDone")))
		(dotimes (i w)
		  (emit-6502-load-byte-n out left class-id i w)
		  (if (expression-constant-p right)
		      (unless (6502-zero-p right)
		        (format out "~%~10Tcmp # $ff & ( ~a >> ~d )"
			      (expression-constant-value right) (* 8 i)))
		      (emit-6502-cmp-byte-n-of-expression out right class-id i w))
		  ;; Any mismatch means NOT (= ...) is true, so skip false-branch.
		  (format out "~%~10Tbne ~a~%" label-done))
		;; Any remaining, higher bytes must be zeroes
		(let ((rem (- (operand-width left) (operand-width right)))
		      (bigger left))
		  (unless (zerop rem)
		    (when (minusp rem) (setf bigger right))
		    (dotimes (i rem)
		      (emit-6502-load-byte-n out bigger class-id (+ i w) (+ rem w))
		      ;; and higher non-zero byte is not equal, ergo ≠ is true, so skip to label-done.
		      (format out "~%~10Tbne ~a~%" label-done))))
		;; All bytes equal ⇒ condition false.
		(format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) branch-label)
		(format out "~%~a:" label-done)
		(setf *6502-accumulator-expression* :trash/≠0
		      *6502-x-index-expression* :trash/≠0))))))

      ;; AND — short-circuit: if first is false, branch; else evaluate second
      ((eql :and op)
       (emit-6502-condition out (second condition) class-id branch-label)
       (emit-6502-condition out (third condition) class-id branch-label))

      ;; OR — short-circuit: if first is true, skip to success; else evaluate second
      ((eql :or op)
       (let ((label-cond2 (new-6502-label "OrCond2"))
	   (label-skip (new-6502-label "OrSkip")))
         (emit-6502-condition out (second condition) class-id label-cond2)
         (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-skip)
         (format out "~%~a:" label-cond2)
         (setf *6502-accumulator-expression* :trash/or-
	     *6502-x-index-expression* :trash/or-)
         (emit-6502-condition out (third condition) class-id branch-label)
         (format out "~%~a:" label-skip)
         (setf *6502-accumulator-expression* :trash/or+
	     *6502-x-index-expression* :trash/or+)))

      ((eq op :null)
       (emit-6502-load-byte-n out (second condition) class-id 1 2)
       (format out "~%~10Tbne ~a~%" branch-label))

      ((eq op :not-null)
       (emit-6502-load-byte-n out (second condition) class-id 1 2)
       (format out "~%~10Tbeq ~a~%" branch-label))

      ;; Generic fallback
      (t (error "Unhandled comparison operation ~s" condition)))))

(defun emit-6502-compare-unsigned (out lhs rhs class-id w)
  "Emit unsigned compare of LHS vs RHS for W-byte values (little-endian in memory).

Unsigned semantics: treat each W-byte value as an integer; compare from the most significant
stored byte (index W-1) to least (index 0). If any byte differs, bne skips
remaining compares; the last cmp (low byte) only runs when all higher bytes were equal.
Last cmp leaves flags for emit-6502-condition (blt/bge/beq on 6502)."
  (let ((label-done (new-6502-label "CmpU")))
    (when (> w 1)
      (loop for i from (1- w) downto 1 do
        (emit-6502-load-byte-n out lhs class-id i w)
        (emit-6502-cmp-byte-n-of-expression out rhs class-id i w)
        (format out "~%~10Tbne ~a~%" label-done)))
    (emit-6502-load-byte-n out lhs class-id 0 w)
    (unless (6502-zero-p rhs)
      (emit-6502-cmp-byte-n-of-expression out rhs class-id 0 w))
    (format out "~%~a:" label-done)
    (setf *6502-accumulator-expression* :trash/cmp.u
	*6502-x-index-expression* :trash/cmp.u)))

(defun emit-6502-compare (out lhs rhs class-id)
  "Emit unsigned compare LHS vs RHS for relational IF branches.

When max operand-width of LHS and RHS is 1: lda LHS then cmp RHS.
When width is 2 or more: emit-6502-compare-unsigned (high byte first, then low).

@table @asis
@item LHS, RHS
Expressions; widths from *pic-width-table*.
@item CLASS-ID
Current class (slot labels).
@end table"
  (cond
    ((and (not (operand-signed-p lhs))
	(not (operand-signed-p rhs)))
     (emit-6502-compare-unsigned out lhs rhs class-id (max (expression-operand-width lhs)
					         (expression-operand-width rhs))))
    (t (error "Signed logic not implemented"))))

(defun emit-6502-pic-scratch-pads (out w-work buf1 buf2 cmem skip)
  "Emit @code{jmp} over zero-initialized scratch for PIC decimal widen-then-add/sub."
  (format out "~%~10Tjmp ~a" skip)
  (format out "~%~a .fill ~d, 0" buf1 w-work)
  (format out "~%~a .fill ~d, 0" buf2 w-work)
  (format out "~%~a .byte 0" cmem)
  (format out "~%~a:" skip))

(defun emit-6502-pic-copy-to-buffer (out expr class-id w-expr buf w-work)
  "Copy W-EXPR bytes of EXPR into BUF[0..], zero-fill through W-WORK-1."
  (dotimes (i w-expr)
    (emit-6502-load-byte-n out expr class-id i w-expr)
    (format out "~%~10Tsta ~a+~d" buf i))
  (when (< w-expr w-work)
    (format out "~%~10Tlda #0")
    (loop for i from w-expr below w-work
	do (format out "~%~10Tsta ~a+~d" buf i))))

(defun emit-6502-pic-mul-le-buffer-by-10-once (out buf w cmem)
  "In-place LE unsigned multiply: BUF[0..W-1] *= 10. CMEM holds 8-bit carry between limbs."
  (format out "~%~10Tlda #0~%~10Tsta ~a" cmem)
  (dotimes (i w)
    (let* ((uid (incf *6502-pic-scale-seq*))
	 (tb (format nil "PicTb~d" uid))
	 (lo (format nil "PicLo~d" uid))
	 (hi (format nil "PicHi~d" uid))
	 (lp (format nil "PicLp~d" uid)))
      (format out "~%~10Tlda ~a+~d" buf i)
      (format out "~%~10Tsta ~a" tb)
      (format out "~%~10Tlda #0~%~10Tsta ~a~%~10Tsta ~a" lo hi)
      (format out "~%~10Tldx #10")
      (format out "~%~a:" lp)
      (format out "~%~10Tclc~%~10Tlda ~a~%~10Tadc ~a~%~10Tsta ~a" lo tb lo)
      (format out "~%~10Tlda ~a~%~10Tadc #0~%~10Tsta ~a" hi hi)
      (format out "~%~10Tdex~%~10Tbne ~a" lp)
      (format out "~%~10Tclc~%~10Tlda ~a~%~10Tadc ~a~%~10Tsta ~a+~d" lo cmem buf i)
      (format out "~%~10Tlda ~a~%~10Tadc #0~%~10Tsta ~a" hi cmem))))

(defun emit-6502-pic-add-le-buffers (out buf1 buf2 w)
  "LE unsigned add BUF2 into BUF1 for W bytes."
  (format out "~%~10Tclc")
  (dotimes (i w)
    (format out "~%~10Tlda ~a+~d" buf1 i)
    (format out "~%~10Tadc ~a+~d" buf2 i)
    (format out "~%~10Tsta ~a+~d" buf1 i)))

(defun emit-6502-pic-sub-le-buffers (out buf1 buf2 w)
  "LE unsigned subtract BUF2 from BUF1 for W bytes."
  (format out "~%~10Tsec")
  (dotimes (i w)
    (format out "~%~10Tlda ~a+~d" buf1 i)
    (format out "~%~10Tsbc ~a+~d" buf2 i)
    (format out "~%~10Tsta ~a+~d" buf1 i)))

(defun compile-6502-add-pic-decimal-scaled-binary (out statement class-id)
  "ADD with mismatched implied decimal (PIC @code{V}/@code{.}): widen binary operands by @code{* 10^n}, add, store.

Supports @code{GIVING} (result fractional scale @code{>=} both operands) and @code{ADD @dots{} TO} when the
target scale is @code{>=} the addend (widen addend only). USAGE DECIMAL (packed) is not implemented here."
  (let* ((from (safe-getf (rest statement) :from))
         (to-op (safe-getf (rest statement) :to))
         (giving (safe-getf (rest statement) :giving))
         (result (or giving to-op))
         (dr (%operand-pic-fractional-decimal-digits (if giving (or giving to-op) to-op)))
         (df (%operand-pic-fractional-decimal-digits from))
         (dt (%operand-pic-fractional-decimal-digits to-op))
         (w-from (expression-operand-width from))
         (w-to (operand-width to-op))
         (w-res (operand-width result))
         (w-work (min 16 (+ 2 (max w-from w-to w-res)))))
    (when (or (operand-bcd-p from) (operand-bcd-p to-op) (operand-bcd-p result))
      (error 'backend-error
	   :message "EIGHTBOL/6502: misaligned PIC decimal scaling with USAGE DECIMAL is not yet implemented"
	   :cpu :6502
	   :detail (list :giving giving :from from :to to-op)))
    (unless (pic-decimal-binary-add-scaling-supported-p giving from to-op)
      (error 'backend-error
	   :message "EIGHTBOL/6502: FIXME ADD misaligned PIC needs narrowing (divide by 10^n) which is trivial to implement as four bits shifted"
	   :cpu :6502
	   :detail (list :giving giving :from from :to to-op)))
    (let* ((id (incf *6502-pic-scale-seq*))
	 (buf1 (format nil "PicS1_~d" id))
	 (buf2 (format nil "PicS2_~d" id))
	 (cmem (format nil "PicCm_~d" id))
	 (skip (format nil "PicSk_~d" id)))
      (format out "~%~10T;; PIC implied-decimal scaled ADD (binary)")
      (emit-6502-pic-scratch-pads out w-work buf1 buf2 cmem skip)
      (if giving
	(progn
	  (emit-6502-pic-copy-to-buffer out from class-id w-from buf1 w-work)
	  (dotimes (_ (- dr df))
	    (emit-6502-pic-mul-le-buffer-by-10-once out buf1 w-work cmem))
	  (emit-6502-pic-copy-to-buffer out to-op class-id w-to buf2 w-work)
	  (dotimes (_ (- dr dt))
	    (emit-6502-pic-mul-le-buffer-by-10-once out buf2 w-work cmem))
	  (emit-6502-pic-add-le-buffers out buf1 buf2 w-work)
	  (dotimes (i w-res)
	    (format out "~%~10Tlda ~a+~d" buf1 i)
	    (emit-6502-store-byte-n out result class-id i w-res)))
	(progn
	  (emit-6502-pic-copy-to-buffer out to-op class-id w-to buf1 w-work)
	  (emit-6502-pic-copy-to-buffer out from class-id w-from buf2 w-work)
	  (dotimes (_ (- dt df))
	    (emit-6502-pic-mul-le-buffer-by-10-once out buf2 w-work cmem))
	  (emit-6502-pic-add-le-buffers out buf1 buf2 w-work)
	  (dotimes (i w-to)
	    (format out "~%~10Tlda ~a+~d" buf1 i)
	    (emit-6502-store-byte-n out to-op class-id i w-to)))))
    (setf *6502-accumulator-expression* :trash/pic-scale)))

(defun compile-6502-subtract-pic-decimal-scaled-binary (out statement class-id)
  "SUBTRACT with mismatched implied decimal: widen binary operands, subtract, store."
  (multiple-value-bind (minuend subtrahend)
      (subtract-statement-minuend-and-subtrahend statement)
    (let* ((giving (safe-getf (rest statement) :giving))
	 (result (or giving minuend))
	 (dr (%operand-pic-fractional-decimal-digits (if giving (or giving minuend) minuend)))
	 (dm (%operand-pic-fractional-decimal-digits minuend))
	 (ds (%operand-pic-fractional-decimal-digits subtrahend))
	 (w-min (max (operand-width minuend) (expression-operand-width minuend)))
	 (w-sub (max (operand-width subtrahend) (expression-operand-width subtrahend)))
	 (w-res (operand-width result))
	 (w-work (min 16 (+ 2 (max w-min w-sub w-res)))))
      (when (or (operand-bcd-p minuend) (operand-bcd-p subtrahend) (operand-bcd-p result))
        (error 'backend-error
	     :message "EIGHTBOL/6502: misaligned PIC decimal scaling with USAGE DECIMAL is not yet implemented"
	     :cpu :6502
	     :detail (list :giving giving :minuend minuend :subtrahend subtrahend)))
      (unless (pic-decimal-binary-subtract-scaling-supported-p giving minuend subtrahend)
        (error 'backend-error
	     :message "EIGHTBOL/6502: SUBTRACT misaligned PIC needs narrowing (divide by 10^n) which is not yet implemented"
	     :cpu :6502
	     :detail (list :giving giving :minuend minuend :subtrahend subtrahend)))
      (let* ((id (incf *6502-pic-scale-seq*))
	   (buf1 (format nil "PicS1_~d" id))
	   (buf2 (format nil "PicS2_~d" id))
	   (cmem (format nil "PicCm_~d" id))
	   (skip (format nil "PicSk_~d" id)))
        (format out "~%~10T;; PIC implied-decimal scaled SUBTRACT (binary)")
        (emit-6502-pic-scratch-pads out w-work buf1 buf2 cmem skip)
        (if giving
	  (progn
	    (emit-6502-pic-copy-to-buffer out minuend class-id w-min buf1 w-work)
	    (dotimes (_ (- dr dm))
                (emit-6502-pic-mul-le-buffer-by-10-once out buf1 w-work cmem))
	    (emit-6502-pic-copy-to-buffer out subtrahend class-id w-sub buf2 w-work)
	    (dotimes (_ (- dr ds))
                (emit-6502-pic-mul-le-buffer-by-10-once out buf2 w-work cmem))
	    (emit-6502-pic-sub-le-buffers out buf1 buf2 w-work)
	    (dotimes (i w-res)
                (format out "~%~10Tlda ~a+~d" buf1 i)
                (emit-6502-store-byte-n out result class-id i w-res)))
	  (progn
	    (emit-6502-pic-copy-to-buffer out minuend class-id w-min buf1 w-work)
	    (emit-6502-pic-copy-to-buffer out subtrahend class-id w-sub buf2 w-work)
	    (dotimes (_ (- dm ds))
                (emit-6502-pic-mul-le-buffer-by-10-once out buf2 w-work cmem))
	    (emit-6502-pic-sub-le-buffers out buf1 buf2 w-work)
	    (dotimes (i w-min)
                (format out "~%~10Tlda ~a+~d" buf1 i)
                (emit-6502-store-byte-n out minuend class-id i w-min)))))
      (setf *6502-accumulator-expression* :trash/pic-scale))))

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
    (when (add-picture-decimal-scales-mismatch-p giving result from to-op)
      (compile-6502-add-pic-decimal-scaled-binary out statement class-id)
      (return-from compile-6502-add nil))
    (cond
      ;; Optimise: ADD 1 TO variable (no GIVING) → inc variable (byte only)
      ((and (literal-one-p from) (not giving) (stringp to-op) (= (operand-width to-op) 1))
       (format out "~%~10Tinc ~a" (emit-6502-value to-op))
       (setf *6502-accumulator-expression* :trash/+1))
      ;; Multi-byte ADD (w >= 2): result = from + to, carry propagates
      ((>= w 2)
       (if (> w 2)
	 (progn
	   (when bcd-p (format out "~%~10Tsed"))
	   (format out "~%~10Tclc")
	   (dotimes (i w)
	     (emit-6502-load-byte-n out from class-id i w)
	     (if (expression-constant-p to-op)
	         (format out "~%~10Tadc # $ff & ( ~a >> ~d )"
		       (expression-constant-value to-op) (* 8 i))
	         (emit-6502-adc-byte-n-of-expression out to-op class-id i w))
	     (emit-6502-store-byte-n out result class-id i w
			         :skip-ldy (and (equal from result))))
	   (when bcd-p (format out "~%~10Tcld")))
	 (let ((use-stack (expression-contains-subscript-p from)))
	   (when (or use-stack (expression-contains-subscript-p to-op)
		   (expression-contains-subscript-p result))
	     (setf use-stack t))
	   ;; Store low sum at (Self),y immediately after low adc when Y is already the
	   ;; destination low offset (TO and result same slot OF Self, no subscript).
	   (let ((store-low-to-self-now-p
		 (and (not use-stack)
                          (not (expression-constant-p to-op))
                          (slot-of-self-p to-op)
                          (slot-of-self-p result)
                          (equal (slot-of-expression to-op) (slot-of-expression result)))))
	     (when bcd-p (format out "~%~10Tsed"))
	     (emit-6502-load-expression out from class-id)
	     (format out "~%~10Tclc")
	     (if (expression-constant-p to-op)
	         (let ((v (expression-constant-value to-op)))
		 (format out "~%~10Tadc #<~d" v))
	         (cond
		 ((and (listp to-op) (eql :on (first to-op)))
		  (emit-6502-alu-with-memory-rhs out "adc" to-op class-id))
		 ((slot-of-expression to-op)
		  (emit-6502-alu-with-memory-rhs out "adc" to-op class-id))
		 (t
		  (format out "~%~10Tadc ~a" (emit-6502-value to-op)))))
	     (if store-low-to-self-now-p
	         (format out "~%~10Tsta (Self), y")
	         (if use-stack
		   (format out "~%~10Tpha")
		   (progn (format out "~%~10Ttax")
			(setf *6502-x-index-expression* *6502-accumulator-expression*))))
	     (emit-6502-load-hi-byte out from class-id)
	     (if (expression-constant-p to-op)
	         (let ((v (expression-constant-value to-op)))
		 (format out "~%~10Tadc #>~d" v))
	         (cond
		 ((slot-of-expression to-op)
		  (let* ((slot-of-expression (slot-of-expression to-op))
		         (offset (apply #'slot-symbol (rest slot-of-expression)))
		         (pointer (6502-object-pointer-label (third slot-of-expression) class-id)))
                        (format out "~%~10Tldy # ~a + 1" offset)
                        (format out "~%~10Tadc (~a), y" pointer)))
		 ((slot-on-expression to-op)
		  (let* ((slot-on-expression (slot-on-expression to-op))
		         (offset (apply #'slot-symbol (rest slot-on-expression)))
		         (pointer (6502-object-pointer-label (third slot-on-expression) class-id)))
                        (format out "~%~10Tadc ~a + ~a + 1" pointer offset)))
		 (t
		  (format out "~%~10Tadc ~a + 1" (emit-6502-value to-op)))))
	     (cond ((slot-of-expression result)
		  (let* ((n (slot-of-expression result))
		         (offset (apply #'slot-symbol (rest n)))
		         (pointer (6502-object-pointer-label (third n) class-id)))
                        (format out "~%~10Tldy # ~a + 1" offset)
                        (format out "~%~10Tsta (~a), y" pointer)
                        (format out "~%~10Tdey")
                        (format out "~%~10T~a" (if use-stack "pla" "txa"))
                        (format out "~%~10Tsta (~a), y" pointer)))
		 ((slot-on-expression result)
		  (let* ((n (slot-on-expression result))
		         (offset (apply #'slot-symbol (rest n)))
		         (pointer (6502-object-pointer-label (third n) class-id)))
                        (format out "~%~10Tsta ~a + ~a + 1" pointer offset)
                        (if use-stack
		        (format out "~%~10Tpla~%~10Tsta ~a + ~a" pointer offset)
		        (format out "~%~10Tstx ~a + ~a" pointer offset))))
		 (t (let ((res-sym (emit-6502-value result)))
		      (format out "~%~10Tsta ~a + 1" res-sym)
		      (format out "~%~10T~a" (if use-stack "pla" "txa"))
		      (format out "~%~10Tsta ~a" res-sym)))))
	   (when bcd-p (format out "~%~10Tcld"))
	   (setf *6502-accumulator-expression* :trash/xx))))
      ;; General 8-bit case
      (t
       (emit-6502-load-expression out from class-id)
       (when bcd-p (format out "~%~10Tsed"))
       (format out "~%~10Tclc")
       (if giving
	 (progn
	   (if (expression-constant-p to-op)
                 (format out "~%~10Tadc # ~a" (expression-constant-value to-op))
                 (cond
	         ((slot-of-expression to-op)
		(emit-6502-alu-with-memory-rhs out "adc" to-op class-id))
	         ((slot-on-expression to-op)
		(emit-6502-alu-with-memory-rhs out "adc" to-op class-id))
	         (t
		(format out "~%~10Tadc ~a" (emit-6502-value to-op)))))
	   (emit-6502-store-byte-n out giving class-id 0 1))
	 (cond
	   ((listp to-op)
	    (emit-6502-alu-with-memory-rhs out "adc" to-op class-id)
	    (emit-6502-store-byte-n out to-op class-id 0 1))
	   (t
	    (format out "~%~10Tadc ~a" (emit-6502-value to-op))
	    (setf *6502-accumulator-expression* :trash/+giving)
	    (format out "~%~10Tsta ~a" (emit-6502-value to-op)))))
       (when bcd-p (format out "~%~10Tcld"))))))


