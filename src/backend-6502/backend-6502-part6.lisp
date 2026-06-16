(in-package :eightbol)
(defun compile-6502-subtract (out statement class-id)
  (multiple-value-bind (minuend subtrahend)
      (subtract-statement-minuend-and-subtrahend statement)
    (let* ((giving (safe-getf (rest statement) :giving))
	 (result (or giving minuend))
	 (w (max (operand-width result)
	         (expression-operand-width subtrahend)
	         (operand-width minuend)))
	 (bcd-p (when result (usage-bcd-p (expression-to-width-name result)))))
      (when (subtract-picture-decimal-scales-mismatch-p giving result subtrahend minuend)
        (compile-6502-subtract-pic-decimal-scaled-binary out statement class-id)
        (return-from compile-6502-subtract nil))
      (cond
        ;; Optimise: SUBTRACT 1 SUBTRAHEND variable (no GIVING, plain variable) → dec (byte only)
        ((and (literal-one-p subtrahend) (not giving) (stringp minuend) (= w 1))
         (format out "~%~10Tdec ~a" (emit-6502-value minuend))
         (setf *6502-accumulator-expression* :trash/1-))
        ;; Multi-byte SUBTRACT (w >= 2): result = minuend - subtrahend
        ((>= w 2)
         (if (> w 2)
	   (progn
	     (when bcd-p (format out "~%~10Tsed"))
	     (format out "~%~10Tsec")
	     (dotimes (i w)
                 (emit-6502-load-byte-n out minuend class-id i w)
                 (if (expression-constant-p subtrahend)
		 (format out "~%~10Tsbc # $ff & ( ~a >> ~d )"
		         (expression-constant-value subtrahend) (* 8 i))
		 (emit-6502-sbc-byte-n-of-expression out subtrahend class-id i w))
                 (emit-6502-store-byte-n out result class-id i w
                                         :skip-ldy (equal minuend result)))
	     (when bcd-p (format out "~%~10Tcld"))))
         (cond
	 ((%6502-subtract-2byte-inplace-eligible-p subtrahend minuend result giving class-id w bcd-p)
            (emit-6502-subtract-2byte-self-inplace out subtrahend result class-id bcd-p))
	 (t
            (let ((use-stack (expression-contains-subscript-p minuend)))
	    (when (or use-stack (expression-contains-subscript-p subtrahend) (expression-contains-subscript-p result))
	      (setf use-stack t))
	    (when bcd-p (format out "~%~10Tsed"))
	    ;; Low bytes: save result_lo (tax or pha)
	    (emit-6502-load-expression out minuend class-id)
	    (format out "~%~10Tsec")
	    (if (expression-constant-p subtrahend)
	        (let ((v (expression-constant-value subtrahend)))
                    (format out "~%~10Tsbc #<~d" v))
	        (cond
                    ((slot-of-expression subtrahend)
                     (emit-6502-alu-with-memory-rhs out "sbc" subtrahend class-id))
	          (t
                     (format out "~%~10Tsbc ~a" (emit-6502-value subtrahend))
		 (if use-stack
		     (format out "~%~10Tpha")
		     (progn
		       (format out "~%~10Ttax")
		       (setf *6502-x-index-expression* *6502-accumulator-expression*))))
	          ;; High bytes: result_hi = minuend_hi - subtrahend_hi - borrow
                    ))
	    (emit-6502-load-hi-byte out minuend class-id)
	    (if (expression-constant-p subtrahend)
	        (let ((v (expression-constant-value subtrahend)))
                    (format out "~%~10Tsbc #>~d" v))
	        (cond
                    ((slot-of-expression subtrahend)
                     (let* ((slot-of-expression (slot-of-expression subtrahend))
                            (offset (apply #'slot-symbol (rest slot-of-expression)))
                            (pointer (6502-object-pointer-label (third slot-of-expression) class-id)))
		   (format out "~%~10Tldy # ~a + 1" offset)
		   (format out "~%~10Tsbc (~a), y" pointer)))
                    (t
                     (format out "~%~10Tsbc ~a + 1" (emit-6502-value subtrahend)))))
	    (if (slot-of-expression result)
	        (let* ((n (slot-of-expression result))
		     (offset (apply #'slot-symbol (rest n)))
		     (pointer (6502-object-pointer-label (third n) class-id)))
                    (format out "~%~10Tldy # ~a + 1" offset)
                    (format out "~%~10Tsta (~a), y" pointer)
                    (format out "~%~10Tdey")
                    (format out "~%~10T~a" (if use-stack "pla" "txa"))
                    (format out "~%~10Tsta (~a), y" pointer))
	        (let ((res-sym (emit-6502-value result)))
                    (format out "~%~10Tsta ~a + 1" res-sym)
                    (format out "~%~10T~a" (if use-stack "pla" "txa"))
                    (format out "~%~10Tsta ~a" res-sym)))
	    (when bcd-p (format out "~%~10Tcld"))
	    (setf *6502-accumulator-expression* :trash/--)))))
        ((slot-of-expression minuend)
         (let* ((n (slot-of-expression minuend))
                (offset (apply #'slot-symbol (rest n)))
                (pointer (6502-object-pointer-label (third n) class-id)))
	 (format out "~%~10Tldy # ~a" offset)
	 (with-accumulator-value (minuend)
	   (format out "~%~10Tlda (~a), y" pointer))
	 (format out "~%~10Tsec")
	 (if (expression-constant-p subtrahend)
	     (format out "~%~10Tsbc # ~a" (expression-constant-value subtrahend))
	     (format out "~%~10Tsbc ~a" (emit-6502-value subtrahend)))
	 (format out "~%~10Tsta (~a), y" pointer)))
        #+ () (giving
               (emit-6502-load-expression out minuend class-id)
               (when bcd-p (format out "~%~10Tsed"))
               (format out "~%~10Tsec")
               (if (expression-constant-p subtrahend)
	         (format out "~%~10Tsbc # ~a" (expression-constant-value subtrahend))
	         (format out "~%~10Tsbc ~a" (emit-6502-value subtrahend)))
               (format out "~%~10Tsta (~a), y" pointer))
        (giving
         (emit-6502-load-expression out minuend class-id)
         (when bcd-p (format out "~%~10Tsed"))
         (format out "~%~10Tsec")
         (if (expression-constant-p subtrahend)
	   (format out "~%~10Tsbc # ~a" (expression-constant-value subtrahend))
	   (cond
	     ((slot-of-self-p subtrahend)
                (let ((n (slot-of-expression subtrahend)))
	        (format out "~%~10Tldy # ~a" (apply #'slot-symbol (rest n)))
	        (format out "~%~10Tsbc (Self), y")))
	     ((and (slot-of-expression subtrahend) (not (slot-of-self-p subtrahend)))
                (emit-6502-alu-with-memory-rhs out "sbc" subtrahend class-id))
	     (t
                (format out "~%~10Tsbc ~a" (emit-6502-value subtrahend)))))
         (emit-6502-store-byte-n out giving class-id 0 1)
         (when bcd-p (format out "~%~10Tcld")))
        (t
         (emit-6502-load-expression out minuend class-id)
         (when bcd-p (format out "~%~10Tsed"))
         (format out "~%~10Tsec")
         (if (expression-constant-p subtrahend)
	   (format out "~%~10Tsbc # ~a" (expression-constant-value subtrahend))
	   (cond
	     ((slot-of-self-p subtrahend)
                (let ((n (slot-of-expression subtrahend)))
	        (format out "~%~10Tldy # ~a" (apply #'slot-symbol (rest n)))
	        (format out "~%~10Tsbc (Self), y")))
	     ((and (slot-of-expression subtrahend) (not (slot-of-self-p subtrahend)))
                (emit-6502-alu-with-memory-rhs out "sbc" subtrahend class-id))
	     (t
                (format out "~%~10Tsbc ~a" (emit-6502-value subtrahend)))))
         (cond
 	 ((slot-of-self-p minuend)
 	  (let ((n (slot-of-expression minuend)))
 	    (format out "~%~10Tldy # ~a" (apply #'slot-symbol (rest n))))
 	  (format out "~%~10Tsta (Self), y"))
           ((and (slot-of-expression minuend) (not (slot-of-self-p minuend)))
	  (emit-6502-store-byte-n out minuend class-id 0 1))
           (t
	  (format out "~%~10Tsta ~a" (emit-6502-value minuend))))
         (when bcd-p (format out "~%~10Tcld")))))))

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
                                 :skip-ldy (equal expression target)))))))

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
         (format out "~%~10Tlda # <~a" (emit-6502-value source-id))
         (emit-6502-store-byte-n out target class-id 0 2)
         (with-accumulator-value ((list :bit-and #xff source-id))
           (format out "~%~10Tlda # >~a" (emit-6502-value source-id)))
         (emit-6502-store-byte-n out target class-id 1 2)))
      ((and (consp target) (eq (first target) :deref))
       ;; SET [pointer] TO value: store value indirectly via pointer
       (let* ((pointer-expr (second target))
              (ptr-w 2)   ; pointer is always 2 bytes on 6502
              (val-w (expression-operand-width value)))
         ;; Save the pointer value (from pointer-expr) to temporary zero page $FE,$FF
         (format out "~%~10T;; Pointer dereference store: SET [pointer] TO value")
         (format out "~%~10T;; Load pointer low byte from ~s" pointer-expr)
         (emit-6502-load-byte-n out pointer-expr class-id 0 ptr-w)
         (format out "~%~10Tsta $FE")
         (format out "~%~10T;; Load pointer high byte from ~s" pointer-expr)
         (emit-6502-load-byte-n out pointer-expr class-id 1 ptr-w)
         (format out "~%~10Tsta $FF")
         ;; Store each byte of the value at offset Y from the pointer address
         (dotimes (i (min val-w ptr-w))
           (format out "~%~10T;; Store byte ~d of value" i)
           (emit-6502-load-byte-n out value class-id i val-w)
           (format out "~%~10Tldy #~d" i)
           (format out "~%~10Tsta ($FE),y"))
         ;; Handle if value is narrower than pointer width (which is 2)
         (when (< val-w ptr-w)
           (format out "~%~10T;; Zero higher bytes of pointer destination")
           (with-accumulator-value (0)
             (format out "~%~10Tlda # 0"))
           (dotimes (i (- ptr-w val-w))
             (format out "~%~10Tldy #~d" (+ val-w i))
             (format out "~%~10Tsta ($FE),y")))))
      ((eql value :null)
       ;; 6502: NULL  = high byte zero.  Set pointer to NULL  by zeroing
       ;; high byte only. (65c02 can use "stz" and preserve "a")
       (with-accumulator-value (0)
         (format out "~%~10Tlda #0"))
       (emit-6502-store-byte-n out target class-id (1- w) w))
      (t
       (let ((val-w (expression-operand-width value)))
         (dotimes (i (min val-w w))
           (emit-6502-load-byte-n out value class-id i val-w)
           (emit-6502-store-byte-n out target class-id i w))
         (when (< val-w w)
           (with-accumulator-value (0)
             (format out "~%~10Tlda # 0"))
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
     (when (or (operand-bcd-p source) (operand-bcd-p dest))
       (error 'backend-error
              :message "DIVIDE: cannot use with USAGE DECIMAL operands"
              :cpu :6502 :detail statement))
     (let ((shift (log2 (expression-constant-value divisor)))
           (w (operand-width dest)))
       (when (zerop shift)
         (return-from compile-6502-divide))
      (if (= w 1)
	(progn
	  (emit-6502-load-expression out source class-id)
	  (dotimes (_ shift) (format out "~%~10Tlsr a"))
	  (emit-6502-store-byte-n out dest class-id 0 1))
	(do ((i 0 (1+ i))
	     (op source (if (= i 0) source dest)))
	    ((>= i shift))
	  (emit-6502-load-byte-n out op class-id 0 w)
	  (format out "~%~10Tlsr a")
	  (emit-6502-store-byte-n out dest class-id 0 w)
	  (emit-6502-load-byte-n out op class-id 1 w)
	  (format out "~%~10Tror a")
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
     (when (or (operand-bcd-p source) (operand-bcd-p dest))
       (error 'backend-error
              :message "MULTIPLY: cannot use with USAGE DECIMAL operands"
              :cpu :6502 :detail statement))
     (let ((shift (log2 (expression-constant-value multiplier)))
           (w (operand-width dest)))
       (when (zerop shift)
         (return-from compile-6502-multiply))
      (if (= w 1)
	(progn
	  (emit-6502-load-expression out source class-id)
	  (dotimes (_ shift) (format out "~%~10Tasl a"))
	  (emit-6502-store-byte-n out dest class-id 0 1))
	(do ((i 0 (1+ i))
	     (op source (if (= i 0) source dest)))
	    ((>= i shift))
	  (emit-6502-load-byte-n out op class-id 0 w)
	  (format out "~%~10Tasl a")
	  (emit-6502-store-byte-n out dest class-id 0 w)
	  (emit-6502-load-byte-n out op class-id 1 w)
	  (format out "~%~10Trol a")
	  (emit-6502-store-byte-n out dest class-id 1 w))))))

;;; PERFORM statement

(defun compile-6502-perform (out statement class-id)
  "Emit PERFORM: jsr  to paragraph label (same method).  Uses para-label so
target matches paragraph."
  (destructuring-bind (&key procedure varying from by until times
                       &allow-other-keys)
      (rest statement)
     (cond
       ((and varying procedure times)
        (let ((label-loop (new-6502-label "PerfLoop"))
              (label-end (new-6502-label "PerfEnd")))
          (format out "~%~10Tlda # ~d" (or from 0))
          (format out "~%~10Tsta ~a" (to-identifier varying))
          (format out "~%~a:" label-loop)
          (setf *6502-accumulator-expression* :trash/perf-loop
                *6502-x-index-expression* :trash/perf-loop)
          (format out "~%~10Tjsr ~a~%" (para-label procedure))
          (setf *6502-accumulator-expression* :trash
                *6502-x-index-expression* :trash)
          (format out "~%~10Tlda ~a" (to-identifier varying))
          (format out "~%~10Tclc")
          (format out "~%~10Tadc # ~d" (or by 1))
          (format out "~%~10Tsta ~a" (to-identifier varying))
          (format out "~%~10Tcmp # ~d" (* by times))
          (format out "~%~10Tbne ~a~%" label-loop)
          (format out "~%~a:" label-end)
          (setf *6502-accumulator-expression* :trash
                *6502-x-index-expression* :trash)))
       ((and varying procedure until)
        (let ((label-loop (new-6502-label "PerfLoop"))
              (label-end (new-6502-label "PerfEnd")))
          (format out "~%~10Tlda # ~d" (or from 0))
          (format out "~%~10Tsta ~a" (to-identifier varying))
          (format out "~%~a:" label-loop)
          (setf *6502-accumulator-expression* :trash/perf-loop
                *6502-x-index-expression* :trash/perf-loop)
          (format out "~%~10Tjsr ~a~%" (para-label procedure))
          (setf *6502-accumulator-expression* :trash
                *6502-x-index-expression* :trash)
          (format out "~%~10Tlda ~a" (to-identifier varying))
          (format out "~%~10Tclc")
          (format out "~%~10Tadc # ~d" (or by 1))
          (format out "~%~10Tsta ~a" (to-identifier varying))
          (emit-6502-condition out until class-id label-end)
          (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-loop)
          (format out "~%~a:" label-end)
          (setf *6502-accumulator-expression* :trash
                *6502-x-index-expression* :trash)))
       (procedure
        (format out "~%~10Tjsr ~a~%" (para-label procedure))
        (setf *6502-accumulator-expression* :trash
              *6502-x-index-expression* :trash))
       ((or times until)
        (error "PERFORM TIMES or UNTIL require VARYING and procedure paragraph name."))
       (t (error "PERFORM requires procedure paragraph name."))))

;;; compile-statement methods — one per (cpu, ast-node-type)
;;; Brief methods delegating to compile-6502-* helpers.

(defmacro define-6502-statement (statement-type (ast-node-data) &body body)
  "Define compile-statement for :6502 and :65c02/:65c816/:huc6280 (same impl). Uses *standard-output*."
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
  (format *standard-output* "~%~10T;; ~a"
	(if (listp ast-node-data)
	    (format nil "~{~a~%~10T;; ~}"
		  (mapcar (curry #'split-sequence #\newline) ast-node-data))
	    (split-sequence #\Newline ast-node-data))))

(define-6502-statement :goback (ast-node-data)
  (declare (ignore ast-node-data))
  (format *standard-output* "~%~10Trts~%"))

(define-6502-statement :exit-method (ast-node-data)
  (declare (ignore ast-node-data))
  (format *standard-output* "~%~10Trts~%"))

(define-6502-statement :exit-program (ast-node-data)
  (declare (ignore ast-node-data))
  (format *standard-output* "~%~10Trts~%"))

(define-6502-statement :stop-run (ast-node-data)
  (declare (ignore ast-node-data))
  (format *standard-output* "~%~10Trts~%"))

(define-6502-statement :exit (ast-node-data)
  (declare (ignore ast-node-data))
  (format *standard-output* "~%~10Trts~%"))

(define-6502-statement :move (ast-node-data)
  (compile-6502-move *standard-output* (statement :move ast-node-data) *class-id*))

(define-6502-statement :invoke (ast-node-data)
  (compile-6502-invoke *standard-output* (statement :invoke ast-node-data)))

(define-6502-statement :call (ast-node-data)
  (compile-6502-call *standard-output* (statement :call ast-node-data)))

(define-6502-statement :if (ast-node-data)
  (compile-6502-if *standard-output* (statement :if ast-node-data) cpu))

(define-6502-statement :add (ast-node-data)
  (compile-6502-add *standard-output* (statement :add ast-node-data) *class-id*))

(define-6502-statement :subtract (ast-node-data)
  (compile-6502-subtract *standard-output* (statement :subtract ast-node-data) *class-id*))

(define-6502-statement :compute (ast-node-data)
  (compile-6502-compute *standard-output* (statement :compute ast-node-data) *class-id*))

(define-6502-statement :divide (ast-node-data)
  (compile-6502-divide *standard-output* (statement :divide ast-node-data) *class-id*))

(define-6502-statement :multiply (ast-node-data)
  (compile-6502-multiply *standard-output* (statement :multiply ast-node-data) *class-id*))

(define-6502-statement :set (ast-node-data)
  (compile-6502-set *standard-output* (statement :set ast-node-data) *class-id*))

(define-6502-statement :log-fault (ast-node-data)
  (let ((code (safe-getf ast-node-data :code)))
    (format *standard-output* "~%~10T.LogFault ~a"
	  (if (stringp code)
                (format nil "\"~a\"" code)
                (emit-6502-value code)))))

(define-6502-statement :debug-break (ast-node-data)
  (let ((code (safe-getf ast-node-data :code)))
    (format *standard-output* "~%~10T.DebugBreak ~a"
	  (if (stringp code)
                (format nil "\"~a\"" code)
                (emit-6502-value code)))))

(define-6502-statement :perform (ast-node-data)
  (compile-6502-perform *standard-output* (statement :perform ast-node-data) *class-id*))

(define-6502-statement :string-blt (ast-node-data)
  (compile-6502-string-blt *standard-output* (statement :string-blt ast-node-data) *class-id*))

(define-6502-statement :goto (ast-node-data)
  (compile-6502-goto (statement :goto ast-node-data)))

(define-6502-statement :paragraph (ast-node-data)
  (compile-6502-paragraph (cons :paragraph ast-node-data)))

(define-6502-statement :evaluate (ast-node-data)
  (compile-6502-evaluate *standard-output* (statement :evaluate ast-node-data) cpu))

(define-6502-statement :inspect (ast-node-data)
  (compile-6502-inspect *standard-output* (statement :inspect ast-node-data) *class-id*))

(define-6502-statement :copy (ast-node-data)
  (error "COPY ~s should have been expanded at lex time"
         (safe-getf ast-node-data :name)))

(define-6502-statement :invoke-super (_)
  (declare (ignore _))
  (unless (gethash *class-id* *parent-classes*)
    (load-classes))
  (if-let (parent-class (gethash *class-id* *parent-classes*))
    (format *standard-output* "~%~10Tjsr Method~a~a~%"
	  (to-identifier parent-class)
	  (to-identifier *method-id*))
    (error "Can't figure out parent class of ~a" *class-id*)))

(define-6502-statement :service-bank (ast-node-data)
  (declare (ignore ast-node-data))
  (error ":service-bank is copybook metadata, not a procedure statement (corrupt AST)"))

