(in-package :eightbol)
(defun emit-6502-load-hi-byte (out pointer-expression class-id)
  "Load the high byte of a 2-byte value into A. Handles literals, constants, variables.
For :subscript, loads index into X then lda base + 1, x."
  (emit-6502-load-byte-n out pointer-expression class-id 1 2))

(defun constant-byte-value (expression n)
  "Extract byte N (0-based) from constant EXPRESSION. Little-endian.
Uses expression-constant-value when EXPRESSION is a constant expression."
  (when (expression-constant-p expression)
    (ldb (byte 8 (* n 8)) (expression-constant-value expression))))

;;; Per-expression-type helpers for emit-6502-load-byte-n

(defun emit-6502-load-byte-n-rithmetic (out mnemonic arg0 arg1 n w &key (swap-allowed-p t))
  (cond
    ((expression-constant-p arg1)
     (emit-6502-load-byte-n out arg0 nil n w)
     (format out "~%~10T~a # $ff & ~a ~[~:;~:* >> ~d~]"
             mnemonic (emit-6502-value arg1) (* 8 n)))
    ((and swap-allowed-p (expression-constant-p arg0))
     (emit-6502-load-byte-n out arg1 nil n w)
     (format out "~%~10T~a # $ff & ~a ~[~:;~:* >> ~d~]"
             mnemonic (emit-6502-value arg0) (* 8 n)))
    ((and (listp arg1) (eql :on (first arg1)))
     (emit-6502-load-byte-n out arg0 nil n w)
     (format out "~%~10T~a ~a + ~a~[~:;~:* + ~d~]"
             mnemonic (emit-6502-value (third arg1))
             (apply #'slot-symbol (rest arg1)) n))
    ((and swap-allowed-p (listp arg0) (eql :on (first arg0)))
     (emit-6502-load-byte-n out arg1 nil n w)
     (format out "~%~10T~a ~a + ~a~[~:;~:* + ~d~]"
             mnemonic (emit-6502-value (third arg0))
             (apply #'slot-symbol (rest arg0)) n))
    (t
     (emit-6502-load-byte-n out arg0 nil n w)
     (format out "~%~10Tpha")
     (emit-6502-load-byte-n out arg1 nil n w)
     (format out "~%~
~10Ttsx
~10T~a $101, x
~10Tdex
~10Ttxs"
             mnemonic)
     (setf *6502-x-index-expression* :trash/rithmetic))))

(defun emit-6502-load-byte-n-of (out expression class-id n w)
  (declare (ignore class-id))
  (format out "~%~10Tldy # ~a~[~:;~:* + ~d~]" (apply #'slot-symbol (rest expression)) n)
  (format out "~%~10Tlda (~a), y" (second expression))
  (setf *6502-accumulator-expression* :trash/of
        *6502-x-index-expression* :trash/of))

(defun emit-6502-load-byte-n-on (out expression class-id n w)
  (declare (ignore class-id))
  (format out "~%~10Tlda ~a + ~a~[~:;~:* + ~d~]"
          (second expression) (apply #'slot-symbol (rest expression)) n)
  (setf *6502-accumulator-expression* :trash/on
        *6502-x-index-expression* :trash/on))

(defun emit-6502-load-byte-n-address-of (out expression class-id n w)
  (declare (ignore class-id))
  (assert (= 2 w))
  (assert (stringp (second expression)))
  (with-accumulator-value ((list :address-of (second expression) n))
    (format out "~%~10Tlda # ~a~a"
            (ecase n (0 "<") (1 ">"))
            (to-identifier (second expression)))))

(defun emit-6502-load-byte-n-add (out expression class-id n w)
  (declare (ignore class-id))
  (when (zerop n) (format out "~&~10Tclc"))
  (emit-6502-load-byte-n-rithmetic out "adc" (getf (rest expression) :from) (getf (rest expression) :to) n w :swap-allowed-p t))

(defun emit-6502-load-byte-n-subtract (out expression class-id n w)
  (declare (ignore class-id))
  (when (zerop n) (format out "~&~10Tsec"))
  (emit-6502-load-byte-n-rithmetic out "sbc" (getf (rest expression) :from) (getf (rest expression) :subtrahend) n w))

(defun emit-6502-load-byte-n-low (out expression class-id n w)
  (declare (ignore class-id))
  (assert (zerop n))
  (emit-6502-load-byte-n out (second expression) nil 0 2))

(defun emit-6502-load-byte-n-high (out expression class-id n w)
  (declare (ignore class-id))
  (assert (zerop n))
  (emit-6502-load-byte-n out (second expression) nil 1 2))

(defun emit-6502-load-byte-n-bit-or (out expression class-id n w)
  (declare (ignore class-id))
  (emit-6502-load-byte-n-rithmetic out "ora" (second expression) (third expression) n w :swap-allowed-p t))

(defun emit-6502-load-byte-n-bit-xor (out expression class-id n w)
  (declare (ignore class-id))
  (emit-6502-load-byte-n-rithmetic out "eor" (second expression) (third expression) n w :swap-allowed-p t))

(defun emit-6502-load-byte-n-bit-not (out expression class-id n w)
  (declare (ignore class-id))
  (emit-6502-load-byte-n-rithmetic out "eor" (second expression) #xff n w :swap-allowed-p t))

(defun emit-6502-load-byte-n-multiply (out expression class-id n w)
  (declare (ignore class-id n w))
  (emit-6502-load-expression out expression nil))

(defun emit-6502-load-byte-n-divide (out expression class-id n w)
  (declare (ignore class-id n w))
  (emit-6502-load-expression out expression nil))

(defun emit-6502-load-byte-n-shift-left (out expression class-id n w)
  (declare (ignore class-id n w))
  (emit-6502-load-expression out expression nil))

(defun emit-6502-load-byte-n-shift-right (out expression class-id n w)
  (declare (ignore class-id n w))
  (emit-6502-load-expression out expression nil))

(defun emit-6502-load-byte-n-bit-and (out expression class-id n w)
  (declare (ignore class-id))
  (emit-6502-load-byte-n-rithmetic out "and" (second expression) (third expression) n w :swap-allowed-p t))

(defun emit-6502-load-byte-n-deref (out expression class-id n w)
  "Load byte N of W-byte value from address computed by inner expression.
Emit 6502 code: compute pointer into ZP Pointer, then lda (Pointer),y."
  (declare (ignore w))
  (let ((pointer-expr (second expression)))
    (format out "~%~10T;; [deref] load via pointer")
    (emit-6502-load-byte-n out pointer-expr class-id 0 2)
    (format out "~%~10Tsta Pointer")
    (emit-6502-load-byte-n out pointer-expr class-id 1 2)
    (format out "~%~10Tsta Pointer + 1")
    (format out "~%~10Tldy # ~d" n)
    (format out "~%~10Tlda (Pointer), y")))

(defparameter *emit-6502-load-byte-n-handlers*
  (let ((table (make-hash-table)))
    (setf (gethash :of table) #'emit-6502-load-byte-n-of)
    (setf (gethash :on table) #'emit-6502-load-byte-n-on)
    (setf (gethash :address-of table) #'emit-6502-load-byte-n-address-of)
    (setf (gethash :add table) #'emit-6502-load-byte-n-add)
    (setf (gethash :subtract table) #'emit-6502-load-byte-n-subtract)
    (setf (gethash :low table) #'emit-6502-load-byte-n-low)
    (setf (gethash :high table) #'emit-6502-load-byte-n-high)
    (setf (gethash :bit-or table) #'emit-6502-load-byte-n-bit-or)
    (setf (gethash :bit-xor table) #'emit-6502-load-byte-n-bit-xor)
    (setf (gethash :bit-not table) #'emit-6502-load-byte-n-bit-not)
    (setf (gethash :multiply table) #'emit-6502-load-byte-n-multiply)
    (setf (gethash :divide table) #'emit-6502-load-byte-n-divide)
    (setf (gethash :shift-left table) #'emit-6502-load-byte-n-shift-left)
    (setf (gethash :shift-right table) #'emit-6502-load-byte-n-shift-right)
    (setf (gethash :bit-and table) #'emit-6502-load-byte-n-bit-and)
    (setf (gethash :deref table) #'emit-6502-load-byte-n-deref)
    table))

(defun emit-6502-load-byte-n (out expression class-id n w)
  "Load byte N (0-based) of W-byte EXPRESSION into A. N must be < W.
Handles  literals,  constants,  variables,   slot  OF  Self,  subscript.
Named     77/78     constants     with      byte     width     1     use
   (symbolic   #   Name),   not
#$nn, matching emit-6502-load-expression."
  (when (>= n w)
    (error "emit-6502-load-byte-n: n ~d ≥ width ~d" n w))
  (cond

    ((expression-constant-p expression)
     (if (and (zerop n) (= 1 w))
       (with-accumulator-value (expression)
         (format out "~%~10Tlda # ~a"
               (expression-constant-value expression)))
       (with-accumulator-value ((list :subscript expression n))
         (format out "~%~10Tlda # $ff & ( ~a~[~:;~:* >> ~d~] )"
               (expression-constant-value expression) (* 8 n)))))

    ((and (listp expression)
        (eql :subscript (first expression))
        (stringp (second expression))
        (expression-constant-p (third expression)))
     (with-accumulator-value (expression)
       (format out "~%~10Tlda ~a + ~a~[~:;~:* + ~d~]"
               (to-identifier (second expression))
               (expression-constant-value (third expression))
               n)))

    ((and (listp expression)
        (eql (first expression) :subscript)
        (stringp (second expression)))
     (with-accumulator-value (expression)
       (emit-6502-load-expression-into-x out (third expression) class-id)
       (format out "~%~10Tlda ~a~[~:;~:* + ~d~], x" (to-identifier (second expression)) n)))

    ((slot-of-expression expression)
     (let* ((slot-of-expression (slot-of-expression expression))
          (offset (apply #'slot-symbol (rest expression)))
          (pointer (to-identifier (third slot-of-expression))))
       (format out "~%~10Tldy # ~a~[~:;~:* + ~d~]" offset n)
       (format out "~%~10Tlda (~a), y" pointer)))

    ((and (stringp expression) (string-equal expression "Self"))
     (with-accumulator-value ((list :subscript "Self" n))
       (format out "~%~10Tlda Self~[~:;~:* + ~d~]" n)))

    ((and (stringp expression) (expression-constant-p expression))
     (if (= n 0)
       (with-accumulator-value (expression)
         (format out "~%~10Tlda # $ff & ~a" (expression-constant-value expression)))
       (with-accumulator-value ((list :subscript expression n))
         (format out "~%~10Tlda # $ff & ( ~a~[~:;~:* << ~d~] )"
               (expression-constant-value expression)
               (* 8 n)))))

    ((stringp expression)
     (with-accumulator-value ((list :subscript expression n))
       (format out "~%~10Tlda ~a~[~:;~:* + ~d~]"
               (to-identifier expression) n)))

    ((and (listp expression)
        (stringp (first expression))
        (string= "(" (first expression))
        (stringp (lastcar expression))
        (string= ")" (lastcar expression))
        (= 3 (length expression)))
     (emit-6502-load-byte-n out (second expression) class-id n w))

    ((and (listp expression)
        (or (keywordp (first expression))
              (and (symbolp (first expression))
               (find (symbol-name (first expression))
                     '("OF" "ON" "ADDRESS-OF" "ADD" "SUBTRACT"
                       "LOW" "HIGH" "BIT-OR" "BIT-XOR" "BIT-NOT" "BIT-AND"
                       "MULTIPLY" "DIVIDE" "SHIFT-LEFT" "SHIFT-RIGHT")
                     :test #'string=))))
     (let ((handler (gethash (first expression) *emit-6502-load-byte-n-handlers*)))
       (unless handler
         (error "emit-6502-load-byte-n: unknown expression type ~s" (first expression)))
       (funcall handler out expression class-id n w)))
    (t
     (with-accumulator-value (expression)
       (format out "~%~10Tlda ~a~[~:;~:* + ~d~]"
               (emit-6502-value expression) n)))))

(defun emit-6502-store-byte-n (out dest class-id n w &key skip-ldy)
  "Store A to byte N (0-based) of W-byte DEST. Handles slot OF Self, variable.

When SKIP-LDY is true, emit only sta (Self), y / sta (Pointer), y — Y must already hold
the slot offset from an immediately preceding emit-6502-load-byte-n to the same lvalue byte
(multi-byte ADD/SUBTRACT/COMPUTE loops)."
  (when (>= n w)
    (error "emit-6502-store-byte-n: index byte # ~d ≥ variable width ~d byte~:p" n w))
  (cond
    ((and (listp dest) (eql :subscript (first dest)))
     (if (and (expression-constant-p (third dest))
	    (let ((value (expression-constant-value (third dest))))
                (numberp value)))
         (if (zerop (third dest))
	   (format out "~%~10Tsta ~a" (emit-6502-value (second dest)))
	   (format out "~%~10Tsta ~a + ~d"
		 (emit-6502-value (second dest))
		 (emit-6502-value (third dest))))
         (progn
	 (emit-6502-load-expression-into-x out (third dest) nil)
	 (format out "~%~10Tsta ~a, x" (emit-6502-value (second dest))))))

    ((and (listp dest) (eql :low (first dest)) (= n 0))
     (format out "~%~10Tsta ~a" (emit-6502-value (second dest))))

    ((and (listp dest) (eql :high (first dest)) (= n 0))
     (format out "~%~10Tsta ~a + 1" (emit-6502-value (second dest))))

    ((slot-of-expression dest)
     (let* ((offset (apply #'slot-symbol (rest dest)))
	  (pointer (6502-object-pointer-label (third dest) class-id)))
       (unless skip-ldy
         (format out "~%~10Tldy # ~a~[~:;~:* + ~d~]" offset n))
       (format out "~%~10Tsta (~a), y" pointer)))

    ((slot-on-expression dest)
     (let* ((offset (apply #'slot-symbol (rest dest)))
	  (pointer (6502-object-pointer-label (third dest) class-id)))
       (format out "~%~10Tsta ~a + ~a ~[~:;~:* + ~d~]" pointer offset n)))

    ((and (listp dest) (eql :deref (first dest)))
     (let ((pointer-expr (second dest)))
       (if (expression-constant-p pointer-expr)
           (format out "~%~10Tsta ~a~[~:;~:* + ~d~]"
                   (expression-constant-value pointer-expr) n)
           (progn
             (format out "~%~10T;; [deref] store byte ~d via pointer" n)
             (format out "~%~10Tpha")
             (emit-6502-load-byte-n out pointer-expr class-id 0 2)
             (format out "~%~10Tsta Pointer")
             (emit-6502-load-byte-n out pointer-expr class-id 1 2)
             (format out "~%~10Tsta Pointer + 1")
             (format out "~%~10Tpla")
             (format out "~%~10Tldy # ~d" n)
              (format out "~%~10Tsta (Pointer), y")))))

    ((stringp dest)
     (format out "~%~10Tsta ~a~[~:;~:* + ~d~]"
	   (to-identifier dest) n))

    (t
     (format out "~%~10Tsta ~a~[~:;~:* + ~d~]"
	   (emit-6502-value dest) n))))

(defun 6502-dest-byte-address (dest class-id n w)
  "Return address string for byte N (0-based) of W-byte DEST, or nil if indirect (e.g. slot OF Self).
Used for STZ when MOVE ZERO to a direct-memory destination on 65c02+."
  (declare (ignore class-id))
  (cond
    ((>= n w) (error "Attempted to access byte ~:d of a ~:d byte~:p long object" n w))
    ((slot-of-expression dest) (error "implementation missing, FIXME ~s" dest))
    ((stringp dest) (format nil "~a~[~:;~:* + ~d~]") dest n)
    (t (format nil "~a~[~:;~:* + ~d~]" (emit-6502-value dest) n))))

(defun emit-6502-move-two-slots-16bit-lax (out from to-dest &rest _)
  "Copy 16 bits from source slot OF Self to dest slot OF Self.

Uses lax (Self),y for the low byte (A and X), iny/lda (Self),y for high,
stores high then txa/sta (Self),y for low — 6502 has no stx (zp),y.
NMOS 6502 only (lax); other CPUs use the generic byte loop."
  (declare (ignore _))
  (let ((source (apply #'slot-symbol (rest (slot-of-expression from))))
        (source-object (third from))
        (dest (apply #'slot-symbol (rest (slot-of-expression to-dest))))
        (dest-object (third from)))
    (ecase (first from)
      (:of
       (format out "~%~10Tldy # ~a" source)
       (format out "~%~10Tlax (~a), y" source-object)
       (format out "~%~10Tiny")
       (format out "~%~10Tlda (~a), y" source-object))
      (:on
       (format out "~%~10Tlax ~a + ~a" source-object source)
       (format out "~%~10Tlda ~a + ~a + 1" source-object source)))
    (ecase (first to-dest)
      (:of
       (format out "~%~10Tldy # ~a + 1" dest)
       (format out "~%~10Tsta (~a), y" dest-object)
       (format out "~%~10Tdey")
       (format out "~%~10Ttxa")
       (format out "~%~10Tsta (~a), y" dest-object))
      (:on
       (format out "~%~10Tsta ~a + ~a + 1" dest-object dest)
       (format out "~%~10Tstx ~a + ~a" dest-object dest))))
  (setf *6502-accumulator-expression* :trash/move.w
        *6502-x-index-expression* :trash/move.w))

(defun emit-6502-move-n-bytes (out from to-dest from-w to-w class-id
			 &key sign-extend)
  "Copy  FROM  (from-w bytes)  to  TO-DEST  (to-w bytes).  Sign-extends  if
SIGN-EXTEND and  from-w < to-w.  When FROM is  literal zero and  CPU has
STZ (65c02+), uses stz for each byte when destination is direct."
  (block nil
    (when (and (listp from) (eql :literal (first from)))
      (return
        (emit-6502-move-n-bytes out (second from) to-dest from-w to-w class-id :sign-extend sign-extend)))
    (when (and (listp to-dest) (eql :literal (first to-dest)))
      (return
        (emit-6502-move-n-bytes out from (second to-dest) from-w to-w class-id :sign-extend sign-extend)))
    (flet ((emit-store-zero-byte (i)
	   (let ((addr (6502-dest-byte-address to-dest class-id i to-w)))
	     (if (and (6502-has-stz-p) addr)
	         (format out "~%~10Tstz ~a" addr)
	         (progn
		 (with-accumulator-value (0)
		   (format out "~%~10Tlda # 0"))
		 (emit-6502-store-byte-n out to-dest class-id i to-w))))))
      (cond

        ((and (expression-literal-zero-p from) (6502-has-stz-p))
         ;; MOVE ZERO: use stz for each byte when destination has direct address
         (dotimes (i to-w)
	 (emit-store-zero-byte i)))

        ((and (= from-w 2) (= to-w 2)
	    (slot-of-self-p from) (slot-of-self-p to-dest)
	    (6502-use-undocumented-p))
         (emit-6502-move-two-slots-16bit-lax out from to-dest class-id))

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
	         (format out "~%~10Tbpl ~a" label-zero)
	         (with-accumulator-value (#xff)
		 (format out "~%~10Tlda #$ff"))
	         (dotimes (i (- to-w from-w))
		 (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w))
	         (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-done)
	         (format out "~%~a:" label-zero)
	         (format out "~%~10Tlda # 0")
	         (setf *6502-accumulator-expression* 0
                         *6502-x-index-expression* :trash/label-zero)
	         (dotimes (i (- to-w from-w))
		 (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w))
	         (format out "~%~a:" label-done)
	         (setf *6502-accumulator-expression* :trash/label-done
                         *6502-x-index-expression* :trash/label-done))
                 ;; no sign extension
                 (dotimes (i (- to-w from-w))
	         (with-accumulator-value (0)
		 (format out "~%~10Tlda # 0"))
	         (emit-6502-store-byte-n out to-dest class-id (+ from-w i) to-w))))))))))

;;; Operand width for byte/word arithmetic (PIC 99 vs PIC 9999)

(defun expression-contains-subscript-p (expression)
  "True if EXPRESSION or any subexpression uses :subscript (array fetch).
Array fetches use X or Y; when true, avoid using X for temp storage."
  (cond
    ((and (listp expression) (eq (first expression) :subscript)) t)
    ((and (listp expression) (member (first expression)
			       '(:add :subtract :multiply :divide
			         :shift-left :shift-right :bit-and :bit-or :bit-xor :bit-not)))
     (or (expression-contains-subscript-p (second expression))
         (expression-contains-subscript-p (third expression))))
    ((and (listp expression) (eq (first expression) :of))
     (expression-contains-subscript-p (third expression)))
    (t nil)))

;;; MOVE statement

(defun compile-6502-move (out statement class-id)
  (let* ((from (safe-getf (rest statement) :from))
         (to-dest (safe-getf (rest statement) :to))
         (to-w (expression-operand-width to-dest))
         (from-signed (expression-operand-signed-p from))
         (to-signed (expression-operand-signed-p to-dest))
         (sign-extend (and from-signed to-signed)))
    (cond
      ;; MOVE NULL TO pointer — 6502: set pointer to NULL by zeroing high byte
      ((eql from :null)
       (with-accumulator-value (0)
         (format out "~%~10Tlda # 0"))
       (emit-6502-store-byte-n out to-dest class-id (1- to-w) to-w))
      (t
       (let ((from-w (expression-operand-width from)))
         (emit-6502-move-n-bytes out from to-dest from-w to-w class-id :sign-extend sign-extend))))))

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
	  (base-string (to-identifier base)))
       (if (and (integerp start) (= start 1))
	 base-string
	 (format nil "~a + ~d" base-string (1- start)))))
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
      (error "STRING DELIMITED BY SIZE requires LENGTH clause when source/dest have no reference modification"))
    (unless (and len-val (integerp len-val) (plusp len-val))
      (error "STRING BLT length must be a positive integer constant"))
    (let ((source-addr (string-operand-address source class-id))
	(dst-addr (string-operand-address dest class-id))
	(label (string (new-6502-label "BLT_"))))
      (format out "~%~10T;; STRING ~a DELIMITED BY SIZE INTO ~a (~d bytes)"
	    (if (listp source) (safe-getf (rest source) :base) source)
	    (if (listp dest) (safe-getf (rest dest) :base) dest)
	    len-val)
      (format out "~%~10Tldy # 0")
      (format out "~%~10T~a:" label)
      (format out "~%~10Tlda ~a, y" source-addr)
      (format out "~%~10Tsta ~a, y" dst-addr)
      (format out "~%~10Tiny")
      (format out "~%~10Tcpy # ~d" len-val)
      (format out "~%~10Tbne ~a~%" label)
      (setf *6502-accumulator-expression* :trash/string-blt))))

;;; GOTO and paragraph

(defun para-label (name &rest _)
  "Return assembly label for paragraph NAME within method.
   COBOL stabby-case (e.g. My-Para) maps to PascalCase (MyPara); underscores become part of one symbol."
   (declare (ignore _))
   (to-identifier name))

(defun compile-6502-paragraph (statement)
  (let ((name (if (eq (first statement) :paragraph)
                  (second statement)
                  (or (safe-getf (rest statement) :paragraph) (second statement)))))
    (when name
      (format *output-stream* "~%~a:" (para-label (format nil "~a" name)))
      (setf *6502-accumulator-expression* :trash/paragraph-top
        *6502-x-index-expression* :trash/paragraph-top))))


