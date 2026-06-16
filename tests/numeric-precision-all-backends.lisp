;;; tests/numeric-precision-all-backends.lisp — Numeric precision operations across all backends
;;;
;;; Tests addition, subtraction, left shift, right shift, and move for various numeric
;;; precision formats (up to 8 bytes) across all supported backends.
;;; Uses compile-method-ast-with-tables to generate assembly and verifies compilation succeeds.

(in-package :eightbol/test)

(fiveam:def-suite :numeric-precision-all-backends
    :description "Numeric precision operations (ADD, SUBTRACT, SHIFT-LEFT, SHIFT-RIGHT, MOVE) for all backends")
(in-suite :numeric-precision-all-backends)

;; List of backends to test
(defvar *all-backends*
  '(:6502 :rp2a03 :65c02 :65c816 :huc6280 :z80 :cp1610 :sm83 :m6800 :m68k :i286 :arm7 :f8)
  "List of backend keywords to test.")

;; Helper to compute width in bytes from picture and usage
(defun pic-display-width (pic)
  "Return width in bytes for PICTURE string under USAGE DISPLAY (each 9 is one byte).
Assumes PIC consists of 9's, optionally with repetition like 9(n), and possibly V (ignored)."
  (let ((total 0)
        (i 0))
    (loop while (< i (length pic))
          do (cond
               ((char= (char pic i) #\9)
                (incf total)
                (incf i))
               ((char= (char pic i) #\V)
                (incf i)) ; ignore V
               ((char= (char pic i) #\()
                (incf i)
                (let ((start i))
                  (loop while (and (< i (length pic)) (digit-char-p (char pic i)))
                        do (incf i))
                  (let ((repeat (parse-integer (subseq pic start i))))
                    (when (< i (length pic)) (assert (char= (char pic i) #\))))
                    (incf i) ; skip )
                    (* total repeat))))
               (t (incf i))))) ; skip any other characters
  total)

(defun pic-width (pic usage)
  "Return width in bytes for PICTURE string under USAGE."
  (cond
    ((eq usage :display)
     (pic-display-width pic))
    (t
     (eightbol::pic-digits-to-width pic usage))))

;; Test cases: each entry is a list of (operation usage signed pic-a pic-b &optional count &optional pic-c)
;; For MOVE, operation is :move, with :from and :to (pic-a, pic-b).
;; For ADD: operation is :add, with :from and :to (pic-a, pic-b).
;; For SUBTRACT: operation is :subtract, with :from (subtrahend), :from-target (minuend), :giving (difference) (pic-a, pic-b, pic-c).
;; For SHIFT-LEFT/SHIFT-RIGHT: operation is :shift-left or :shift-right, with :target and :count (pic-a, count).
(defvar *test-cases*
   ;; Binary, unsigned, simple byte
   '((:add :binary nil "99" "99")
     (:subtract :binary nil "99" "99" "99")
     (:shift-left :binary nil "99" nil 1)
     (:shift-right :binary nil "99" nil 1)
     (:move :binary nil "99" "99"))
   
   ;; Binary, signed, simple byte
   '((:add :binary t "s9" "s9")
     (:subtract :binary t "s9" "s9" "s9")
     (:shift-left :binary t "s9" nil 1)
     (:shift-right :binary t "s9" nil 1)
     (:move :binary t "s9" "s9"))
   
   ;; Binary, unsigned, 2-byte (16-bit)
   '((:add :binary nil "9999" "9999")
     (:subtract :binary nil "9999" "9999" "9999")
     (:shift-left :binary nil "9999" nil 1)
     (:shift-right :binary nil "9999" nil 1)
     (:move :binary nil "9999" "9999"))
   
   ;; Binary, signed, 2-byte
   '((:add :binary t "s9999" "s9999")
     (:subtract :binary t "s9999" "s9999" "s9999")
     (:shift-left :binary t "s9999" nil 1)
     (:shift-right :binary t "s9999" nil 1)
     (:move :binary t "s9999" "s9999"))
   
   ;; Binary, unsigned, 4-byte (32-bit)
   '((:add :binary nil "9(8)" "9(8)")
     (:subtract :binary nil "9(8)" "9(8)" "9(8)")
     (:shift-left :binary nil "9(8)" nil 1)
     (:shift-right :binary nil "9(8)" nil 1)
     (:move :binary nil "9(8)" "9(8)"))
   
   ;; Binary, signed, 4-byte
   '((:add :binary t "s9(8)" "s9(8)")
     (:subtract :binary t "s9(8)" "s9(8)" "s9(8)")
     (:shift-left :binary t "s9(8)" nil 1)
     (:shift-right :binary t "s9(8)" nil 1)
     (:move :binary t "s9(8)" "s9(8)"))
   
   ;; Binary, unsigned, 8-byte (64-bit)
   '((:add :binary nil "9(16)" "9(16)")
     (:subtract :binary nil "9(16)" "9(16)" "9(16)")
     (:shift-left :binary nil "9(16)" nil 1)
     (:shift-right :binary nil "9(16)" nil 1)
     (:move :binary nil "9(16)" "9(16)"))
   
   ;; Binary, signed, 8-byte
   '((:add :binary t "S9(15)" "S9(15)") ; sign nybble + 15 digits = 16 nybbles = 8 bytes
     (:subtract :binary t "S9(15)" "S9(15)" "S9(15)")
     (:shift-left :binary t "S9(15)" nil 1)
     (:shift-right :binary t "S9(15)" nil 1)
     (:move :binary t "S9(15)" "S9(15)"))
   
   ;; Decimal, unsigned, simple byte (packed BCD)
   '((:add :decimal nil "99" "99")
     (:subtract :decimal nil "99" "99" "99")
     ;; Shift not meaningful for decimal; we still test but backends may treat as binary? We'll include.
     (:shift-left :decimal nil "99" nil 1)
     (:shift-right :decimal nil "99" nil 1)
     (:move :decimal nil "99" "99"))
   
   ;; Decimal, signed, simple byte
   '((:add :decimal t "s9" "s9")
     (:subtract :decimal t "s9" "s9" "s9")
     (:shift-left :decimal t "s9" nil 1)
     (:shift-right :decimal t "s9" nil 1)
     (:move :decimal t "s9" "s9"))
   
   ;; Decimal, unsigned, 2-byte
   '((:add :decimal nil "9999" "9999")
     (:subtract :decimal nil "9999" "9999" "9999")
     (:shift-left :decimal nil "9999" nil 1)
     (:shift-right :decimal nil "9999" nil 1)
     (:move :decimal nil "9999" "9999"))
   
   ;; Decimal, signed, 2-byte
   '((:add :decimal t "s9999" "s9999")
     (:subtract :decimal t "s9999" "s9999" "s9999")
     (:shift-left :decimal t "s9999" nil 1)
     (:shift-right :decimal t "s9999" nil 1)
     (:move :decimal t "s9999" "s9999"))
   
   ;; Decimal, unsigned, 4-byte
   '((:add :decimal nil "9(8)" "9(8)")
     (:subtract :decimal nil "9(8)" "9(8)" "9(8)")
     (:shift-left :decimal nil "9(8)" nil 1)
     (:shift-right :decimal nil "9(8)" nil 1)
     (:move :decimal nil "9(8)" "9(8)"))
   
   ;; Decimal, signed, 4-byte
   '((:add :decimal t "s9(8)" "s9(8)")
     (:subtract :decimal t "s9(8)" "s9(8)" "s9(8)")
     (:shift-left :decimal t "s9(8)" nil 1)
     (:shift-right :decimal t "s9(8)" nil 1)
     (:move :decimal t "s9(8)" "s9(8)"))
   
   ;; Decimal, unsigned, 8-byte
   '((:add :decimal nil "9(16)" "9(16)")
     (:subtract :decimal nil "9(16)" "9(16)" "9(16)")
     (:shift-left :decimal nil "9(16)" nil 1)
     (:shift-right :decimal nil "9(16)" nil 1)
     (:move :decimal nil "9(16)" "9(16)"))
   
   ;; Decimal, signed, 8-byte
   '((:add :decimal t "S9(15)" "S9(15)")
     (:subtract :decimal t "S9(15)" "S9(15)" "S9(15)")
     (:shift-left :decimal t "S9(15)" nil 1)
     (:shift-right :decimal t "S9(15)" nil 1)
     (:move :decimal t "S9(15)" "S9(15)"))
   
   ;; Mixed integer/fractional binary: 4 integer bits, 4 fractional bits (1 byte)
   '((:add :binary nil "9(4)V9(4)" "9(4)V9(4)")
     (:subtract :binary nil "9(4)V9(4)" "9(4)V9(4)" "9(4)V9(4)")
     (:shift-left :binary nil "9(4)V9(4)" nil 1)
     (:shift-right :binary nil "9(4)V9(4)" nil 1)
     (:move :binary nil "9(4)V9(4)" "9(4)V9(4)"))
   
   ;; Mixed integer/fractional binary: 12 integer, 4 fractional (2 bytes)
   '((:add :binary nil "9(12)V9(4)" "9(12)V9(4)")
     (:subtract :binary nil "9(12)V9(4)" "9(12)V9(4)" "9(12)V9(4)")
     (:shift-left :binary nil "9(12)V9(4)" nil 1)
     (:shift-right :binary nil "9(12)V9(4)" nil 1)
     (:move :binary nil "9(12)V9(4)" "9(12)V9(4)"))
   
   ;; Mixed integer/fractional decimal: 4 integer, 4 fractional (1 byte)
   '((:add :decimal nil "9(4)V9(4)" "9(4)V9(4)")
     (:subtract :decimal nil "9(4)V9(4)" "9(4)V9(4)" "9(4)V9(4)")
     (:shift-left :decimal nil "9(4)V9(4)" nil 1)
     (:shift-right :decimal nil "9(4)V9(4)" nil 1)
     (:move :decimal nil "9(4)V9(4)" "9(4)V9(4)"))
   
   ;; Mixed integer/fractional decimal: 12 integer, 4 fractional (2 bytes)
   '((:add :decimal nil "9(12)V9(4)" "9(12)V9(4)")
     (:subtract :decimal nil "9(12)V9(4)" "9(12)V9(4)" "9(12)V9(4)")
     (:shift-left :decimal nil "9(12)V9(4)" nil 1)
     (:shift-right :decimal nil "9(12)V9(4)" nil 1)
     (:move :decimal nil "9(12)V9(4)" "9(12)V9(4)"))
   
   ;; Signed mixed binary with fractional
   '((:add :binary t "S9(3)V9(4)" "S9(3)V9(4)")
     (:subtract :binary t "S9(3)V9(4)" "S9(3)V9(4)" "S9(3)V9(4)")
     (:shift-left :binary t "S9(3)V9(4)" nil 1)
     (:shift-right :binary t "S9(3)V9(4)" nil 1)
     (:move :binary t "S9(3)V9(4)" "S9(3)V9(4)")))
   
   ;; Display, unsigned (character count) - only MOVE is meaningful for DISPLAY
   '((:move :display nil "9" "9")          ; 1 character
     (:move :display nil "99" "99")        ; 2 characters
     (:move :display nil "9(4)" "9(4)")    ; 4 characters
     (:move :display nil "9(8)" "9(8)")    ; 8 characters
     ;; Note: We don't test signed for DISPLAY because it's typically unsigned character data
     ;; We don't test arithmetic or shifting for DISPLAY as they are not typically supported in the same way.
   )
;; End of test cases

;; Subscripted access test cases (same as regular but for array elements)
(defvar *subscript-test-cases* *test-cases*)

;; Generate tests for each backend and each test case (regular, non-subscripted)
(dolist (backend *all-backends*)
  (dolist (tc *test-cases*)
    (destructuring-bind (op usage signed pic-a pic-b &optional (count 1) pic-c) tc
      (let* ((width-a (pic-width pic-a usage))
             (width-b (pic-width pic-b usage))
             (width-c (if pic-c (pic-width pic-c usage) width-b)) ; default C same as B
             (pic-table (make-hash-table :test 'equalp))
             (working-storage-table (make-hash-table :test 'equalp))
             (statements
               (cond
                 ((eq op :add)
                  `((:add :from "A" :to "B")))
                 ((eq op :subtract)
                  `((:subtract :from "A" :from-target "B" :giving "C")))
                 ((eq op :shift-left)
                  `((:shift-left :target "A" :count ,count)))
                 ((eq op :shift-right)
                  `((:shift-right :target "A" :count ,count)))
                 ((eq op :move)
                  `((:move :from "A" :to "B")))
                 (t (error "Unknown operation ~S" op)))))
        ;; Set up picture width table
        (setf (gethash "A" pic-table) width-a)
        (setf (gethash "B" pic-table) width-b)
        (when (eq op :subtract)
          (setf (gethash "C" pic-table) width-c))
        ;; Set up working-storage
        (setf (gethash "A" working-storage-table) (list :usage usage :signed signed :pic pic-a))
        (setf (gethash "B" working-storage-table) (list :usage usage :signed signed :pic pic-b))
        (when (eq op :subtract)
          (setf (gethash "C" working-storage-table) (list :usage usage :signed signed :pic (if pic-c pic-c pic-b))))
        ;; Generate a unique test name
        (let* ((test-name (intern (format nil "~A-~A-~A-~A-~A"
                                          backend
                                          (if usage :binary :decimal)
                                          (if signed :signed :unsigned)
                                          (string-downcase (symbol-name op))
                                          (cond
                                            ((eq op :shift-left) "L")
                                            ((eq op :shift-right) "R")
                                            ((eq op :move) "M")
                                            ((eq op :add) "A")
                                            ((eq op :subtract) "S")
                                            (t "?"))))))
          (fiveam:def-test test-name
              (let ((asm (compile-method-ast-with-tables
                          `(:method :method-id "TEST" :statements ,statements)
                          "T" backend
                          :pic-width-table pic-table
                          :working-storage working-storage-table))
                    (is (stringp asm) "Assembly output should be a string")
                    (is (> (length asm) 0) "Assembly output should be non-empty for ~A ~A ~A ~A ~A"
                        backend usage signed op (cond
                                                  ((eq op :shift-left) "shift-left")
                                                  ((eq op :shift-right) "shift-right")
                                                  ((eq op :move) "move")
                                                  ((eq op :add) "add")
                                                  ((eq op :subtract) "subtract")
                                                  (t "unknown")))))))))))

;; Generate tests for subscripted access
(dolist (backend *all-backends*)
  (dolist (tc *subscript-test-cases*)
    (destructuring-bind (op usage signed array-pic index-pic &optional (count 1) unused-pic-c) tc
      (let* ((array-width (pic-width array-pic usage))
             (index-width (pic-width index-pic :binary)) ; index is always binary
             (pic-table (make-hash-table :test 'equalp))
             (working-storage-table (make-hash-table :test 'equalp))
             (array-var "ARR")
             (index-var "IDX")
             (statements
               (cond
                 ((eq op :add)
                  `((:add :from (:subscript ,array-var ,index-var) :to (:subscript ,array-var ,index-var))))
                 ((eq op :subtract)
                  `((:subtract :from (:subscript ,array-var ,index-var)
                               :from-target (:subscript ,array-var ,index-var)
                               :giving (:subscript ,array-var ,index-var))))
                 ((eq op :shift-left)
                  `((:shift-left :target (:subscript ,array-var ,index-var) :count ,count)))
                 ((eq op :shift-right)
                  `((:shift-right :target (:subscript ,array-var ,index-var) :count ,count)))
                 ((eq op :move)
                  `((:move :from (:subscript ,array-var ,index-var) :to (:subscript ,array-var ,index-var))))
                 (t (error "Unknown operation ~S" op)))))
        ;; Set up picture width table
        (setf (gethash array-var pic-table) array-width)
        (setf (gethash index-var pic-table) index-width)
        ;; Set up working-storage
        (setf (gethash array-var working-storage-table) (list :usage usage :signed signed :pic array-pic :occurs 10))
        (setf (gethash index-var working-storage-table) (list :usage :binary :signed nil :pic index-pic))
        ;; Generate a unique test name
        (let* ((test-name (intern (format nil "~A-~A-~A-~A-~A-SUB"
                                          backend
                                          (if usage :binary :decimal)
                                          (if signed :signed :unsigned)
                                          (string-downcase (symbol-name op))
                                          (cond
                                            ((eq op :shift-left) "L")
                                            ((eq op :shift-right) "R")
                                            ((eq op :move) "M")
                                            ((eq op :add) "A")
                                            ((eq op :subtract) "S")
                                            (t "?"))))))
          (fiveam:def-test test-name
              (let ((asm (compile-method-ast-with-tables
                          `(:method :method-id "TEST" :statements ,statements)
                          "T" backend
                          :pic-width-table pic-table
                          :working-storage working-storage-table))
                    (is (stringp asm) "Assembly output should be a string")
                    (is (> (length asm) 0) "Assembly output should be non-empty for ~A ~A ~A ~A ~A (subscripted)"
                        backend usage signed op (cond
                                                  ((eq op :shift-left) "shift-left")
                                                  ((eq op :shift-right) "shift-right")
                                                  ((eq op :move) "move")
                                                  ((eq op :add) "add")
                                                  ((eq op :subtract) "subtract")
                                                  (t "unknown")))))))))))
