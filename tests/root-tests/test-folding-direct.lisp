;;; Direct test of fold-literal-expression logic

(defpackage :test-fold
  (:use :cl))

(in-package :test-fold)

;;; Implement fold-literal-expression locally for testing
(defun fold-literal-expression (expression)
  "Fold EXPRESSION when all operands are integer constants."
  (block nil
    ;; Base cases: non-list expressions
    (unless (listp expression)
      (return-from fold-literal-expression
        (cond
          ((numberp expression) expression)
          ((stringp expression) expression)
          (t (error "unknown expression ~s" expression)))))
    
    ;; Dispatch on operator keyword
    (case (first expression)
      
      ;; Canonical arithmetic operators
      (:+
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (if (and (integerp a) (integerp b))
             (+ a b)
             (list :+ a b))))
      
      (:-
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (if (and (integerp a) (integerp b))
             (- a b)
             (list :- a b))))
      
      (:×
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (if (and (integerp a) (integerp b))
             (* a b)
             (list :× a b))))
      
      (:÷
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (if (and (integerp a) (integerp b) (not (zerop b)))
             (truncate a b)
             (list :÷ a b))))
      
      ;; Bitwise operators
      (:∧
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (if (and (integerp a) (integerp b))
             (logand a b)
             (list :∧ a b))))
      
      (:∨
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (if (and (integerp a) (integerp b))
             (logior a b)
             (list :∨ a b))))
      
      (:⊻
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (if (and (integerp a) (integerp b))
             (logxor a b)
             (list :⊻ a b))))
      
      (:¬
       (let ((a (fold-literal-expression (second expression))))
         (if (integerp a)
             (lognot a)
             (list :¬ a))))
      
      ;; Shift operator: :ash uses positive for left, negative for right
      (:ash
       (let ((a (fold-literal-expression (second expression)))
             (n (fold-literal-expression (third expression))))
         (if (and (integerp a) (integerp n))
             (ash a n)
             (list :ash a n))))
      
      ;; Otherwise unchanged
      (otherwise expression))))

(defun test-fold (expr expected description)
  "Test constant folding"
  (let ((result (fold-literal-expression expr)))
    (format t "~%~A~%" description)
    (format t "  Input:    ~S~%" expr)
    (format t "  Result:   ~S~%" result)
    (format t "  Expected: ~S~%" expected)
    (if (equal result expected)
        (format t "  ✓ PASS~%")
        (format t "  ✗ FAIL~%"))))

(format t "~%===== CONSTANT FOLDING TEST SUITE =====~%")

;;; Arithmetic operators
(format t "~%--- ARITHMETIC OPERATORS ---~%")
(test-fold (list :+ 5 3) 8 "Addition: 5 + 3 = 8")
(test-fold (list :- 10 4) 6 "Subtraction: 10 - 4 = 6")
(test-fold (list :× 2 3) 6 "Multiplication: 2 × 3 = 6")
(test-fold (list :÷ 8 2) 4 "Division: 8 ÷ 2 = 4")

;;; Bitwise operators
(format t "~%--- BITWISE OPERATORS ---~%")
(test-fold (list :∧ #xFF #x0F) #x0F "Bitwise AND: $FF ∧ $0F = $0F")
(test-fold (list :∨ #xF0 #x0F) #xFF "Bitwise OR: $F0 ∨ $0F = $FF")
(test-fold (list :⊻ #xFF #xF0) #x0F "Bitwise XOR: $FF ⊻ $F0 = $0F")
(test-fold (list :¬ #x00) -1 "Bitwise NOT: ¬ $00 = -1")

;;; Shift operators
(format t "~%--- SHIFT OPERATORS ---~%")
(test-fold (list :ash 5 1) 10 "Shift left: 5 << 1 = 10")
(test-fold (list :ash 16 -2) 4 "Shift right: 16 >> 2 = 4")

;;; Non-constant expressions (should not fold)
(format t "~%--- NON-CONSTANT EXPRESSIONS (SHOULD NOT FOLD) ---~%")
(test-fold (list :+ 5 "X") (list :+ 5 "X") "Mixed: 5 + X (unchanged)")
(test-fold (list :∧ 255 "mask") (list :∧ 255 "mask") "Mixed: 255 ∧ mask (unchanged)")

;;; Edge cases
(format t "~%--- EDGE CASES ---~%")
(test-fold (list :÷ 5 0) (list :÷ 5 0) "Division by zero (not folded)")

;;; Nested expressions
(format t "~%--- NESTED EXPRESSIONS ---~%")
(test-fold (list :+ 2 (list :× 3 4)) 14 "Nested: 2 + (3 × 4) = 14")
(test-fold (list :∨ (list :∧ #xFF #x0F) #xF0) #xFF
           "Nested bitwise: ($FF ∧ $0F) ∨ $F0 = $FF")

(format t "~%===== TESTS COMPLETE =====~%~%")
