;;; Simple test for constant folding implementation
;;; Verifies the fold-literal-expression function with new operators

(require 'sb-posix)
(load "src/package.lisp")
(load "src/ast.lisp")
(load "src/ast-optimize.lisp")

(in-package :eightbol)

(defun test-fold (expr expected description)
  "Test constant folding"
  (let ((result (fold-literal-expression expr)))
    (format t "~%~A~%" description)
    (format t "  Input:    ~S~%" expr)
    (format t "  Result:   ~S~%" result)
    (format t "  Expected: ~S~%" expected)
    (if (equal result expected)
        (format t "  ✓ PASS~%")
        (format t "  ✗ FAIL (Result differs from expected)~%")))
  T)

(format t "~%===== EIGHTBOL CONSTANT FOLDING TESTS =====~%")

;;; Arithmetic operators
(format t "~%--- ARITHMETIC OPERATORS ---~%")
(test-fold (list :+ 5 3) 8 "Addition: 5 + 3")
(test-fold (list :- 10 4) 6 "Subtraction: 10 - 4")
(test-fold (list :× 2 3) 6 "Multiplication: 2 × 3")
(test-fold (list :÷ 8 2) 4 "Division: 8 ÷ 2")

;;; Bitwise operators
(format t "~%--- BITWISE OPERATORS ---~%")
(test-fold (list :∧ #xFF #x0F) #x0F "Bitwise AND: $FF ∧ $0F")
(test-fold (list :∨ #xF0 #x0F) #xFF "Bitwise OR: $F0 ∨ $0F")
(test-fold (list :⊻ #xFF #xF0) #x0F "Bitwise XOR: $FF ⊻ $F0")
(test-fold (list :¬ #x00) -1 "Bitwise NOT: ¬ $00")

;;; Shift operators
(format t "~%--- SHIFT OPERATORS ---~%")
(test-fold (list :ash 5 1) 10 "Shift left: 5 << 1")
(test-fold (list :ash 16 -2) 4 "Shift right: 16 >> 2")

;;; Non-constant expressions
(format t "~%--- NON-CONSTANT EXPRESSIONS ---~%")
(test-fold (list :+ 5 "X") (list :+ 5 "X") "Mixed: 5 + X (unchanged)")
(test-fold (list :∧ 255 "mask") (list :∧ 255 "mask") "Mixed: 255 ∧ mask (unchanged)")

;;; Edge cases
(format t "~%--- EDGE CASES ---~%")
(test-fold (list :÷ 5 0) (list :÷ 5 0) "Division by zero (unchanged)")

;;; Nested expressions
(format t "~%--- NESTED EXPRESSIONS ---~%")
(test-fold (list :+ 2 (list :× 3 4)) 14 "Nested: 2 + (3 × 4)")
(test-fold (list :∨ (list :∧ #xFF #x0F) #xF0) #xFF "Nested bitwise: ($FF ∧ $0F) ∨ $F0")

(format t "~%===== TESTS COMPLETE =====~%~%")
(quit)
