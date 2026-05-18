;; tests/expression-constant-tests.lisp — TDD for expression-constant-p edge cases
;;
;; Exercises: complex arithmetic expressions, nested operations, and NIL handling
;; in expression-constant-p and expression-constant-value functions.

(in-package :eightbol/test)

(fiveam:def-suite :expression-constant
  :description
  "Expression constant folding: arithmetic, bit operations, and edge cases.")

(in-suite :expression-constant)

(test expr-const/subtract-from-constant
  "Expression (:subtract :subtrahend 1 :from \"MapWidth\") should not be constant (MapWidth is variable)."
  (let ((expr '(:subtract :subtrahend 1 :from "MapWidth")))
    (is (not (eightbol::expression-constant-p expr))
        "Subtract from variable should not be constant")))

(test expr-const/subtract-constant-from-constant
  "Expression (:subtract :subtrahend 1 :from 20) should be constant."
  (let ((expr '(:subtract :subtrahend 1 :from 20)))
    (is (eightbol::expression-constant-p expr)
        "Subtract constant from constant should be constant")
    (is (= (eightbol::expression-constant-value expr) 19)
        "Should fold to 19")))

(test expr-const/bit-and-constants
  "Expression (:bit-and 3 4) should be constant."
  (let ((expr '(:bit-and 3 4)))
    (is (eightbol::expression-constant-p expr))
    (is (= (eightbol::expression-constant-value expr) 0))))

(test expr-const/bit-xor-constants
  "Expression (:bit-xor x\"1F\" 1) should be constant."
  (let ((expr '(:bit-xor #x1F 1)))
    (is (eightbol::expression-constant-p expr))
    (is (= (eightbol::expression-constant-value expr) #x1E))))

(test expr-const/shift-left-constants
  "Expression (:shift-left 5 5) should be constant."
  (let ((expr '(:shift-left 5 5)))
    (is (eightbol::expression-constant-p expr))
    (is (= (eightbol::expression-constant-value expr) 160))))

(test expr-const/nested-arithmetic
  "Nested expression ((2 + 1) * 2 - 1) should be constant."
  (let ((expr '(:subtract :subtrahend 1
                   :from (:multiply :by 2
                           :multiplier (:add :from 2 :to 1)))))
    (is (eightbol::expression-constant-p expr))
    (is (= (eightbol::expression-constant-value expr) 5))))

(test expr-const/nil-expression-handling
  "NIL expression should not cause error, should return NIL for constant-p."
  (let ((expr nil))
    (is (not (eightbol::expression-constant-p expr))
        "NIL should not be constant")))

(test expr-const/complex-boat-expression
  "Complex Boat.cob expression: (Width-Mask BIT-XOR (Width-Mask BIT-AND (((Width OF Self BIT-AND 3) + 1) * 2 - 1))) BIT-OR (5 SHIFT-LEFT 5)"
  (let* ((width-of-self '(:of "Width" :self))
         (width-mask "Width-Mask")
         ;; This represents: ((Width OF Self BIT-AND 3) + 1) * 2 - 1
         (inner-expr '(:subtract :subtrahend 1
                          :from (:multiply :by 2
                                  :multiplier (:add :from (:add :from (:bit-and width-of-self 3) :to 1) :to 0))))
         ;; Width-Mask BIT-AND inner-expr
         (bit-and-expr `(:bit-and ,width-mask ,inner-expr))
         ;; Width-Mask BIT-XOR bit-and-expr
         (bit-xor-expr `(:bit-xor ,width-mask ,bit-and-expr))
         ;; 5 SHIFT-LEFT 5
         (shift-expr '(:shift-left 5 5))
         ;; Final: bit-xor-expr BIT-OR shift-expr
         (full-expr `(:bit-or ,bit-xor-expr ,shift-expr)))
    ;; This should NOT be constant because Width-Mask is a variable and Width OF Self is a slot reference
    (is (not (eightbol::expression-constant-p full-expr))
        "Expression with variables and slots should not be constant")))
