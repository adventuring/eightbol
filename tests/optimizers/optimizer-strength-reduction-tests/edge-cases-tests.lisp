;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-strength-reduction -*-
;;;
;;; EIGHTBOL Strength Reduction Optimizer Edge Cases
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the strength reduction optimizer pass.
;;; See: src/ast-optimize.lisp

(in-package :eightbol/test/optimizer-strength-reduction)

(fiveam:def-suite :optimizer-strength-reduction
  :description "Strength Reduction optimizer tests"
  :in :ast-optimize)

(fiveam:def-suite :optimizer-strength-reduction-edge-cases
  :description "Strength Reduction edge-cases tests"
  :in :optimizer-strength-reduction)

(in-suite :optimizer-strength-reduction-edge-cases)

;;; Nested expressions

(test strength_reduction_nested_multiply_expressions
  "EDGE CASES: Nested multiply expressions reduced recursively"
  (let* ((inner (list :multiply-expr "X" 2))
         (outer (list :multiply-expr inner 4)))
    (let ((result (strength-reduce-expression outer)))
      ;; Inner should reduce to (shift-left "X" 1)
      ;; Outer should reduce to (shift-left (shift-left "X" 1) 2)
      (is (equal (first result) :shift-left)))))

(test strength_reduction_multiply_then_divide
  "EDGE CASES: Multiply by 4 then divide by 2 both reduced"
  (let* ((mult (list :multiply-expr "X" 4))
         (div (list :divide-expr mult 2)))
    (let ((result (strength-reduce-expression div)))
      (is (eq (first result) :shift-right)))))

;;; Empty and null inputs

(test strength_reduction_null_expression
  "EDGE CASES: NIL expression returns NIL"
  (let ((result (strength-reduce-expression nil)))
    (is (null result))))

(test strength_reduction_non_list_expression
  "EDGE CASES: Non-list expression returns unchanged"
  (let ((result (strength-reduce-expression "X")))
    (is (equal result "X"))))

(test strength_reduction_empty_list
  "EDGE CASES: Empty list returns unchanged"
  (let ((result (strength-reduce-expression '())))
    (is (equal result '()))))

;;; Large shift counts

(test strength_reduction_large_power_of_two
  "EDGE CASES: Large power of 2 (1024 = 2^10) → shift left by 10"
  (let ((input (list :multiply-expr "X" 1024)))
    (let ((result (strength-reduce-expression input)))
      (is (equal result (list :shift-left "X" 10))))))

(test strength_reduction_divide_by_1024
  "EDGE CASES: Division by 1024 (2^10) → shift right by 10"
  (let ((input (list :divide-expr "X" 1024)))
    (let ((result (strength-reduce-expression input)))
      (is (equal result (list :shift-right "X" 10))))))

;;; Multiple operations in statement lists

(test strength_reduction_multiple_statements
  "EDGE CASES: Multiple statements all reduced"
  (let ((stmts (list
                 (list :compute :target "R1" :expression (list :multiply-expr "X" 4))
                 (list :compute :target "R2" :expression (list :divide-expr "Y" 8))
                 (list :compute :target "R3" :expression (list :modulo-expr "Z" 16)))))
    (let ((result (strength-reduce-in-list stmts)))
      (is (= 3 (length result)))
      (is (equal (getf (rest (first result)) :expression) (list :shift-left "X" 2)))
      (is (equal (getf (rest (second result)) :expression) (list :shift-right "Y" 3)))
      (is (equal (getf (rest (third result)) :expression) (list :∧ "Z" 15))))))

;;; Deeply nested if statements

(test strength_reduction_if_statement_nested
  "EDGE CASES: Strength reduction in nested if statements"
  (let ((input (list :if
                     :condition (list :> "X" 0)
                     :then (list (list :compute :target "R" :expression (list :multiply-expr "X" 2)))
                     :else (list (list :compute :target "R" :expression (list :divide-expr "Y" 4))))))
    (let ((result (strength-reduce-in-statement input)))
      (let ((then-stmts (getf (rest result) :then))
            (else-stmts (getf (rest result) :else)))
        (is (equal (getf (rest (first then-stmts)) :expression) (list :shift-left "X" 1)))
        (is (equal (getf (rest (first else-stmts)) :expression) (list :shift-right "Y" 2)))))))

;;; Variables vs. literals

(test strength_reduction_with_variable
  "EDGE CASES: Strength reduction works with variable identifiers"
  (let ((input (list :multiply-expr "MyVariable" 8)))
    (let ((result (strength-reduce-expression input)))
      (is (equal result (list :shift-left "MyVariable" 3))))))

(test strength_reduction_with_subscript
  "EDGE CASES: Strength reduction works with subscripted operands"
  (let ((input (list :multiply-expr (list :subscript "Array" 0) 4)))
    (let ((result (strength-reduce-expression input)))
      (is (equal result (list :shift-left (list :subscript "Array" 0) 2))))))

;;; Addition with negative constants

(test strength_reduction_add_negative
  "EDGE CASES: Adding negative constant → subtraction"
  (let ((input (list :add-expr "X" -5)))
    (let ((result (strength-reduce-expression input)))
      (is (equal result (list :subtract-expr "X" 5))))))

(test strength_reduction_subtract_negative
  "EDGE CASES: Subtracting negative constant → addition"
  (let ((input (list :subtract-expr "X" -5)))
    (let ((result (strength-reduce-expression input)))
      (is (equal result (list :add-expr "X" 5))))))

;;; Power-of-2 edge cases

(test strength_reduction_power_of_two_1
  "EDGE CASES: 1 is a power of 2, multiply by 1"
  (let ((input (list :multiply-expr "X" 1)))
    (let ((result (strength-reduce-expression input)))
      ;; 1 is power of 2 but should reduce via multiply-by-1 identity first
      (is (equal result "X")))))

;;; Confirm no transformation for unpowered values

(test strength_reduction_zero_divisor_not_transformed
  "EDGE CASES: Division by 0 not transformed (edge case)"
  (let ((input (list :divide-expr "X" 0)))
    (let ((result (strength-reduce-expression input)))
      ;; 0 is NOT a power of 2, so remains unchanged
      (is (equal result input)))))

