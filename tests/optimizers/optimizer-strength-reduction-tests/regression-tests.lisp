;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-strength-reduction -*-
;;;
;;; EIGHTBOL Strength Reduction Optimizer Regressions
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

(fiveam:def-suite :optimizer-strength-reduction-regression
  :description "Strength Reduction regression tests"
  :in :optimizer-strength-reduction)

(in-suite :optimizer-strength-reduction-regression)

;;; Optimizer integration tests

(test strength_reduction_in_optimize_ast_pipeline
  "REGRESSION: Strength reduction is applied in optimize-ast pipeline"
  ;; Create a simple program with multiply-by-power-of-2
  (let* ((program (list :program
                        :class-id "TestProgram"
                        :program-id "TestProgram"
                        :identification nil
                        :environment nil
                        :data nil
                        :statements nil
                        :methods (list
                                  (list :method
                                        :method-id "Main"
                                        :statements
                                        (list
                                         (list :compute
                                               :target "Result"
                                               :expression (list :×-expr "X" 4)))))))
         (optimized (optimize-ast program)))
    ;; Check that the optimization was applied
    (let* ((methods (getf (rest optimized) :methods))
           (method (first methods))
           (stmts (getf (rest method) :statements))
           (stmt (first stmts))
           (expr (getf (rest stmt) :expression)))
      (is (equal expr (list :ash "X" 2))))))

(test strength_reduction_with_constant_folding
  "REGRESSION: Strength reduction works with constant folding"
  ;; Both optimizers should work together
  (let* ((expr (list :×-expr
                     (list :+expr 5 3)  ; Will fold to 8
                     2))                    ; Will shift by 1
         (result (strength-reduce-expression (fold-literal-expression expr))))
    ;; After folding: (multiply-expr 8 2) → after strength reduction
    (is (equal result (list :ash 8 1)))))

(test strength_reduction_preserves_non_matching
  "REGRESSION: Non-matching operations preserved unchanged"
  (let ((expressions (list
                      (list :×-expr "X" 5)   ; Not power of 2
                      (list :÷-expr "X" 7)     ; Not power of 2
                      (list :modulo-expr "X" 3)     ; Not power of 2
                      (list :+expr "X" "Y")      ; Not constant operand
                      (list :-expr "X" 1)   ; Not zero
                      (list :∧ "X" 5)                ; Not 0
                      (list :∨ "X" 1))))            ; Not 0
    ;; All should remain unchanged
    (dolist (expr expressions)
      (let ((result (strength-reduce-expression expr)))
        (is (equal result expr))))))

(test strength_reduction_invoke_statement
  "REGRESSION: :invoke statements with :using parameter reduced"
  (let ((input (list :invoke
                     :object "Object"
                     :method "DoSomething"
                     :using (list :×-expr "X" 8))))
    (let ((result (strength-reduce-in-statement input)))
      (is (equal (getf (rest result) :using) (list :ash "X" 3))))))

(test strength_reduction_nested_statements
  "REGRESSION: Statement lists with nested :if preserved correctly"
  (let ((input (list
                (list :compute :target "R1" :expression (list :×-expr "X" 2))
                (list :if
                      :condition (list :> "R1" 0)
                      :then (list (list :compute :target "R2" :expression (list :÷-expr "Y" 4)))
                      :else (list (list :compute :target "R3" :expression (list :modulo-expr "Z" 8)))))))
    (let ((result (strength-reduce-in-list input)))
      (is (= 2 (length result)))
      ;; Check first statement
      (is (equal (getf (rest (first result)) :expression) (list :ash "X" 1)))
      ;; Check if statement then branch
      (let* ((if-stmt (second result))
             (then-stmts (getf (rest if-stmt) :then))
             (then-expr (getf (rest (first then-stmts)) :expression)))
        (is (equal then-expr (list :ash "Y" 2))))
      ;; Check if statement else branch
      (let* ((if-stmt (second result))
             (else-stmts (getf (rest if-stmt) :else))
             (else-expr (getf (rest (first else-stmts)) :expression)))
        (is (equal else-expr (list :∧ "Z" 7)))))))

;;; Algebraic simplification combined with strength reduction

(test strength_reduction_multiply_1_priority_over_power_of_2
  "REGRESSION: Multiply-by-1 identity has priority over power-of-2"
  (let ((input (list :×-expr "X" 1)))
    (let ((result (strength-reduce-expression input)))
      ;; Should return "X" not (:ash "X" 0)
      (is (equal result "X")))))

(test strength_reduction_add_zero_priority_over_power_of_2
  "REGRESSION: Add-zero identity has priority over bitwise reduction"
  (let ((input (list :+expr "X" 0)))
    (let ((result (strength-reduce-expression input)))
      ;; Should return "X" not something else
      (is (equal result "X")))))

;;; Verify all transformations are implemented

(test strength_reduction_all_transformations_available
  "REGRESSION: All planned transformations are implemented"
  ;; Just verify functions exist and don't error
  (is (functionp 'is-power-of-two-p))
  (is (functionp 'shift-count-for-power-of-two))
  (is (functionp 'strength-reduce-expression))
  (is (functionp 'strength-reduce-in-statement))
  (is (functionp 'strength-reduce-in-list)))

(test strength_reduction_power_of_two_detection
  "REGRESSION: Power-of-2 detection works correctly"
  ;; Test various powers of 2
  (dolist (n '(1 2 4 8 16 32 64 128 256 512 1024))
    (is (is-power-of-two-p n) "Should detect ~A as power of 2" n))
  ;; Test non-powers
  (dolist (n '(0 3 5 6 7 9 10 11 15 17 31 33 127 129))
    (is (not (is-power-of-two-p n)) "Should NOT detect ~A as power of 2" n)))

(test strength_reduction_shift_count_calculation
  "REGRESSION: Shift count calculation is correct"
  (is (= 0 (shift-count-for-power-of-two 1)))
  (is (= 1 (shift-count-for-power-of-two 2)))
  (is (= 2 (shift-count-for-power-of-two 4)))
  (is (= 3 (shift-count-for-power-of-two 8)))
  (is (= 10 (shift-count-for-power-of-two 1024))))

