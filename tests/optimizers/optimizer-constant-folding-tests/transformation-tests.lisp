;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-constant-folding -*-
;;;
;;; EIGHTBOL Constant Folding Optimizer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the constant folding optimizer pass.
;;; See: src/ast-optimize.lisp

(in-package :eightbol/test/optimizer-constant-folding)

(in-suite :optimizer-constant-folding-transformation)

(test optimizer_const_fold_add_constants
  "Constant folding: (:add 2 3) optimizes to (:const 5)"
  (let* ((input '(:add (:const 2) (:const 3)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 5)))
    (is (equal result expected))))

(test optimizer_const_fold_subtract_constants
  "Constant folding: (:subtract 10 3) optimizes to (:const 7)"
  (let* ((input '(:subtract (:const 10) (:const 3)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 7)))
    (is (equal result expected))))

(test optimizer_const_fold_multiply_constants
  "Constant folding: (:multiply 4 5) optimizes to (:const 20)"
  (let* ((input '(:multiply (:const 4) (:const 5)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 20)))
    (is (equal result expected))))

(test optimizer_const_fold_divide_constants
  "Constant folding: (:divide 20 4) optimizes to (:const 5)"
  (let* ((input '(:divide (:const 20) (:const 4)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 5)))
    (is (equal result expected))))

(test optimizer_const_fold_nested
  "Constant folding: (:add (:const 2) (:multiply (:const 3) (:const 4))) optimizes fully"
  (let* ((input '(:add (:const 2) (:multiply (:const 3) (:const 4))))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 14)))
    (is (equal result expected))))

(test optimizer_const_fold_no_fold_with_variable
  "Constant folding: (:add 5 X) is NOT folded (contains variable)"
  (let* ((input '(:add (:const 5) (:var x)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding))))
    (is (not (eql (car result) :const)))))

(test optimizer_const_fold_partial_nested
  "Constant folding: (:add X (:multiply 3 4)) partially folds multiplication"
  (let* ((input '(:add (:var x) (:multiply (:const 3) (:const 4))))
         (result (eightbol:optimize-ast input :passes '(:constant-folding))))
    (is (listp result))
    (is (eql (car result) :add))))

(test optimizer_const_fold_move_literal
  "Constant folding: (:move 123 X) stays as-is (not arithmetic)"
  (let* ((input '(:move (:const 123) (:var x)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding))))
    (is (eql (car result) :move))))

(test optimizer_const_fold_string_operation
  "Constant folding: String operations are not constant-folded"
  (let* ((input '(:string-blt (:const "Hello") (:var output)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding))))
    (is (eql (car result) :string-blt))))

(test optimizer_const_fold_zero_division
  "Constant folding: Division by zero is handled gracefully"
  (let* ((input '(:divide (:const 5) (:const 0)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding))))
    ;; Should either leave as-is or produce error node, not crash
    (is (listp result))))
