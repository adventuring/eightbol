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
  "Constant folding: (:+ 2 3) optimizes to (:const 5)"
  (let* ((input '(:+ (:const 2) (:const 3)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 5)))
    (is (equal result expected))))

(test optimizer_const_fold_subtract_constants
  "Constant folding: (:- 10 3) optimizes to (:const 7)"
  (let* ((input '(:- (:const 10) (:const 3)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 7)))
    (is (equal result expected))))

(test optimizer_const_fold_multiply_constants
  "Constant folding: (:× 4 5) optimizes to (:const 20)"
  (let* ((input '(:× (:const 4) (:const 5)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 20)))
    (is (equal result expected))))

(test optimizer_const_fold_divide_constants
  "Constant folding: (:÷ 20 4) optimizes to (:const 5)"
  (let* ((input '(:÷ (:const 20) (:const 4)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 5)))
    (is (equal result expected))))

(test optimizer_const_fold_nested
  "Constant folding: (:+ (:const 2) (:× (:const 3) (:const 4))) optimizes fully"
  (let* ((input '(:+ (:const 2) (:× (:const 3) (:const 4))))
         (result (eightbol:optimize-ast input :passes '(:constant-folding)))
         (expected '(:const 14)))
    (is (equal result expected))))

(test optimizer_const_fold_no_fold_with_variable
  "Constant folding: (:+ 5 X) is NOT folded (contains variable)"
  (let* ((input '(:+ (:const 5) (:var x)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding))))
    (is (not (eql (car result) :const)))))

(test optimizer_const_fold_partial_nested
  "Constant folding: (:+ X (:× 3 4)) partially folds multiplication"
  (let* ((input '(:+ (:var x) (:× (:const 3) (:const 4))))
         (result (eightbol:optimize-ast input :passes '(:constant-folding))))
    (is (listp result))
    (is (eql (car result) :+))))

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
  (let* ((input '(:÷ (:const 5) (:const 0)))
         (result (eightbol:optimize-ast input :passes '(:constant-folding))))
    ;; Should either leave as-is or produce error node, not crash
    (is (listp result))))
