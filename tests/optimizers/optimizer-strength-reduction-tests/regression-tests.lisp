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


(test strength_reduction_regression_known_issue_1
  "REGRESSION: Known issue from issue tracker is fixed"
  (skip "Implementation pending"))

(test strength_reduction_regression_previous_failures
  "REGRESSION: Previously failing programs now work correctly"
  (skip "Implementation pending"))

(test strength_reduction_regression_optimization_disabled
  "REGRESSION: Results are identical when optimization is disabled"
  (skip "Implementation pending"))

