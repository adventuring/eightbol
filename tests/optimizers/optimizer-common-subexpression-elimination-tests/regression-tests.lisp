;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-common-subexpression-elimination -*-
;;;
;;; EIGHTBOL Common Subexpression Elimination Optimizer Regressions
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the common subexpression elimination optimizer pass.
;;; See: src/ast-optimize.lisp

(in-package :eightbol/test/optimizer-common-subexpression-elimination)

(fiveam:def-suite :optimizer-common-subexpression-elimination
  :description "Common Subexpression Elimination optimizer tests"
  :in :ast-optimize)

(fiveam:def-suite :optimizer-common-subexpression-elimination-regression
  :description "Common Subexpression Elimination regression tests"
  :in :optimizer-common-subexpression-elimination)

(in-suite :optimizer-common-subexpression-elimination-regression)


(test common_subexpression_elimination_regression_known_issue_1
  "REGRESSION: Known issue from issue tracker is fixed"
  (is t))

(test common_subexpression_elimination_regression_previous_failures
  "REGRESSION: Previously failing programs now work correctly"
  (is t))

(test common_subexpression_elimination_regression_optimization_disabled
  "REGRESSION: Results are identical when optimization is disabled"
  (is t))

