;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-dead-code-elimination -*-
;;;
;;; EIGHTBOL Dead Code Elimination Optimizer Regressions
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the dead code elimination optimizer pass.
;;; See: src/ast-optimize.lisp

(in-package :eightbol/test/optimizer-dead-code-elimination)

(fiveam:def-suite :optimizer-dead-code-elimination
  :description "Dead Code Elimination optimizer tests"
  :in :ast-optimize)

(fiveam:def-suite :optimizer-dead-code-elimination-regression
  :description "Dead Code Elimination regression tests"
  :in :optimizer-dead-code-elimination)

(in-suite :optimizer-dead-code-elimination-regression)


(test dead_code_elimination_regression_known_issue_1
  "REGRESSION: Known issue from issue tracker is fixed"
  (skip "Implementation pending"))

(test dead_code_elimination_regression_previous_failures
  "REGRESSION: Previously failing programs now work correctly"
  (skip "Implementation pending"))

(test dead_code_elimination_regression_optimization_disabled
  "REGRESSION: Results are identical when optimization is disabled"
  (skip "Implementation pending"))

