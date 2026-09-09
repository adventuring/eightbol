;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-constant-folding -*-
;;;
;;; EIGHTBOL Constant Folding Optimizer Regressions
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the constant folding optimizer pass.
;;; See: src/ast-optimize.lisp

(in-package :eightbol/test/optimizer-constant-folding)

(fiveam:def-suite :optimizer-constant-folding
  :description "Constant Folding optimizer tests"
  :in :ast-optimize)

(fiveam:def-suite :optimizer-constant-folding-regression
  :description "Constant Folding regression tests"
  :in :optimizer-constant-folding)

(in-suite :optimizer-constant-folding-regression)


(test constant_folding_regression_known_issue_1
  "REGRESSION: Known issue from issue tracker is fixed"
  (skip "Implementation pending"))

(test constant_folding_regression_previous_failures
  "REGRESSION: Previously failing programs now work correctly"
  (skip "Implementation pending"))

(test constant_folding_regression_optimization_disabled
  "REGRESSION: Results are identical when optimization is disabled"
  (skip "Implementation pending"))

