;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-loop-unrolling -*-
;;;
;;; EIGHTBOL Loop Unrolling Optimizer Regressions
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the loop unrolling optimizer pass.
;;; See: src/ast-optimize.lisp

(in-package :eightbol/test/optimizer-loop-unrolling)

(fiveam:def-suite :optimizer-loop-unrolling
  :description "Loop Unrolling optimizer tests"
  :in :ast-optimize)

(fiveam:def-suite :optimizer-loop-unrolling-regression
  :description "Loop Unrolling regression tests"
  :in :optimizer-loop-unrolling)

(in-suite :optimizer-loop-unrolling-regression)


(test loop_unrolling_regression_known_issue_1
  "REGRESSION: Known issue from issue tracker is fixed"
  (skip "Implementation pending"))

(test loop_unrolling_regression_previous_failures
  "REGRESSION: Previously failing programs now work correctly"
  (skip "Implementation pending"))

(test loop_unrolling_regression_optimization_disabled
  "REGRESSION: Results are identical when optimization is disabled"
  (skip "Implementation pending"))

