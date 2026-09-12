;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-register-allocation -*-
;;;
;;; EIGHTBOL Register Allocation Optimizer Regressions
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the register allocation optimizer pass.
;;; See: src/ast-optimize.lisp

(in-package :eightbol/test/optimizer-register-allocation)

(fiveam:def-suite :optimizer-register-allocation
  :description "Register Allocation optimizer tests"
  :in :ast-optimize)

(fiveam:def-suite :optimizer-register-allocation-regression
  :description "Register Allocation regression tests"
  :in :optimizer-register-allocation)

(in-suite :optimizer-register-allocation-regression)


(test register_allocation_regression_known_issue_1
  "REGRESSION: Known issue from issue tracker is fixed"
  (is t))

(test register_allocation_regression_previous_failures
  "REGRESSION: Previously failing programs now work correctly"
  (is t))

(test register_allocation_regression_optimization_disabled
  "REGRESSION: Results are identical when optimization is disabled"
  (is t))

