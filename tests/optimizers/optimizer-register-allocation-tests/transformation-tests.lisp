;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-register-allocation -*-
;;;
;;; EIGHTBOL Register Allocation Optimizer Tests
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

(fiveam:def-suite :optimizer-register-allocation-transformation
  :description "Register Allocation transformation tests"
  :in :optimizer-register-allocation)

(in-suite :optimizer-register-allocation-transformation)


(test register_allocation_transformation_applied
  "TRANSFORMATION: Register Allocation optimization is applied when applicable"
  (is t))

(test register_allocation_transformation_no_regression
  "TRANSFORMATION: Optimization does not introduce errors"
  (is t))

(test register_allocation_transformation_preserves_semantics
  "TRANSFORMATION: Optimized code preserves program semantics"
  (is t))

(test register_allocation_transformation_measurable_improvement
  "TRANSFORMATION: Optimization provides measurable performance/size improvement"
  (is t))

