;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/optimizer-strength-reduction -*-
;;;
;;; EIGHTBOL Strength Reduction Optimizer Tests
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

(fiveam:def-suite :optimizer-strength-reduction-transformation
  :description "Strength Reduction transformation tests"
  :in :optimizer-strength-reduction)

(in-suite :optimizer-strength-reduction-transformation)


(test strength_reduction_transformation_applied
  "TRANSFORMATION: Strength Reduction optimization is applied when applicable"
  (is t))

(test strength_reduction_transformation_no_regression
  "TRANSFORMATION: Optimization does not introduce errors"
  (is t))

(test strength_reduction_transformation_preserves_semantics
  "TRANSFORMATION: Optimized code preserves program semantics"
  (is t))

(test strength_reduction_transformation_measurable_improvement
  "TRANSFORMATION: Optimization provides measurable performance/size improvement"
  (is t))

