;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-goal -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend GOAL Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the GOAL language numeric-types.
;;; See: src/frontend-goal/goal-parser.lisp

(in-package :eightbol/test/frontend-goal)

(fiveam:def-suite :goal-numeric-types
  :description "GOAL numeric-types tests"
  :in :frontend-goal)

(in-suite :goal-numeric-types)


(test goal_numeric_binary
  "NUMERIC: GOAL binary integers (8/16/32-bit) are correctly parsed"
  (skip "Implementation pending"))

(test goal_numeric_fixed_point
  "NUMERIC: GOAL fixed-point numbers are correctly parsed and scaled"
  (skip "Implementation pending"))

(test goal_numeric_bcd
  "NUMERIC: GOAL BCD numbers (if supported) are correctly parsed"
  (skip "Implementation pending"))

(test goal_numeric_arithmetic
  "NUMERIC: GOAL arithmetic on mixed numeric types maintains correct precision"
  (skip "Implementation pending"))

(test goal_numeric_overflow
  "NUMERIC: GOAL arithmetic overflow is handled correctly"
  (skip "Implementation pending"))

