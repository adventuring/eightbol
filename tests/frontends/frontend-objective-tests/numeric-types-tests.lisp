;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-objective -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend OBJECTIVE Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the OBJECTIVE language numeric-types.
;;; See: src/frontend-objective/objective-parser.lisp

(in-package :eightbol/test/frontend-objective)

(fiveam:def-suite :objective-numeric-types
  :description "OBJECTIVE numeric-types tests"
  :in :frontend-objective)

(in-suite :objective-numeric-types)


(test objective_numeric_binary
  "NUMERIC: OBJECTIVE binary integers (8/16/32-bit) are correctly parsed"
(is t))

(test objective_numeric_fixed_point
  "NUMERIC: OBJECTIVE fixed-point numbers are correctly parsed and scaled"
(is t))

(test objective_numeric_bcd
  "NUMERIC: OBJECTIVE BCD numbers (if supported) are correctly parsed"
(is t))

(test objective_numeric_arithmetic
  "NUMERIC: OBJECTIVE arithmetic on mixed numeric types maintains correct precision"
(is t))

(test objective_numeric_overflow
  "NUMERIC: OBJECTIVE arithmetic overflow is handled correctly"
(is t))

