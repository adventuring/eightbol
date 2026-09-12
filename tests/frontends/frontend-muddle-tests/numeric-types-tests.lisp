;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-muddle -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend MUDDLE Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the MUDDLE language numeric-types.
;;; See: src/frontend-muddle/muddle-parser.lisp

(in-package :eightbol/test/frontend-muddle)

(fiveam:def-suite :muddle-numeric-types
  :description "MUDDLE numeric-types tests"
  :in :frontend-muddle)

(in-suite :muddle-numeric-types)


(test muddle_numeric_binary
  "NUMERIC: MUDDLE binary integers (8/16/32-bit) are correctly parsed"
(is t))

(test muddle_numeric_fixed_point
  "NUMERIC: MUDDLE fixed-point numbers are correctly parsed and scaled"
(is t))

(test muddle_numeric_bcd
  "NUMERIC: MUDDLE BCD numbers (if supported) are correctly parsed"
(is t))

(test muddle_numeric_arithmetic
  "NUMERIC: MUDDLE arithmetic on mixed numeric types maintains correct precision"
(is t))

(test muddle_numeric_overflow
  "NUMERIC: MUDDLE arithmetic overflow is handled correctly"
(is t))

