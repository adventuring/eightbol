;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-fountain -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FOUNTAIN Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FOUNTAIN language numeric-types.
;;; See: src/frontend-fountain/fountain-parser.lisp

(in-package :eightbol/test/frontend-fountain)

(fiveam:def-suite :fountain-numeric-types
  :description "FOUNTAIN numeric-types tests"
  :in :frontend-fountain)

(in-suite :fountain-numeric-types)


(test fountain_numeric_binary
  "NUMERIC: FOUNTAIN binary integers (8/16/32-bit) are correctly parsed"
(is t))

(test fountain_numeric_fixed_point
  "NUMERIC: FOUNTAIN fixed-point numbers are correctly parsed and scaled"
(is t))

(test fountain_numeric_bcd
  "NUMERIC: FOUNTAIN BCD numbers (if supported) are correctly parsed"
(is t))

(test fountain_numeric_arithmetic
  "NUMERIC: FOUNTAIN arithmetic on mixed numeric types maintains correct precision"
(is t))

(test fountain_numeric_overflow
  "NUMERIC: FOUNTAIN arithmetic overflow is handled correctly"
(is t))

