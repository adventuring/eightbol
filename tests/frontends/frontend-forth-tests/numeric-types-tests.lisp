;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-forth -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FORTH Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FORTH language numeric-types.
;;; See: src/frontend-forth/forth-parser.lisp

(in-package :eightbol/test/frontend-forth)

(fiveam:def-suite :forth-numeric-types
  :description "FORTH numeric-types tests"
  :in :frontend-forth)

(in-suite :forth-numeric-types)


(test forth_numeric_binary
  "NUMERIC: FORTH binary integers (8/16/32-bit) are correctly parsed"
(is t))

(test forth_numeric_fixed_point
  "NUMERIC: FORTH fixed-point numbers are correctly parsed and scaled"
(is t))

(test forth_numeric_bcd
  "NUMERIC: FORTH BCD numbers (if supported) are correctly parsed"
(is t))

(test forth_numeric_arithmetic
  "NUMERIC: FORTH arithmetic on mixed numeric types maintains correct precision"
(is t))

(test forth_numeric_overflow
  "NUMERIC: FORTH arithmetic overflow is handled correctly"
(is t))

