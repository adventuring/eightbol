;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-cobol -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend COBOL Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the COBOL language numeric-types.
;;; See: src/frontend-cobol/cobol-parser.lisp

(in-package :eightbol/test/frontend-cobol)

(fiveam:def-suite :cobol-numeric-types
  :description "COBOL numeric-types tests"
  :in :frontend-cobol)

(in-suite :cobol-numeric-types)


(test cobol_numeric_binary
  "NUMERIC: COBOL binary integers (8/16/32-bit) are correctly parsed"
  (skip "Implementation pending"))

(test cobol_numeric_fixed_point
  "NUMERIC: COBOL fixed-point numbers are correctly parsed and scaled"
  (skip "Implementation pending"))

(test cobol_numeric_bcd
  "NUMERIC: COBOL BCD numbers (if supported) are correctly parsed"
  (skip "Implementation pending"))

(test cobol_numeric_arithmetic
  "NUMERIC: COBOL arithmetic on mixed numeric types maintains correct precision"
  (skip "Implementation pending"))

(test cobol_numeric_overflow
  "NUMERIC: COBOL arithmetic overflow is handled correctly"
  (skip "Implementation pending"))

