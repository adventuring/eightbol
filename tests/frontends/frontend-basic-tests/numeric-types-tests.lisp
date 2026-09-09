;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-basic -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BASIC Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BASIC language numeric-types.
;;; See: src/frontend-basic/basic-parser.lisp

(in-package :eightbol/test/frontend-basic)

(fiveam:def-suite :basic-numeric-types
  :description "BASIC numeric-types tests"
  :in :frontend-basic)

(in-suite :basic-numeric-types)


(test basic_numeric_binary
  "NUMERIC: BASIC binary integers (8/16/32-bit) are correctly parsed"
  (skip "Implementation pending"))

(test basic_numeric_fixed_point
  "NUMERIC: BASIC fixed-point numbers are correctly parsed and scaled"
  (skip "Implementation pending"))

(test basic_numeric_bcd
  "NUMERIC: BASIC BCD numbers (if supported) are correctly parsed"
  (skip "Implementation pending"))

(test basic_numeric_arithmetic
  "NUMERIC: BASIC arithmetic on mixed numeric types maintains correct precision"
  (skip "Implementation pending"))

(test basic_numeric_overflow
  "NUMERIC: BASIC arithmetic overflow is handled correctly"
  (skip "Implementation pending"))

