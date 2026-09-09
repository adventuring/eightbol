;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-zil -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend ZIL Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ZIL language numeric-types.
;;; See: src/frontend-zil/zil-parser.lisp

(in-package :eightbol/test/frontend-zil)

(fiveam:def-suite :zil-numeric-types
  :description "ZIL numeric-types tests"
  :in :frontend-zil)

(in-suite :zil-numeric-types)


(test zil_numeric_binary
  "NUMERIC: ZIL binary integers (8/16/32-bit) are correctly parsed"
  (skip "Implementation pending"))

(test zil_numeric_fixed_point
  "NUMERIC: ZIL fixed-point numbers are correctly parsed and scaled"
  (skip "Implementation pending"))

(test zil_numeric_bcd
  "NUMERIC: ZIL BCD numbers (if supported) are correctly parsed"
  (skip "Implementation pending"))

(test zil_numeric_arithmetic
  "NUMERIC: ZIL arithmetic on mixed numeric types maintains correct precision"
  (skip "Implementation pending"))

(test zil_numeric_overflow
  "NUMERIC: ZIL arithmetic overflow is handled correctly"
  (skip "Implementation pending"))

