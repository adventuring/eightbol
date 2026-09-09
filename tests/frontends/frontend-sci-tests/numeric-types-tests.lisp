;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-sci -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCI Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCI language numeric-types.
;;; See: src/frontend-sci/sci-parser.lisp

(in-package :eightbol/test/frontend-sci)

(fiveam:def-suite :sci-numeric-types
  :description "SCI numeric-types tests"
  :in :frontend-sci)

(in-suite :sci-numeric-types)


(test sci_numeric_binary
  "NUMERIC: SCI binary integers (8/16/32-bit) are correctly parsed"
  (skip "Implementation pending"))

(test sci_numeric_fixed_point
  "NUMERIC: SCI fixed-point numbers are correctly parsed and scaled"
  (skip "Implementation pending"))

(test sci_numeric_bcd
  "NUMERIC: SCI BCD numbers (if supported) are correctly parsed"
  (skip "Implementation pending"))

(test sci_numeric_arithmetic
  "NUMERIC: SCI arithmetic on mixed numeric types maintains correct precision"
  (skip "Implementation pending"))

(test sci_numeric_overflow
  "NUMERIC: SCI arithmetic overflow is handled correctly"
  (skip "Implementation pending"))

