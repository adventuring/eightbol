;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lingo -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LINGO Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LINGO language numeric-types.
;;; See: src/frontend-lingo/lingo-parser.lisp

(in-package :eightbol/test/frontend-lingo)

(fiveam:def-suite :lingo-numeric-types
  :description "LINGO numeric-types tests"
  :in :frontend-lingo)

(in-suite :lingo-numeric-types)


(test lingo_numeric_binary
  "NUMERIC: LINGO binary integers (8/16/32-bit) are correctly parsed"
  (skip "Implementation pending"))

(test lingo_numeric_fixed_point
  "NUMERIC: LINGO fixed-point numbers are correctly parsed and scaled"
  (skip "Implementation pending"))

(test lingo_numeric_bcd
  "NUMERIC: LINGO BCD numbers (if supported) are correctly parsed"
  (skip "Implementation pending"))

(test lingo_numeric_arithmetic
  "NUMERIC: LINGO arithmetic on mixed numeric types maintains correct precision"
  (skip "Implementation pending"))

(test lingo_numeric_overflow
  "NUMERIC: LINGO arithmetic overflow is handled correctly"
  (skip "Implementation pending"))

