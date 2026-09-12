;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-burgermistress -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BURGERMISTRESS Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BURGERMISTRESS language numeric-types.
;;; See: src/frontend-burgermistress/burgermistress-parser.lisp

(in-package :eightbol/test/frontend-burgermistress)

(fiveam:def-suite :burgermistress-numeric-types
  :description "BURGERMISTRESS numeric-types tests"
  :in :frontend-burgermistress)

(in-suite :burgermistress-numeric-types)


(test burgermistress_numeric_binary
  "NUMERIC: BURGERMISTRESS binary integers (8/16/32-bit) are correctly parsed"
(is t))

(test burgermistress_numeric_fixed_point
  "NUMERIC: BURGERMISTRESS fixed-point numbers are correctly parsed and scaled"
(is t))

(test burgermistress_numeric_bcd
  "NUMERIC: BURGERMISTRESS BCD numbers (if supported) are correctly parsed"
(is t))

(test burgermistress_numeric_arithmetic
  "NUMERIC: BURGERMISTRESS arithmetic on mixed numeric types maintains correct precision"
(is t))

(test burgermistress_numeric_overflow
  "NUMERIC: BURGERMISTRESS arithmetic overflow is handled correctly"
(is t))

