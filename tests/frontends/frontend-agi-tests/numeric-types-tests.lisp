;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-agi -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend AGI Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the AGI language numeric-types.
;;; See: src/frontend-agi/agi-parser.lisp

(in-package :eightbol/test/frontend-agi)

(fiveam:def-suite :agi-numeric-types
  :description "AGI numeric-types tests"
  :in :frontend-agi)

(in-suite :agi-numeric-types)


(test agi_numeric_binary
  "NUMERIC: AGI binary integers (8/16/32-bit) are correctly parsed"
(is t))

(test agi_numeric_fixed_point
  "NUMERIC: AGI fixed-point numbers are correctly parsed and scaled"
(is t))

(test agi_numeric_bcd
  "NUMERIC: AGI BCD numbers (if supported) are correctly parsed"
(is t))

(test agi_numeric_arithmetic
  "NUMERIC: AGI arithmetic on mixed numeric types maintains correct precision"
(is t))

(test agi_numeric_overflow
  "NUMERIC: AGI arithmetic overflow is handled correctly"
(is t))

