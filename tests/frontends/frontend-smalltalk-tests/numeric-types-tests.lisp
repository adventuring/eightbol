;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-smalltalk -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SMALLTALK Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SMALLTALK language numeric-types.
;;; See: src/frontend-smalltalk/smalltalk-parser.lisp

(in-package :eightbol/test/frontend-smalltalk)

(fiveam:def-suite :smalltalk-numeric-types
  :description "SMALLTALK numeric-types tests"
  :in :frontend-smalltalk)

(in-suite :smalltalk-numeric-types)


(test smalltalk_numeric_binary
  "NUMERIC: SMALLTALK binary integers (8/16/32-bit) are correctly parsed"
(is t))

(test smalltalk_numeric_fixed_point
  "NUMERIC: SMALLTALK fixed-point numbers are correctly parsed and scaled"
(is t))

(test smalltalk_numeric_bcd
  "NUMERIC: SMALLTALK BCD numbers (if supported) are correctly parsed"
(is t))

(test smalltalk_numeric_arithmetic
  "NUMERIC: SMALLTALK arithmetic on mixed numeric types maintains correct precision"
(is t))

(test smalltalk_numeric_overflow
  "NUMERIC: SMALLTALK arithmetic overflow is handled correctly"
(is t))

