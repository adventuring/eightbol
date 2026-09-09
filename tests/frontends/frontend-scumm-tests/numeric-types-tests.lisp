;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-scumm -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCUMM Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCUMM language numeric-types.
;;; See: src/frontend-scumm/scumm-parser.lisp

(in-package :eightbol/test/frontend-scumm)

(fiveam:def-suite :scumm-numeric-types
  :description "SCUMM numeric-types tests"
  :in :frontend-scumm)

(in-suite :scumm-numeric-types)


(test scumm_numeric_binary
  "NUMERIC: SCUMM binary integers (8/16/32-bit) are correctly parsed"
  (skip "Implementation pending"))

(test scumm_numeric_fixed_point
  "NUMERIC: SCUMM fixed-point numbers are correctly parsed and scaled"
  (skip "Implementation pending"))

(test scumm_numeric_bcd
  "NUMERIC: SCUMM BCD numbers (if supported) are correctly parsed"
  (skip "Implementation pending"))

(test scumm_numeric_arithmetic
  "NUMERIC: SCUMM arithmetic on mixed numeric types maintains correct precision"
  (skip "Implementation pending"))

(test scumm_numeric_overflow
  "NUMERIC: SCUMM arithmetic overflow is handled correctly"
  (skip "Implementation pending"))

