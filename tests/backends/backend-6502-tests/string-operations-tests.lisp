;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-6502 -*-
;;;
;;; EIGHTBOL EIGHTBOL 6502 Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 6502 backend code generation.
;;; See: src/backend-6502/

(in-package :eightbol/test/backend-6502)

(fiveam:def-suite :backend-6502
  :description "6502 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-6502-string-operations
  :description "6502 string-operations tests"
  :in :backend-6502)

(in-suite :backend-6502-string-operations)


(test 6502_string_blt
  "STRING: 6502 STRING BLT operations generate correct memory operations"
  (skip "Implementation pending"))

(test 6502_string_subscript
  "STRING: 6502 subscript operations on strings generate correct index calculations"
  (skip "Implementation pending"))

(test 6502_string_refmod
  "STRING: 6502 reference modification produces correct substring operations"
  (skip "Implementation pending"))

(test 6502_string_indexing_base
  "STRING: 6502 zero-based vs one-based indexing is correctly handled"
  (skip "Implementation pending"))

