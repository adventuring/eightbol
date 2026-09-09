;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c02 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C02 Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 65C02 backend code generation.
;;; See: src/backend-65c02/

(in-package :eightbol/test/backend-65c02)

(fiveam:def-suite :backend-65c02
  :description "65C02 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-65c02-string-operations
  :description "65C02 string-operations tests"
  :in :backend-65c02)

(in-suite :backend-65c02-string-operations)


(test 65c02_string_blt
  "STRING: 65C02 STRING BLT operations generate correct memory operations"
  (skip "Implementation pending"))

(test 65c02_string_subscript
  "STRING: 65C02 subscript operations on strings generate correct index calculations"
  (skip "Implementation pending"))

(test 65c02_string_refmod
  "STRING: 65C02 reference modification produces correct substring operations"
  (skip "Implementation pending"))

(test 65c02_string_indexing_base
  "STRING: 65C02 zero-based vs one-based indexing is correctly handled"
  (skip "Implementation pending"))

