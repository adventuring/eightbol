;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-arm7 -*-
;;;
;;; EIGHTBOL EIGHTBOL ARM7 Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ARM7 backend code generation.
;;; See: src/backend-arm7/

(in-package :eightbol/test/backend-arm7)

(fiveam:def-suite :backend-arm7
  :description "ARM7 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-arm7-string-operations
  :description "ARM7 string-operations tests"
  :in :backend-arm7)

(in-suite :backend-arm7-string-operations)


(test arm7_string_blt
  "STRING: ARM7 STRING BLT operations generate correct memory operations"
  (skip "Implementation pending"))

(test arm7_string_subscript
  "STRING: ARM7 subscript operations on strings generate correct index calculations"
  (skip "Implementation pending"))

(test arm7_string_refmod
  "STRING: ARM7 reference modification produces correct substring operations"
  (skip "Implementation pending"))

(test arm7_string_indexing_base
  "STRING: ARM7 zero-based vs one-based indexing is correctly handled"
  (skip "Implementation pending"))

