;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-f8 -*-
;;;
;;; EIGHTBOL EIGHTBOL F8 Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the F8 backend code generation.
;;; See: src/backend-f8/

(in-package :eightbol/test/backend-f8)

(fiveam:def-suite :backend-f8
  :description "F8 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-f8-string-operations
  :description "F8 string-operations tests"
  :in :backend-f8)

(in-suite :backend-f8-string-operations)


(test f8_string_blt
  "STRING: F8 STRING BLT operations generate correct memory operations"
  (skip "Implementation pending"))

(test f8_string_subscript
  "STRING: F8 subscript operations on strings generate correct index calculations"
  (skip "Implementation pending"))

(test f8_string_refmod
  "STRING: F8 reference modification produces correct substring operations"
  (skip "Implementation pending"))

(test f8_string_indexing_base
  "STRING: F8 zero-based vs one-based indexing is correctly handled"
  (skip "Implementation pending"))

