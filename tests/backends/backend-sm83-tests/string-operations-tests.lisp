;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-sm83 -*-
;;;
;;; EIGHTBOL EIGHTBOL SM83 Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SM83 backend code generation.
;;; See: src/backend-sm83/

(in-package :eightbol/test/backend-sm83)

(fiveam:def-suite :backend-sm83
  :description "SM83 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-sm83-string-operations
  :description "SM83 string-operations tests"
  :in :backend-sm83)

(in-suite :backend-sm83-string-operations)


(test sm83_string_blt
  "STRING: SM83 STRING BLT operations generate correct memory operations"
  (skip "Implementation pending"))

(test sm83_string_subscript
  "STRING: SM83 subscript operations on strings generate correct index calculations"
  (skip "Implementation pending"))

(test sm83_string_refmod
  "STRING: SM83 reference modification produces correct substring operations"
  (skip "Implementation pending"))

(test sm83_string_indexing_base
  "STRING: SM83 zero-based vs one-based indexing is correctly handled"
  (skip "Implementation pending"))

