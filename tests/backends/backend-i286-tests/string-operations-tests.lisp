;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-i286 -*-
;;;
;;; EIGHTBOL EIGHTBOL I286 Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the I286 backend code generation.
;;; See: src/backend-i286/

(in-package :eightbol/test/backend-i286)

(fiveam:def-suite :backend-i286
  :description "I286 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-i286-string-operations
  :description "I286 string-operations tests"
  :in :backend-i286)

(in-suite :backend-i286-string-operations)


(test i286_string_blt
  "STRING: I286 STRING BLT operations generate correct memory operations"
  (skip "Implementation pending"))

(test i286_string_subscript
  "STRING: I286 subscript operations on strings generate correct index calculations"
  (skip "Implementation pending"))

(test i286_string_refmod
  "STRING: I286 reference modification produces correct substring operations"
  (skip "Implementation pending"))

(test i286_string_indexing_base
  "STRING: I286 zero-based vs one-based indexing is correctly handled"
  (skip "Implementation pending"))

