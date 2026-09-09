;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-z80 -*-
;;;
;;; EIGHTBOL EIGHTBOL Z80 Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Z80 backend code generation.
;;; See: src/backend-z80/

(in-package :eightbol/test/backend-z80)

(fiveam:def-suite :backend-z80
  :description "Z80 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-z80-string-operations
  :description "Z80 string-operations tests"
  :in :backend-z80)

(in-suite :backend-z80-string-operations)


(test z80_string_blt
  "STRING: Z80 STRING BLT operations generate correct memory operations"
  (skip "Implementation pending"))

(test z80_string_subscript
  "STRING: Z80 subscript operations on strings generate correct index calculations"
  (skip "Implementation pending"))

(test z80_string_refmod
  "STRING: Z80 reference modification produces correct substring operations"
  (skip "Implementation pending"))

(test z80_string_indexing_base
  "STRING: Z80 zero-based vs one-based indexing is correctly handled"
  (skip "Implementation pending"))

