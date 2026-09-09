;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-6502 -*-
;;;
;;; EIGHTBOL EIGHTBOL 6502 Backend Arithmetic Node Tests
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

(fiveam:def-suite :backend-6502-arithmetic-node
  :description "6502 arithmetic-node tests"
  :in :backend-6502)

(in-suite :backend-6502-arithmetic-node)


(test 6502_add_8bit
  "ARITHMETIC: 6502 8-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test 6502_add_16bit
  "ARITHMETIC: 6502 16-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test 6502_subtract_8bit
  "ARITHMETIC: 6502 8-bit SUBTRACT produces correct opcodes"
  (skip "Implementation pending"))

(test 6502_multiply
  "ARITHMETIC: 6502 MULTIPLY produces correct sequence (multiply/divide if available)"
  (skip "Implementation pending"))

(test 6502_divide
  "ARITHMETIC: 6502 DIVIDE produces correct sequence"
  (skip "Implementation pending"))

(test 6502_fixed_point_arithmetic
  "ARITHMETIC: 6502 fixed-point arithmetic maintains correct scale"
  (skip "Implementation pending"))

