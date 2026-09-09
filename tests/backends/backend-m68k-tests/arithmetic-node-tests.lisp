;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-m68k -*-
;;;
;;; EIGHTBOL EIGHTBOL M68K Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the M68K backend code generation.
;;; See: src/backend-m68k/

(in-package :eightbol/test/backend-m68k)

(fiveam:def-suite :backend-m68k
  :description "M68K backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-m68k-arithmetic-node
  :description "M68K arithmetic-node tests"
  :in :backend-m68k)

(in-suite :backend-m68k-arithmetic-node)


(test m68k_add_8bit
  "ARITHMETIC: M68K 8-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test m68k_add_16bit
  "ARITHMETIC: M68K 16-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test m68k_subtract_8bit
  "ARITHMETIC: M68K 8-bit SUBTRACT produces correct opcodes"
  (skip "Implementation pending"))

(test m68k_multiply
  "ARITHMETIC: M68K MULTIPLY produces correct sequence (multiply/divide if available)"
  (skip "Implementation pending"))

(test m68k_divide
  "ARITHMETIC: M68K DIVIDE produces correct sequence"
  (skip "Implementation pending"))

(test m68k_fixed_point_arithmetic
  "ARITHMETIC: M68K fixed-point arithmetic maintains correct scale"
  (skip "Implementation pending"))

