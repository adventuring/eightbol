;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-rp2a03 -*-
;;;
;;; EIGHTBOL EIGHTBOL RP2A03 Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the RP2A03 backend code generation.
;;; See: src/backend-rp2a03/

(in-package :eightbol/test/backend-rp2a03)

(fiveam:def-suite :backend-rp2a03
  :description "RP2A03 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-rp2a03-arithmetic-node
  :description "RP2A03 arithmetic-node tests"
  :in :backend-rp2a03)

(in-suite :backend-rp2a03-arithmetic-node)


(test rp2a03_add_8bit
  "ARITHMETIC: RP2A03 8-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test rp2a03_add_16bit
  "ARITHMETIC: RP2A03 16-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test rp2a03_subtract_8bit
  "ARITHMETIC: RP2A03 8-bit SUBTRACT produces correct opcodes"
  (skip "Implementation pending"))

(test rp2a03_multiply
  "ARITHMETIC: RP2A03 MULTIPLY produces correct sequence (multiply/divide if available)"
  (skip "Implementation pending"))

(test rp2a03_divide
  "ARITHMETIC: RP2A03 DIVIDE produces correct sequence"
  (skip "Implementation pending"))

(test rp2a03_fixed_point_arithmetic
  "ARITHMETIC: RP2A03 fixed-point arithmetic maintains correct scale"
  (skip "Implementation pending"))

