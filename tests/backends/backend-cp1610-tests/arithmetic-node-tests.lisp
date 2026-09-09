;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-cp1610 -*-
;;;
;;; EIGHTBOL EIGHTBOL CP1610 Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the CP1610 backend code generation.
;;; See: src/backend-cp1610/

(in-package :eightbol/test/backend-cp1610)

(fiveam:def-suite :backend-cp1610
  :description "CP1610 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-cp1610-arithmetic-node
  :description "CP1610 arithmetic-node tests"
  :in :backend-cp1610)

(in-suite :backend-cp1610-arithmetic-node)


(test cp1610_add_8bit
  "ARITHMETIC: CP1610 8-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test cp1610_add_16bit
  "ARITHMETIC: CP1610 16-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test cp1610_subtract_8bit
  "ARITHMETIC: CP1610 8-bit SUBTRACT produces correct opcodes"
  (skip "Implementation pending"))

(test cp1610_multiply
  "ARITHMETIC: CP1610 MULTIPLY produces correct sequence (multiply/divide if available)"
  (skip "Implementation pending"))

(test cp1610_divide
  "ARITHMETIC: CP1610 DIVIDE produces correct sequence"
  (skip "Implementation pending"))

(test cp1610_fixed_point_arithmetic
  "ARITHMETIC: CP1610 fixed-point arithmetic maintains correct scale"
  (skip "Implementation pending"))

