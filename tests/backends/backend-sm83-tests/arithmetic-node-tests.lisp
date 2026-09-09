;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-sm83 -*-
;;;
;;; EIGHTBOL EIGHTBOL SM83 Backend Arithmetic Node Tests
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

(fiveam:def-suite :backend-sm83-arithmetic-node
  :description "SM83 arithmetic-node tests"
  :in :backend-sm83)

(in-suite :backend-sm83-arithmetic-node)


(test sm83_add_8bit
  "ARITHMETIC: SM83 8-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test sm83_add_16bit
  "ARITHMETIC: SM83 16-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test sm83_subtract_8bit
  "ARITHMETIC: SM83 8-bit SUBTRACT produces correct opcodes"
  (skip "Implementation pending"))

(test sm83_multiply
  "ARITHMETIC: SM83 MULTIPLY produces correct sequence (multiply/divide if available)"
  (skip "Implementation pending"))

(test sm83_divide
  "ARITHMETIC: SM83 DIVIDE produces correct sequence"
  (skip "Implementation pending"))

(test sm83_fixed_point_arithmetic
  "ARITHMETIC: SM83 fixed-point arithmetic maintains correct scale"
  (skip "Implementation pending"))

