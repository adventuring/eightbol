;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-stack -*-
;;;
;;; EIGHTBOL EIGHTBOL STACK Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the STACK backend code generation.
;;; See: src/backend-stack/

(in-package :eightbol/test/backend-stack)

(fiveam:def-suite :backend-stack
  :description "STACK backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-stack-arithmetic-node
  :description "STACK arithmetic-node tests"
  :in :backend-stack)

(in-suite :backend-stack-arithmetic-node)


(test stack_add_8bit
  "ARITHMETIC: STACK 8-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test stack_add_16bit
  "ARITHMETIC: STACK 16-bit ADD produces correct opcodes"
  (skip "Implementation pending"))

(test stack_subtract_8bit
  "ARITHMETIC: STACK 8-bit SUBTRACT produces correct opcodes"
  (skip "Implementation pending"))

(test stack_multiply
  "ARITHMETIC: STACK MULTIPLY produces correct sequence (multiply/divide if available)"
  (skip "Implementation pending"))

(test stack_divide
  "ARITHMETIC: STACK DIVIDE produces correct sequence"
  (skip "Implementation pending"))

(test stack_fixed_point_arithmetic
  "ARITHMETIC: STACK fixed-point arithmetic maintains correct scale"
  (skip "Implementation pending"))

