;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-m68k -*-
;;;
;;; EIGHTBOL EIGHTBOL M68K Backend Special Node Tests
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

(fiveam:def-suite :backend-m68k-special-nodes
  :description "M68K special-nodes tests"
  :in :backend-m68k)

(in-suite :backend-m68k-special-nodes)


(test m68k_log_fault
  "SPECIAL: M68K :log-fault produces correct debug output sequence"
  (skip "Implementation pending"))

(test m68k_debug_break
  "SPECIAL: M68K :debug-break produces correct breakpoint instruction"
  (skip "Implementation pending"))

(test m68k_inspect
  "SPECIAL: M68K :inspect produces correct debug inspection code"
  (skip "Implementation pending"))

(test m68k_evaluate
  "SPECIAL: M68K :evaluate WHEN clauses generate correct case/switch logic"
  (skip "Implementation pending"))

(test m68k_bitwise_ops
  "SPECIAL: M68K bitwise AND/OR/XOR operations generate correct opcodes"
  (skip "Implementation pending"))

