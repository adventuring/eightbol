;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-arm7 -*-
;;;
;;; EIGHTBOL EIGHTBOL ARM7 Backend Special Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ARM7 backend code generation.
;;; See: src/backend-arm7/

(in-package :eightbol/test/backend-arm7)

(fiveam:def-suite :backend-arm7
  :description "ARM7 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-arm7-special-nodes
  :description "ARM7 special-nodes tests"
  :in :backend-arm7)

(in-suite :backend-arm7-special-nodes)


(test arm7_log_fault
  "SPECIAL: ARM7 :log-fault produces correct debug output sequence"
  (skip "Implementation pending"))

(test arm7_debug_break
  "SPECIAL: ARM7 :debug-break produces correct breakpoint instruction"
  (skip "Implementation pending"))

(test arm7_inspect
  "SPECIAL: ARM7 :inspect produces correct debug inspection code"
  (skip "Implementation pending"))

(test arm7_evaluate
  "SPECIAL: ARM7 :evaluate WHEN clauses generate correct case/switch logic"
  (skip "Implementation pending"))

(test arm7_bitwise_ops
  "SPECIAL: ARM7 bitwise AND/OR/XOR operations generate correct opcodes"
  (skip "Implementation pending"))

