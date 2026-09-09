;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-stack -*-
;;;
;;; EIGHTBOL EIGHTBOL STACK Backend Special Node Tests
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

(fiveam:def-suite :backend-stack-special-nodes
  :description "STACK special-nodes tests"
  :in :backend-stack)

(in-suite :backend-stack-special-nodes)


(test stack_log_fault
  "SPECIAL: STACK :log-fault produces correct debug output sequence"
  (skip "Implementation pending"))

(test stack_debug_break
  "SPECIAL: STACK :debug-break produces correct breakpoint instruction"
  (skip "Implementation pending"))

(test stack_inspect
  "SPECIAL: STACK :inspect produces correct debug inspection code"
  (skip "Implementation pending"))

(test stack_evaluate
  "SPECIAL: STACK :evaluate WHEN clauses generate correct case/switch logic"
  (skip "Implementation pending"))

(test stack_bitwise_ops
  "SPECIAL: STACK bitwise AND/OR/XOR operations generate correct opcodes"
  (skip "Implementation pending"))

