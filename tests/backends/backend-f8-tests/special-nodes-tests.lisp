;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-f8 -*-
;;;
;;; EIGHTBOL EIGHTBOL F8 Backend Special Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the F8 backend code generation.
;;; See: src/backend-f8/

(in-package :eightbol/test/backend-f8)

(fiveam:def-suite :backend-f8
  :description "F8 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-f8-special-nodes
  :description "F8 special-nodes tests"
  :in :backend-f8)

(in-suite :backend-f8-special-nodes)


(test f8_log_fault
  "SPECIAL: F8 :log-fault produces correct debug output sequence"
  (skip "Implementation pending"))

(test f8_debug_break
  "SPECIAL: F8 :debug-break produces correct breakpoint instruction"
  (skip "Implementation pending"))

(test f8_inspect
  "SPECIAL: F8 :inspect produces correct debug inspection code"
  (skip "Implementation pending"))

(test f8_evaluate
  "SPECIAL: F8 :evaluate WHEN clauses generate correct case/switch logic"
  (skip "Implementation pending"))

(test f8_bitwise_ops
  "SPECIAL: F8 bitwise AND/OR/XOR operations generate correct opcodes"
  (skip "Implementation pending"))

