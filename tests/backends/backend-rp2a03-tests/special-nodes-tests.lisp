;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-rp2a03 -*-
;;;
;;; EIGHTBOL EIGHTBOL RP2A03 Backend Special Node Tests
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

(fiveam:def-suite :backend-rp2a03-special-nodes
  :description "RP2A03 special-nodes tests"
  :in :backend-rp2a03)

(in-suite :backend-rp2a03-special-nodes)


(test rp2a03_log_fault
  "SPECIAL: RP2A03 :log-fault produces correct debug output sequence"
  (skip "Implementation pending"))

(test rp2a03_debug_break
  "SPECIAL: RP2A03 :debug-break produces correct breakpoint instruction"
  (skip "Implementation pending"))

(test rp2a03_inspect
  "SPECIAL: RP2A03 :inspect produces correct debug inspection code"
  (skip "Implementation pending"))

(test rp2a03_evaluate
  "SPECIAL: RP2A03 :evaluate WHEN clauses generate correct case/switch logic"
  (skip "Implementation pending"))

(test rp2a03_bitwise_ops
  "SPECIAL: RP2A03 bitwise AND/OR/XOR operations generate correct opcodes"
  (skip "Implementation pending"))

