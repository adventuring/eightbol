;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-arm7 -*-
;;;
;;; EIGHTBOL EIGHTBOL ARM7 Backend Control Flow Tests
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

(fiveam:def-suite :backend-arm7-control-flow
  :description "ARM7 control-flow tests"
  :in :backend-arm7)

(in-suite :backend-arm7-control-flow)


(test arm7_if_conditional
  "CONTROL FLOW: ARM7 :if conditionals generate correct branch instructions"
  (skip "Implementation pending"))

(test arm7_if_nested
  "CONTROL FLOW: ARM7 nested conditionals generate correct branch chains"
  (skip "Implementation pending"))

(test arm7_perform_loop
  "CONTROL FLOW: ARM7 :perform loops generate correct branch/jump sequences"
  (skip "Implementation pending"))

(test arm7_goto
  "CONTROL FLOW: ARM7 :goto jumps generate correct unconditional jumps"
  (skip "Implementation pending"))

(test arm7_loop_unroll
  "CONTROL FLOW: ARM7 loop optimization correctly unrolls small loops"
  (skip "Implementation pending"))

