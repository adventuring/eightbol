;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-6502 -*-
;;;
;;; EIGHTBOL EIGHTBOL 6502 Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 6502 backend code generation.
;;; See: src/backend-6502/

(in-package :eightbol/test/backend-6502)

(fiveam:def-suite :backend-6502
  :description "6502 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-6502-control-flow
  :description "6502 control-flow tests"
  :in :backend-6502)

(in-suite :backend-6502-control-flow)


(test 6502_if_conditional
  "CONTROL FLOW: 6502 :if conditionals generate correct branch instructions"
  (skip "Implementation pending"))

(test 6502_if_nested
  "CONTROL FLOW: 6502 nested conditionals generate correct branch chains"
  (skip "Implementation pending"))

(test 6502_perform_loop
  "CONTROL FLOW: 6502 :perform loops generate correct branch/jump sequences"
  (skip "Implementation pending"))

(test 6502_goto
  "CONTROL FLOW: 6502 :goto jumps generate correct unconditional jumps"
  (skip "Implementation pending"))

(test 6502_loop_unroll
  "CONTROL FLOW: 6502 loop optimization correctly unrolls small loops"
  (skip "Implementation pending"))

