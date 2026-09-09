;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-stack -*-
;;;
;;; EIGHTBOL EIGHTBOL STACK Backend Control Flow Tests
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

(fiveam:def-suite :backend-stack-control-flow
  :description "STACK control-flow tests"
  :in :backend-stack)

(in-suite :backend-stack-control-flow)


(test stack_if_conditional
  "CONTROL FLOW: STACK :if conditionals generate correct branch instructions"
  (skip "Implementation pending"))

(test stack_if_nested
  "CONTROL FLOW: STACK nested conditionals generate correct branch chains"
  (skip "Implementation pending"))

(test stack_perform_loop
  "CONTROL FLOW: STACK :perform loops generate correct branch/jump sequences"
  (skip "Implementation pending"))

(test stack_goto
  "CONTROL FLOW: STACK :goto jumps generate correct unconditional jumps"
  (skip "Implementation pending"))

(test stack_loop_unroll
  "CONTROL FLOW: STACK loop optimization correctly unrolls small loops"
  (skip "Implementation pending"))

