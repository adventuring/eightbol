;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c02 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C02 Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 65C02 backend code generation.
;;; See: src/backend-65c02/

(in-package :eightbol/test/backend-65c02)

(fiveam:def-suite :backend-65c02
  :description "65C02 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-65c02-control-flow
  :description "65C02 control-flow tests"
  :in :backend-65c02)

(in-suite :backend-65c02-control-flow)


(test 65c02_if_conditional
  "CONTROL FLOW: 65C02 :if conditionals generate correct branch instructions"
  (skip "Implementation pending"))

(test 65c02_if_nested
  "CONTROL FLOW: 65C02 nested conditionals generate correct branch chains"
  (skip "Implementation pending"))

(test 65c02_perform_loop
  "CONTROL FLOW: 65C02 :perform loops generate correct branch/jump sequences"
  (skip "Implementation pending"))

(test 65c02_goto
  "CONTROL FLOW: 65C02 :goto jumps generate correct unconditional jumps"
  (skip "Implementation pending"))

(test 65c02_loop_unroll
  "CONTROL FLOW: 65C02 loop optimization correctly unrolls small loops"
  (skip "Implementation pending"))

