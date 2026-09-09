;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-rp2a03 -*-
;;;
;;; EIGHTBOL EIGHTBOL RP2A03 Backend Control Flow Tests
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

(fiveam:def-suite :backend-rp2a03-control-flow
  :description "RP2A03 control-flow tests"
  :in :backend-rp2a03)

(in-suite :backend-rp2a03-control-flow)


(test rp2a03_if_conditional
  "CONTROL FLOW: RP2A03 :if conditionals generate correct branch instructions"
  (skip "Implementation pending"))

(test rp2a03_if_nested
  "CONTROL FLOW: RP2A03 nested conditionals generate correct branch chains"
  (skip "Implementation pending"))

(test rp2a03_perform_loop
  "CONTROL FLOW: RP2A03 :perform loops generate correct branch/jump sequences"
  (skip "Implementation pending"))

(test rp2a03_goto
  "CONTROL FLOW: RP2A03 :goto jumps generate correct unconditional jumps"
  (skip "Implementation pending"))

(test rp2a03_loop_unroll
  "CONTROL FLOW: RP2A03 loop optimization correctly unrolls small loops"
  (skip "Implementation pending"))

