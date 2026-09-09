;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-z80 -*-
;;;
;;; EIGHTBOL EIGHTBOL Z80 Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Z80 backend code generation.
;;; See: src/backend-z80/

(in-package :eightbol/test/backend-z80)

(fiveam:def-suite :backend-z80
  :description "Z80 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-z80-control-flow
  :description "Z80 control-flow tests"
  :in :backend-z80)

(in-suite :backend-z80-control-flow)


(test z80_if_conditional
  "CONTROL FLOW: Z80 :if conditionals generate correct branch instructions"
  (skip "Implementation pending"))

(test z80_if_nested
  "CONTROL FLOW: Z80 nested conditionals generate correct branch chains"
  (skip "Implementation pending"))

(test z80_perform_loop
  "CONTROL FLOW: Z80 :perform loops generate correct branch/jump sequences"
  (skip "Implementation pending"))

(test z80_goto
  "CONTROL FLOW: Z80 :goto jumps generate correct unconditional jumps"
  (skip "Implementation pending"))

(test z80_loop_unroll
  "CONTROL FLOW: Z80 loop optimization correctly unrolls small loops"
  (skip "Implementation pending"))

