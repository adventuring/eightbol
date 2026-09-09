;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-cp1610 -*-
;;;
;;; EIGHTBOL EIGHTBOL CP1610 Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the CP1610 backend code generation.
;;; See: src/backend-cp1610/

(in-package :eightbol/test/backend-cp1610)

(fiveam:def-suite :backend-cp1610
  :description "CP1610 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-cp1610-control-flow
  :description "CP1610 control-flow tests"
  :in :backend-cp1610)

(in-suite :backend-cp1610-control-flow)


(test cp1610_if_conditional
  "CONTROL FLOW: CP1610 :if conditionals generate correct branch instructions"
  (skip "Implementation pending"))

(test cp1610_if_nested
  "CONTROL FLOW: CP1610 nested conditionals generate correct branch chains"
  (skip "Implementation pending"))

(test cp1610_perform_loop
  "CONTROL FLOW: CP1610 :perform loops generate correct branch/jump sequences"
  (skip "Implementation pending"))

(test cp1610_goto
  "CONTROL FLOW: CP1610 :goto jumps generate correct unconditional jumps"
  (skip "Implementation pending"))

(test cp1610_loop_unroll
  "CONTROL FLOW: CP1610 loop optimization correctly unrolls small loops"
  (skip "Implementation pending"))

