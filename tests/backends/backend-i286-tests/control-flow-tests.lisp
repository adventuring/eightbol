;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-i286 -*-
;;;
;;; EIGHTBOL EIGHTBOL I286 Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the I286 backend code generation.
;;; See: src/backend-i286/

(in-package :eightbol/test/backend-i286)

(fiveam:def-suite :backend-i286
  :description "I286 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-i286-control-flow
  :description "I286 control-flow tests"
  :in :backend-i286)

(in-suite :backend-i286-control-flow)


(test i286_if_conditional
  "CONTROL FLOW: I286 :if conditionals generate correct branch instructions"
  (skip "Implementation pending"))

(test i286_if_nested
  "CONTROL FLOW: I286 nested conditionals generate correct branch chains"
  (skip "Implementation pending"))

(test i286_perform_loop
  "CONTROL FLOW: I286 :perform loops generate correct branch/jump sequences"
  (skip "Implementation pending"))

(test i286_goto
  "CONTROL FLOW: I286 :goto jumps generate correct unconditional jumps"
  (skip "Implementation pending"))

(test i286_loop_unroll
  "CONTROL FLOW: I286 loop optimization correctly unrolls small loops"
  (skip "Implementation pending"))

