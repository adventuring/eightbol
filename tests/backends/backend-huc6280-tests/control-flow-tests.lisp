;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-huc6280 -*-
;;;
;;; EIGHTBOL EIGHTBOL HUC6280 Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the HUC6280 backend code generation.
;;; See: src/backend-huc6280/

(in-package :eightbol/test/backend-huc6280)

(fiveam:def-suite :backend-huc6280
  :description "HUC6280 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-huc6280-control-flow
  :description "HUC6280 control-flow tests"
  :in :backend-huc6280)

(in-suite :backend-huc6280-control-flow)


(test huc6280_if_conditional
  "CONTROL FLOW: HUC6280 :if conditionals generate correct branch instructions"
  (skip "Implementation pending"))

(test huc6280_if_nested
  "CONTROL FLOW: HUC6280 nested conditionals generate correct branch chains"
  (skip "Implementation pending"))

(test huc6280_perform_loop
  "CONTROL FLOW: HUC6280 :perform loops generate correct branch/jump sequences"
  (skip "Implementation pending"))

(test huc6280_goto
  "CONTROL FLOW: HUC6280 :goto jumps generate correct unconditional jumps"
  (skip "Implementation pending"))

(test huc6280_loop_unroll
  "CONTROL FLOW: HUC6280 loop optimization correctly unrolls small loops"
  (skip "Implementation pending"))

