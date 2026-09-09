;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-rp2a03 -*-
;;;
;;; EIGHTBOL EIGHTBOL RP2A03 Backend Call/Invoke Tests
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

(fiveam:def-suite :backend-rp2a03-call-invoke
  :description "RP2A03 call-invoke tests"
  :in :backend-rp2a03)

(in-suite :backend-rp2a03-call-invoke)


(test rp2a03_call_local
  "CALL: RP2A03 local :call invocations generate correct JSR/CALL instructions"
  (skip "Implementation pending"))

(test rp2a03_call_library
  "CALL: RP2A03 library :call invocations are correctly linked"
  (skip "Implementation pending"))

(test rp2a03_invoke_method
  "INVOKE: RP2A03 :invoke method calls generate correct dispatch sequences"
  (skip "Implementation pending"))

(test rp2a03_call_return_value
  "CALL: RP2A03 function return values are correctly placed in accumulator"
  (skip "Implementation pending"))

(test rp2a03_call_register_preservation
  "CALL: RP2A03 register preservation across calls is correctly generated"
  (skip "Implementation pending"))

