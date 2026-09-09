;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c02 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C02 Backend Call/Invoke Tests
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

(fiveam:def-suite :backend-65c02-call-invoke
  :description "65C02 call-invoke tests"
  :in :backend-65c02)

(in-suite :backend-65c02-call-invoke)


(test 65c02_call_local
  "CALL: 65C02 local :call invocations generate correct JSR/CALL instructions"
  (skip "Implementation pending"))

(test 65c02_call_library
  "CALL: 65C02 library :call invocations are correctly linked"
  (skip "Implementation pending"))

(test 65c02_invoke_method
  "INVOKE: 65C02 :invoke method calls generate correct dispatch sequences"
  (skip "Implementation pending"))

(test 65c02_call_return_value
  "CALL: 65C02 function return values are correctly placed in accumulator"
  (skip "Implementation pending"))

(test 65c02_call_register_preservation
  "CALL: 65C02 register preservation across calls is correctly generated"
  (skip "Implementation pending"))

