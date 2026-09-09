;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c816 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C816 Backend Call/Invoke Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 65C816 backend code generation.
;;; See: src/backend-65c816/

(in-package :eightbol/test/backend-65c816)

(fiveam:def-suite :backend-65c816
  :description "65C816 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-65c816-call-invoke
  :description "65C816 call-invoke tests"
  :in :backend-65c816)

(in-suite :backend-65c816-call-invoke)


(test 65c816_call_local
  "CALL: 65C816 local :call invocations generate correct JSR/CALL instructions"
  (skip "Implementation pending"))

(test 65c816_call_library
  "CALL: 65C816 library :call invocations are correctly linked"
  (skip "Implementation pending"))

(test 65c816_invoke_method
  "INVOKE: 65C816 :invoke method calls generate correct dispatch sequences"
  (skip "Implementation pending"))

(test 65c816_call_return_value
  "CALL: 65C816 function return values are correctly placed in accumulator"
  (skip "Implementation pending"))

(test 65c816_call_register_preservation
  "CALL: 65C816 register preservation across calls is correctly generated"
  (skip "Implementation pending"))

