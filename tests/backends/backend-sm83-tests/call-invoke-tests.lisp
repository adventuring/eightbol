;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-sm83 -*-
;;;
;;; EIGHTBOL EIGHTBOL SM83 Backend Call/Invoke Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SM83 backend code generation.
;;; See: src/backend-sm83/

(in-package :eightbol/test/backend-sm83)

(fiveam:def-suite :backend-sm83
  :description "SM83 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-sm83-call-invoke
  :description "SM83 call-invoke tests"
  :in :backend-sm83)

(in-suite :backend-sm83-call-invoke)


(test sm83_call_local
  "CALL: SM83 local :call invocations generate correct JSR/CALL instructions"
  (skip "Implementation pending"))

(test sm83_call_library
  "CALL: SM83 library :call invocations are correctly linked"
  (skip "Implementation pending"))

(test sm83_invoke_method
  "INVOKE: SM83 :invoke method calls generate correct dispatch sequences"
  (skip "Implementation pending"))

(test sm83_call_return_value
  "CALL: SM83 function return values are correctly placed in accumulator"
  (skip "Implementation pending"))

(test sm83_call_register_preservation
  "CALL: SM83 register preservation across calls is correctly generated"
  (skip "Implementation pending"))

