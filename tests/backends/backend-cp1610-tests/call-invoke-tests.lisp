;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-cp1610 -*-
;;;
;;; EIGHTBOL EIGHTBOL CP1610 Backend Call/Invoke Tests
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

(fiveam:def-suite :backend-cp1610-call-invoke
  :description "CP1610 call-invoke tests"
  :in :backend-cp1610)

(in-suite :backend-cp1610-call-invoke)


(test cp1610_call_local
  "CALL: CP1610 local :call invocations generate correct JSR/CALL instructions"
  (skip "Implementation pending"))

(test cp1610_call_library
  "CALL: CP1610 library :call invocations are correctly linked"
  (skip "Implementation pending"))

(test cp1610_invoke_method
  "INVOKE: CP1610 :invoke method calls generate correct dispatch sequences"
  (skip "Implementation pending"))

(test cp1610_call_return_value
  "CALL: CP1610 function return values are correctly placed in accumulator"
  (skip "Implementation pending"))

(test cp1610_call_register_preservation
  "CALL: CP1610 register preservation across calls is correctly generated"
  (skip "Implementation pending"))

