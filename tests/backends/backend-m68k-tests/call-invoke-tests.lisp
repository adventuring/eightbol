;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-m68k -*-
;;;
;;; EIGHTBOL EIGHTBOL M68K Backend Call/Invoke Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the M68K backend code generation.
;;; See: src/backend-m68k/

(in-package :eightbol/test/backend-m68k)

(fiveam:def-suite :backend-m68k
  :description "M68K backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-m68k-call-invoke
  :description "M68K call-invoke tests"
  :in :backend-m68k)

(in-suite :backend-m68k-call-invoke)


(test m68k_call_local
  "CALL: M68K local :call invocations generate correct JSR/CALL instructions"
  (skip "Implementation pending"))

(test m68k_call_library
  "CALL: M68K library :call invocations are correctly linked"
  (skip "Implementation pending"))

(test m68k_invoke_method
  "INVOKE: M68K :invoke method calls generate correct dispatch sequences"
  (skip "Implementation pending"))

(test m68k_call_return_value
  "CALL: M68K function return values are correctly placed in accumulator"
  (skip "Implementation pending"))

(test m68k_call_register_preservation
  "CALL: M68K register preservation across calls is correctly generated"
  (skip "Implementation pending"))

