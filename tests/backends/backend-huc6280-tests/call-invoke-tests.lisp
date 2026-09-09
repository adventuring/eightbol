;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-huc6280 -*-
;;;
;;; EIGHTBOL EIGHTBOL HUC6280 Backend Call/Invoke Tests
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

(fiveam:def-suite :backend-huc6280-call-invoke
  :description "HUC6280 call-invoke tests"
  :in :backend-huc6280)

(in-suite :backend-huc6280-call-invoke)


(test huc6280_call_local
  "CALL: HUC6280 local :call invocations generate correct JSR/CALL instructions"
  (skip "Implementation pending"))

(test huc6280_call_library
  "CALL: HUC6280 library :call invocations are correctly linked"
  (skip "Implementation pending"))

(test huc6280_invoke_method
  "INVOKE: HUC6280 :invoke method calls generate correct dispatch sequences"
  (skip "Implementation pending"))

(test huc6280_call_return_value
  "CALL: HUC6280 function return values are correctly placed in accumulator"
  (skip "Implementation pending"))

(test huc6280_call_register_preservation
  "CALL: HUC6280 register preservation across calls is correctly generated"
  (skip "Implementation pending"))

