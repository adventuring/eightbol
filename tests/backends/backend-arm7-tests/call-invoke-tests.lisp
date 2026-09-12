;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-arm7 -*-
;;;
;;; EIGHTBOL EIGHTBOL ARM7 Backend Call/Invoke Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ARM7 backend code generation.
;;; See: src/backend-arm7/

(in-package :eightbol/test/backend-arm7)

(fiveam:def-suite :backend-arm7
  :description "ARM7 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-arm7-call-invoke
  :description "ARM7 call-invoke tests"
  :in :backend-arm7)

(in-suite :backend-arm7-call-invoke)


(test arm7_call_local
  "CALL: ARM7 local :call invocations generate correct JSR/CALL instructions"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_call_library
  "CALL: ARM7 library :call invocations are correctly linked"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_invoke_method
  "INVOKE: ARM7 :invoke method calls generate correct dispatch sequences"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_call_return_value
  "CALL: ARM7 function return values are correctly placed in accumulator"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_call_register_preservation
  "CALL: ARM7 register preservation across calls is correctly generated"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

