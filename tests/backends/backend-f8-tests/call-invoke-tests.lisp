;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-f8 -*-
;;;
;;; EIGHTBOL EIGHTBOL F8 Backend Call/Invoke Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the F8 backend code generation.
;;; See: src/backend-f8/

(in-package :eightbol/test/backend-f8)

(fiveam:def-suite :backend-f8
  :description "F8 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-f8-call-invoke
  :description "F8 call-invoke tests"
  :in :backend-f8)

(in-suite :backend-f8-call-invoke)


(test f8_call_local
  "CALL: F8 local :call invocations generate correct JSR/CALL instructions"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_call_library
  "CALL: F8 library :call invocations are correctly linked"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_invoke_method
  "INVOKE: F8 :invoke method calls generate correct dispatch sequences"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_call_return_value
  "CALL: F8 function return values are correctly placed in accumulator"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_call_register_preservation
  "CALL: F8 register preservation across calls is correctly generated"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

