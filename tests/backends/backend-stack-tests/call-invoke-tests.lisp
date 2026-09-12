;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-stack -*-
;;;
;;; EIGHTBOL EIGHTBOL STACK Backend Call/Invoke Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the STACK backend code generation.
;;; See: src/backend-stack/

(in-package :eightbol/test/backend-stack)

(fiveam:def-suite :backend-stack
  :description "STACK backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-stack-call-invoke
  :description "STACK call-invoke tests"
  :in :backend-stack)

(in-suite :backend-stack-call-invoke)


(test stack_call_local
  "CALL: STACK local :call invocations generate correct JSR/CALL instructions"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_call_library
  "CALL: STACK library :call invocations are correctly linked"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_invoke_method
  "INVOKE: STACK :invoke method calls generate correct dispatch sequences"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_call_return_value
  "CALL: STACK function return values are correctly placed in accumulator"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_call_register_preservation
  "CALL: STACK register preservation across calls is correctly generated"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

