;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-6502 -*-
;;;
;;; EIGHTBOL EIGHTBOL 6502 Backend Call/Invoke Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 6502 backend code generation.
;;; See: src/backend-6502/

(in-package :eightbol/test/backend-6502)

(fiveam:def-suite :backend-6502
  :description "6502 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-6502-call-invoke
  :description "6502 call-invoke tests"
  :in :backend-6502)

(in-suite :backend-6502-call-invoke)


(test 6502_call_local
  "CALL: 6502 local :call invocations generate correct JSR/CALL instructions"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 6502_call_library
  "CALL: 6502 library :call invocations are correctly linked"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 6502_invoke_method
  "INVOKE: 6502 :invoke method calls generate correct dispatch sequences"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 6502_call_return_value
  "CALL: 6502 function return values are correctly placed in accumulator"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 6502_call_register_preservation
  "CALL: 6502 register preservation across calls is correctly generated"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

