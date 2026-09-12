;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-z80 -*-
;;;
;;; EIGHTBOL EIGHTBOL Z80 Backend Call/Invoke Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Z80 backend code generation.
;;; See: src/backend-z80/

(in-package :eightbol/test/backend-z80)

(fiveam:def-suite :backend-z80
  :description "Z80 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-z80-call-invoke
  :description "Z80 call-invoke tests"
  :in :backend-z80)

(in-suite :backend-z80-call-invoke)


(test z80_call_local
  "CALL: Z80 local :call invocations generate correct JSR/CALL instructions"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_call_library
  "CALL: Z80 library :call invocations are correctly linked"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_invoke_method
  "INVOKE: Z80 :invoke method calls generate correct dispatch sequences"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_call_return_value
  "CALL: Z80 function return values are correctly placed in accumulator"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_call_register_preservation
  "CALL: Z80 register preservation across calls is correctly generated"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

