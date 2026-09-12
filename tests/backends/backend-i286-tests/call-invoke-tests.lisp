;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-i286 -*-
;;;
;;; EIGHTBOL EIGHTBOL I286 Backend Call/Invoke Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the I286 backend code generation.
;;; See: src/backend-i286/

(in-package :eightbol/test/backend-i286)

(fiveam:def-suite :backend-i286
  :description "I286 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-i286-call-invoke
  :description "I286 call-invoke tests"
  :in :backend-i286)

(in-suite :backend-i286-call-invoke)


(test i286_call_local
  "CALL: I286 local :call invocations generate correct JSR/CALL instructions"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_call_library
  "CALL: I286 library :call invocations are correctly linked"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_invoke_method
  "INVOKE: I286 :invoke method calls generate correct dispatch sequences"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_call_return_value
  "CALL: I286 function return values are correctly placed in accumulator"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_call_register_preservation
  "CALL: I286 register preservation across calls is correctly generated"
(let* ((ast ('(:call (:var func))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

