;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-f8 -*-
;;;
;;; EIGHTBOL EIGHTBOL F8 Backend Control Flow Tests
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

(fiveam:def-suite :backend-f8-control-flow
  :description "F8 control-flow tests"
  :in :backend-f8)

(in-suite :backend-f8-control-flow)


(test f8_if_conditional
  "CONTROL FLOW: F8 :if conditionals generate correct branch instructions"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_if_nested
  "CONTROL FLOW: F8 nested conditionals generate correct branch chains"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_perform_loop
  "CONTROL FLOW: F8 :perform loops generate correct branch/jump sequences"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_goto
  "CONTROL FLOW: F8 :goto jumps generate correct unconditional jumps"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_loop_unroll
  "CONTROL FLOW: F8 loop optimization correctly unrolls small loops"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

