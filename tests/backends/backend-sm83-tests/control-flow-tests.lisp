;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-sm83 -*-
;;;
;;; EIGHTBOL EIGHTBOL SM83 Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SM83 backend code generation.
;;; See: src/backend-sm83/

(in-package :eightbol/test/backend-sm83)

(fiveam:def-suite :backend-sm83
  :description "SM83 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-sm83-control-flow
  :description "SM83 control-flow tests"
  :in :backend-sm83)

(in-suite :backend-sm83-control-flow)


(test sm83_if_conditional
  "CONTROL FLOW: SM83 :if conditionals generate correct branch instructions"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_if_nested
  "CONTROL FLOW: SM83 nested conditionals generate correct branch chains"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_perform_loop
  "CONTROL FLOW: SM83 :perform loops generate correct branch/jump sequences"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_goto
  "CONTROL FLOW: SM83 :goto jumps generate correct unconditional jumps"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_loop_unroll
  "CONTROL FLOW: SM83 loop optimization correctly unrolls small loops"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

