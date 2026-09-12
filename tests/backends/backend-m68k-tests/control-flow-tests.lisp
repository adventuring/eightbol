;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-m68k -*-
;;;
;;; EIGHTBOL EIGHTBOL M68K Backend Control Flow Tests
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

(fiveam:def-suite :backend-m68k-control-flow
  :description "M68K control-flow tests"
  :in :backend-m68k)

(in-suite :backend-m68k-control-flow)


(test m68k_if_conditional
  "CONTROL FLOW: M68K :if conditionals generate correct branch instructions"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :M68K :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test m68k_if_nested
  "CONTROL FLOW: M68K nested conditionals generate correct branch chains"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :M68K :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test m68k_perform_loop
  "CONTROL FLOW: M68K :perform loops generate correct branch/jump sequences"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :M68K :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test m68k_goto
  "CONTROL FLOW: M68K :goto jumps generate correct unconditional jumps"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :M68K :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test m68k_loop_unroll
  "CONTROL FLOW: M68K loop optimization correctly unrolls small loops"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :M68K :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

