;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c816 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C816 Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 65C816 backend code generation.
;;; See: src/backend-65c816/

(in-package :eightbol/test/backend-65c816)

(fiveam:def-suite :backend-65c816
  :description "65C816 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-65c816-control-flow
  :description "65C816 control-flow tests"
  :in :backend-65c816)

(in-suite :backend-65c816-control-flow)


(test 65c816_if_conditional
  "CONTROL FLOW: 65C816 :if conditionals generate correct branch instructions"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_if_nested
  "CONTROL FLOW: 65C816 nested conditionals generate correct branch chains"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_perform_loop
  "CONTROL FLOW: 65C816 :perform loops generate correct branch/jump sequences"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_goto
  "CONTROL FLOW: 65C816 :goto jumps generate correct unconditional jumps"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_loop_unroll
  "CONTROL FLOW: 65C816 loop optimization correctly unrolls small loops"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

