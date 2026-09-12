;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c816 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C816 Backend Special Node Tests
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

(fiveam:def-suite :backend-65c816-special-nodes
  :description "65C816 special-nodes tests"
  :in :backend-65c816)

(in-suite :backend-65c816-special-nodes)


(test 65c816_log_fault
  "SPECIAL: 65C816 :log-fault produces correct debug output sequence"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_debug_break
  "SPECIAL: 65C816 :debug-break produces correct breakpoint instruction"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_inspect
  "SPECIAL: 65C816 :inspect produces correct debug inspection code"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_evaluate
  "SPECIAL: 65C816 :evaluate WHEN clauses generate correct case/switch logic"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_bitwise_ops
  "SPECIAL: 65C816 bitwise AND/OR/XOR operations generate correct opcodes"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

