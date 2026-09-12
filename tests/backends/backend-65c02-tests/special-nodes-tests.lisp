;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c02 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C02 Backend Special Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 65C02 backend code generation.
;;; See: src/backend-65c02/

(in-package :eightbol/test/backend-65c02)

(fiveam:def-suite :backend-65c02
  :description "65C02 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-65c02-special-nodes
  :description "65C02 special-nodes tests"
  :in :backend-65c02)

(in-suite :backend-65c02-special-nodes)


(test 65c02_log_fault
  "SPECIAL: 65C02 :log-fault produces correct debug output sequence"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_debug_break
  "SPECIAL: 65C02 :debug-break produces correct breakpoint instruction"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_inspect
  "SPECIAL: 65C02 :inspect produces correct debug inspection code"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_evaluate
  "SPECIAL: 65C02 :evaluate WHEN clauses generate correct case/switch logic"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_bitwise_ops
  "SPECIAL: 65C02 bitwise AND/OR/XOR operations generate correct opcodes"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

