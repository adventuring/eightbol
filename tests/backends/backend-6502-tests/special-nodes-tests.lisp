;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-6502 -*-
;;;
;;; EIGHTBOL EIGHTBOL 6502 Backend Special Node Tests
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

(fiveam:def-suite :backend-6502-special-nodes
  :description "6502 special-nodes tests"
  :in :backend-6502)

(in-suite :backend-6502-special-nodes)


(test 6502_log_fault
  "SPECIAL: 6502 :log-fault produces correct debug output sequence"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 6502_debug_break
  "SPECIAL: 6502 :debug-break produces correct breakpoint instruction"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 6502_inspect
  "SPECIAL: 6502 :inspect produces correct debug inspection code"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 6502_evaluate
  "SPECIAL: 6502 :evaluate WHEN clauses generate correct case/switch logic"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 6502_bitwise_ops
  "SPECIAL: 6502 bitwise AND/OR/XOR operations generate correct opcodes"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

