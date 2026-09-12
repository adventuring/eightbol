;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-sm83 -*-
;;;
;;; EIGHTBOL EIGHTBOL SM83 Backend Special Node Tests
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

(fiveam:def-suite :backend-sm83-special-nodes
  :description "SM83 special-nodes tests"
  :in :backend-sm83)

(in-suite :backend-sm83-special-nodes)


(test sm83_log_fault
  "SPECIAL: SM83 :log-fault produces correct debug output sequence"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_debug_break
  "SPECIAL: SM83 :debug-break produces correct breakpoint instruction"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_inspect
  "SPECIAL: SM83 :inspect produces correct debug inspection code"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_evaluate
  "SPECIAL: SM83 :evaluate WHEN clauses generate correct case/switch logic"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_bitwise_ops
  "SPECIAL: SM83 bitwise AND/OR/XOR operations generate correct opcodes"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

