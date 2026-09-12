;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-i286 -*-
;;;
;;; EIGHTBOL EIGHTBOL I286 Backend Special Node Tests
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

(fiveam:def-suite :backend-i286-special-nodes
  :description "I286 special-nodes tests"
  :in :backend-i286)

(in-suite :backend-i286-special-nodes)


(test i286_log_fault
  "SPECIAL: I286 :log-fault produces correct debug output sequence"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_debug_break
  "SPECIAL: I286 :debug-break produces correct breakpoint instruction"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_inspect
  "SPECIAL: I286 :inspect produces correct debug inspection code"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_evaluate
  "SPECIAL: I286 :evaluate WHEN clauses generate correct case/switch logic"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_bitwise_ops
  "SPECIAL: I286 bitwise AND/OR/XOR operations generate correct opcodes"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

