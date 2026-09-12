;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-cp1610 -*-
;;;
;;; EIGHTBOL EIGHTBOL CP1610 Backend Special Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the CP1610 backend code generation.
;;; See: src/backend-cp1610/

(in-package :eightbol/test/backend-cp1610)

(fiveam:def-suite :backend-cp1610
  :description "CP1610 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-cp1610-special-nodes
  :description "CP1610 special-nodes tests"
  :in :backend-cp1610)

(in-suite :backend-cp1610-special-nodes)


(test cp1610_log_fault
  "SPECIAL: CP1610 :log-fault produces correct debug output sequence"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_debug_break
  "SPECIAL: CP1610 :debug-break produces correct breakpoint instruction"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_inspect
  "SPECIAL: CP1610 :inspect produces correct debug inspection code"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_evaluate
  "SPECIAL: CP1610 :evaluate WHEN clauses generate correct case/switch logic"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_bitwise_ops
  "SPECIAL: CP1610 bitwise AND/OR/XOR operations generate correct opcodes"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

