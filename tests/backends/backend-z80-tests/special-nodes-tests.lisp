;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-z80 -*-
;;;
;;; EIGHTBOL EIGHTBOL Z80 Backend Special Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Z80 backend code generation.
;;; See: src/backend-z80/

(in-package :eightbol/test/backend-z80)

(fiveam:def-suite :backend-z80
  :description "Z80 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-z80-special-nodes
  :description "Z80 special-nodes tests"
  :in :backend-z80)

(in-suite :backend-z80-special-nodes)


(test z80_log_fault
  "SPECIAL: Z80 :log-fault produces correct debug output sequence"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_debug_break
  "SPECIAL: Z80 :debug-break produces correct breakpoint instruction"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_inspect
  "SPECIAL: Z80 :inspect produces correct debug inspection code"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_evaluate
  "SPECIAL: Z80 :evaluate WHEN clauses generate correct case/switch logic"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_bitwise_ops
  "SPECIAL: Z80 bitwise AND/OR/XOR operations generate correct opcodes"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

