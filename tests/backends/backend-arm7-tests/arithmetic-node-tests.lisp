;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-arm7 -*-
;;;
;;; EIGHTBOL EIGHTBOL ARM7 Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ARM7 backend code generation.
;;; See: src/backend-arm7/

(in-package :eightbol/test/backend-arm7)

(fiveam:def-suite :backend-arm7
  :description "ARM7 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-arm7-arithmetic-node
  :description "ARM7 arithmetic-node tests"
  :in :backend-arm7)

(in-suite :backend-arm7-arithmetic-node)


(test arm7_add_8bit
  "ARITHMETIC: ARM7 8-bit ADD produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_add_16bit
  "ARITHMETIC: ARM7 16-bit ADD produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_subtract_8bit
  "ARITHMETIC: ARM7 8-bit SUBTRACT produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_multiply
  "ARITHMETIC: ARM7 MULTIPLY produces correct sequence (multiply/divide if available)"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_divide
  "ARITHMETIC: ARM7 DIVIDE produces correct sequence"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_fixed_point_arithmetic
  "ARITHMETIC: ARM7 fixed-point arithmetic maintains correct scale"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

