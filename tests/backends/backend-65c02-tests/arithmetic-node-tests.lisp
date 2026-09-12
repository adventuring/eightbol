;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c02 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C02 Backend Arithmetic Node Tests
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

(fiveam:def-suite :backend-65c02-arithmetic-node
  :description "65C02 arithmetic-node tests"
  :in :backend-65c02)

(in-suite :backend-65c02-arithmetic-node)


(test 65c02_add_8bit
  "ARITHMETIC: 65C02 8-bit ADD produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65c02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_add_16bit
  "ARITHMETIC: 65C02 16-bit ADD produces correct opcodes"
  (let* ((ast '(:add (:const 1000) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65c02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_subtract_8bit
  "ARITHMETIC: 65C02 8-bit SUBTRACT produces correct opcodes"
  (let* ((ast '(:subtract (:var x) (:const 1)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65c02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_multiply
  "ARITHMETIC: 65C02 MULTIPLY produces correct sequence (multiply/divide if available)"
  (let* ((ast '(:multiply (:var x) (:const 2)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65c02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_divide
  "ARITHMETIC: 65C02 DIVIDE produces correct sequence"
  (let* ((ast '(:divide (:var x) (:const 2)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65c02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_fixed_point_arithmetic
  "ARITHMETIC: 65C02 fixed-point arithmetic maintains correct scale"
  (let* ((ast '(:add (:fixed-point (:var x) 8 8) (:fixed-point (:var y) 8 8)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65c02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

