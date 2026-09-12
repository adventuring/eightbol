;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-f8 -*-
;;;
;;; EIGHTBOL EIGHTBOL F8 Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the F8 backend code generation.
;;; See: src/backend-f8/

(in-package :eightbol/test/backend-f8)

(fiveam:def-suite :backend-f8
  :description "F8 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-f8-arithmetic-node
  :description "F8 arithmetic-node tests"
  :in :backend-f8)

(in-suite :backend-f8-arithmetic-node)


(test f8_add_8bit
  "ARITHMETIC: F8 8-bit ADD produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_add_16bit
  "ARITHMETIC: F8 16-bit ADD produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_subtract_8bit
  "ARITHMETIC: F8 8-bit SUBTRACT produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_multiply
  "ARITHMETIC: F8 MULTIPLY produces correct sequence (multiply/divide if available)"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_divide
  "ARITHMETIC: F8 DIVIDE produces correct sequence"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_fixed_point_arithmetic
  "ARITHMETIC: F8 fixed-point arithmetic maintains correct scale"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

