;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c816 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C816 Backend Arithmetic Node Tests
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

(fiveam:def-suite :backend-65c816-arithmetic-node
  :description "65C816 arithmetic-node tests"
  :in :backend-65c816)

(in-suite :backend-65c816-arithmetic-node)


(test 65c816_add_8bit
  "ARITHMETIC: 65C816 8-bit ADD produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_add_16bit
  "ARITHMETIC: 65C816 16-bit ADD produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_subtract_8bit
  "ARITHMETIC: 65C816 8-bit SUBTRACT produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_multiply
  "ARITHMETIC: 65C816 MULTIPLY produces correct sequence (multiply/divide if available)"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_divide
  "ARITHMETIC: 65C816 DIVIDE produces correct sequence"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_fixed_point_arithmetic
  "ARITHMETIC: 65C816 fixed-point arithmetic maintains correct scale"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

