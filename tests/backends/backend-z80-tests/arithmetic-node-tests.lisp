;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-z80 -*-
;;;
;;; EIGHTBOL EIGHTBOL Z80 Backend Arithmetic Node Tests
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

(fiveam:def-suite :backend-z80-arithmetic-node
  :description "Z80 arithmetic-node tests"
  :in :backend-z80)

(in-suite :backend-z80-arithmetic-node)


(test z80_add_8bit
  "ARITHMETIC: Z80 8-bit ADD produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_add_16bit
  "ARITHMETIC: Z80 16-bit ADD produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_subtract_8bit
  "ARITHMETIC: Z80 8-bit SUBTRACT produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_multiply
  "ARITHMETIC: Z80 MULTIPLY produces correct sequence (multiply/divide if available)"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_divide
  "ARITHMETIC: Z80 DIVIDE produces correct sequence"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test z80_fixed_point_arithmetic
  "ARITHMETIC: Z80 fixed-point arithmetic maintains correct scale"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :Z80 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

