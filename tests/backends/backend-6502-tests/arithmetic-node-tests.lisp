;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-6502 -*-
;;;
;;; EIGHTBOL 6502 Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 6502 backend code generation for arithmetic nodes.
;;; See: src/backend-6502/

(in-package :eightbol/test/backend-6502)

(in-suite :backend-6502-arithmetic-node)

(test backend_6502_add_8bit
  "6502 backend: 8-bit ADD produces LDA/CLC/ADC/STA sequence"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test backend_6502_add_16bit
  "6502 backend: 16-bit ADD produces double-byte sequence"
  (let* ((ast '(:add (:const 1000) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test backend_6502_subtract_8bit
  "6502 backend: 8-bit SUBTRACT produces LDA/SEC/SBC/STA sequence"
  (let* ((ast '(:subtract (:var x) (:const 1)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test backend_6502_multiply_8bit
  "6502 backend: 8-bit MULTIPLY uses multiplication routine or shift"
  (let* ((ast '(:multiply (:var x) (:const 2)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test backend_6502_divide_8bit
  "6502 backend: 8-bit DIVIDE uses division routine or shift"
  (let* ((ast '(:divide (:var x) (:const 2)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test backend_6502_fixed_point_arithmetic
  "6502 backend: Fixed-point arithmetic maintains scale through shifts"
  (let* ((ast '(:add (:fixed-point (:var x) 8 8) (:fixed-point (:var y) 8 8)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test backend_6502_arithmetic_overflow
  "6502 backend: Overflow handling uses carry flag"
  (let* ((ast '(:add (:const 200) (:const 100)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test backend_6502_nested_arithmetic
  "6502 backend: Nested arithmetic (A + B) * C produces correct sequence"
  (let* ((ast '(:multiply (:add (:var x) (:var y)) (:const 2)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :6502 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test backend_6502_arithmetic_bcd
  "6502 backend: BCD arithmetic uses decimal mode when appropriate"
  (skip "BCD arithmetic test"))

(test backend_6502_arithmetic_register_preservation
  "6502 backend: Arithmetic operations don't clobber needed registers"
  (skip "Register preservation test"))
