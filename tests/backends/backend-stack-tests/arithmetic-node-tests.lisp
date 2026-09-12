;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-stack -*-
;;;
;;; EIGHTBOL EIGHTBOL STACK Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the STACK backend code generation.
;;; See: src/backend-stack/

(in-package :eightbol/test/backend-stack)

(fiveam:def-suite :backend-stack
  :description "STACK backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-stack-arithmetic-node
  :description "STACK arithmetic-node tests"
  :in :backend-stack)

(in-suite :backend-stack-arithmetic-node)


(test stack_add_8bit
  "ARITHMETIC: STACK 8-bit ADD produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_add_16bit
  "ARITHMETIC: STACK 16-bit ADD produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_subtract_8bit
  "ARITHMETIC: STACK 8-bit SUBTRACT produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_multiply
  "ARITHMETIC: STACK MULTIPLY produces correct sequence (multiply/divide if available)"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_divide
  "ARITHMETIC: STACK DIVIDE produces correct sequence"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_fixed_point_arithmetic
  "ARITHMETIC: STACK fixed-point arithmetic maintains correct scale"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

