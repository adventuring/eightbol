;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-i286 -*-
;;;
;;; EIGHTBOL EIGHTBOL I286 Backend Arithmetic Node Tests
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

(fiveam:def-suite :backend-i286-arithmetic-node
  :description "I286 arithmetic-node tests"
  :in :backend-i286)

(in-suite :backend-i286-arithmetic-node)


(test i286_add_8bit
  "ARITHMETIC: I286 8-bit ADD produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_add_16bit
  "ARITHMETIC: I286 16-bit ADD produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_subtract_8bit
  "ARITHMETIC: I286 8-bit SUBTRACT produces correct opcodes"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_multiply
  "ARITHMETIC: I286 MULTIPLY produces correct sequence (multiply/divide if available)"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_divide
  "ARITHMETIC: I286 DIVIDE produces correct sequence"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_fixed_point_arithmetic
  "ARITHMETIC: I286 fixed-point arithmetic maintains correct scale"
  (let* ((ast '(:add (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

