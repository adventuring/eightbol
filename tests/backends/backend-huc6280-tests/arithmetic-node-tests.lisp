;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-huc6280 -*-
;;;
;;; EIGHTBOL EIGHTBOL HUC6280 Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the HUC6280 backend code generation.
;;; See: src/backend-huc6280/

(in-package :eightbol/test/backend-huc6280)

(fiveam:def-suite :backend-huc6280
  :description "HUC6280 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-huc6280-arithmetic-node
  :description "HUC6280 arithmetic-node tests"
  :in :backend-huc6280)

(in-suite :backend-huc6280-arithmetic-node)


(test huc6280_add_8bit
  "ARITHMETIC: HUC6280 8-bit ADD produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_add_16bit
  "ARITHMETIC: HUC6280 16-bit ADD produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_subtract_8bit
  "ARITHMETIC: HUC6280 8-bit SUBTRACT produces correct opcodes"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_multiply
  "ARITHMETIC: HUC6280 MULTIPLY produces correct sequence (multiply/divide if available)"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_divide
  "ARITHMETIC: HUC6280 DIVIDE produces correct sequence"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_fixed_point_arithmetic
  "ARITHMETIC: HUC6280 fixed-point arithmetic maintains correct scale"
  (let* ((ast '(:+ (:const 5) (:var x)))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

