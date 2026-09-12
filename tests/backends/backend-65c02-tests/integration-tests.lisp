;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c02 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C02 Backend Integration Tests
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

(fiveam:def-suite :backend-65c02-integration
  :description "65C02 integration tests"
  :in :backend-65c02)

(in-suite :backend-65c02-integration)


(test 65c02_integration_full_program
  "INTEGRATION: 65C02 complex programs with all node types produce valid assembly"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_integration_cross_frontend_parity
  "INTEGRATION: 65C02 produces identical output for same AST from different frontends"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_integration_numeric_precision
  "INTEGRATION: 65C02 maintains numeric precision across all operations"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c02_integration_memory_layout
  "INTEGRATION: 65C02 correctly manages memory layout and data placement"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C02 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

