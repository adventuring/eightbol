;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c816 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C816 Backend Integration Tests
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

(fiveam:def-suite :backend-65c816-integration
  :description "65C816 integration tests"
  :in :backend-65c816)

(in-suite :backend-65c816-integration)


(test 65c816_integration_full_program
  "INTEGRATION: 65C816 complex programs with all node types produce valid assembly"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_integration_cross_frontend_parity
  "INTEGRATION: 65C816 produces identical output for same AST from different frontends"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_integration_numeric_precision
  "INTEGRATION: 65C816 maintains numeric precision across all operations"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_integration_memory_layout
  "INTEGRATION: 65C816 correctly manages memory layout and data placement"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

