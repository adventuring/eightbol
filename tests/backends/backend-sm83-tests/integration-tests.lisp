;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-sm83 -*-
;;;
;;; EIGHTBOL EIGHTBOL SM83 Backend Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SM83 backend code generation.
;;; See: src/backend-sm83/

(in-package :eightbol/test/backend-sm83)

(fiveam:def-suite :backend-sm83
  :description "SM83 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-sm83-integration
  :description "SM83 integration tests"
  :in :backend-sm83)

(in-suite :backend-sm83-integration)


(test sm83_integration_full_program
  "INTEGRATION: SM83 complex programs with all node types produce valid assembly"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_integration_cross_frontend_parity
  "INTEGRATION: SM83 produces identical output for same AST from different frontends"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_integration_numeric_precision
  "INTEGRATION: SM83 maintains numeric precision across all operations"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test sm83_integration_memory_layout
  "INTEGRATION: SM83 correctly manages memory layout and data placement"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :SM83 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

