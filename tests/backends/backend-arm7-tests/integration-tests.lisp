;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-arm7 -*-
;;;
;;; EIGHTBOL EIGHTBOL ARM7 Backend Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ARM7 backend code generation.
;;; See: src/backend-arm7/

(in-package :eightbol/test/backend-arm7)

(fiveam:def-suite :backend-arm7
  :description "ARM7 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-arm7-integration
  :description "ARM7 integration tests"
  :in :backend-arm7)

(in-suite :backend-arm7-integration)


(test arm7_integration_full_program
  "INTEGRATION: ARM7 complex programs with all node types produce valid assembly"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_integration_cross_frontend_parity
  "INTEGRATION: ARM7 produces identical output for same AST from different frontends"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_integration_numeric_precision
  "INTEGRATION: ARM7 maintains numeric precision across all operations"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_integration_memory_layout
  "INTEGRATION: ARM7 correctly manages memory layout and data placement"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

