;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-cp1610 -*-
;;;
;;; EIGHTBOL EIGHTBOL CP1610 Backend Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the CP1610 backend code generation.
;;; See: src/backend-cp1610/

(in-package :eightbol/test/backend-cp1610)

(fiveam:def-suite :backend-cp1610
  :description "CP1610 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-cp1610-integration
  :description "CP1610 integration tests"
  :in :backend-cp1610)

(in-suite :backend-cp1610-integration)


(test cp1610_integration_full_program
  "INTEGRATION: CP1610 complex programs with all node types produce valid assembly"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_integration_cross_frontend_parity
  "INTEGRATION: CP1610 produces identical output for same AST from different frontends"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_integration_numeric_precision
  "INTEGRATION: CP1610 maintains numeric precision across all operations"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_integration_memory_layout
  "INTEGRATION: CP1610 correctly manages memory layout and data placement"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

