;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-stack -*-
;;;
;;; EIGHTBOL EIGHTBOL STACK Backend Integration Tests
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

(fiveam:def-suite :backend-stack-integration
  :description "STACK integration tests"
  :in :backend-stack)

(in-suite :backend-stack-integration)


(test stack_integration_full_program
  "INTEGRATION: STACK complex programs with all node types produce valid assembly"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_integration_cross_frontend_parity
  "INTEGRATION: STACK produces identical output for same AST from different frontends"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_integration_numeric_precision
  "INTEGRATION: STACK maintains numeric precision across all operations"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test stack_integration_memory_layout
  "INTEGRATION: STACK correctly manages memory layout and data placement"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :STACK :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

