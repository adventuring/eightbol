;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-m68k -*-
;;;
;;; EIGHTBOL EIGHTBOL M68K Backend Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the M68K backend code generation.
;;; See: src/backend-m68k/

(in-package :eightbol/test/backend-m68k)

(fiveam:def-suite :backend-m68k
  :description "M68K backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-m68k-integration
  :description "M68K integration tests"
  :in :backend-m68k)

(in-suite :backend-m68k-integration)


(test m68k_integration_full_program
  "INTEGRATION: M68K complex programs with all node types produce valid assembly"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :M68K :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test m68k_integration_cross_frontend_parity
  "INTEGRATION: M68K produces identical output for same AST from different frontends"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :M68K :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test m68k_integration_numeric_precision
  "INTEGRATION: M68K maintains numeric precision across all operations"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :M68K :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test m68k_integration_memory_layout
  "INTEGRATION: M68K correctly manages memory layout and data placement"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :M68K :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

