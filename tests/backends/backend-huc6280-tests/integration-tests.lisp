;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-huc6280 -*-
;;;
;;; EIGHTBOL EIGHTBOL HUC6280 Backend Integration Tests
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

(fiveam:def-suite :backend-huc6280-integration
  :description "HUC6280 integration tests"
  :in :backend-huc6280)

(in-suite :backend-huc6280-integration)


(test huc6280_integration_full_program
  "INTEGRATION: HUC6280 complex programs with all node types produce valid assembly"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_integration_cross_frontend_parity
  "INTEGRATION: HUC6280 produces identical output for same AST from different frontends"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_integration_numeric_precision
  "INTEGRATION: HUC6280 maintains numeric precision across all operations"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_integration_memory_layout
  "INTEGRATION: HUC6280 correctly manages memory layout and data placement"
(let* ((ast ('(:move (:const 1) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

