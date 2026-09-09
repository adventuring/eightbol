;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-i286 -*-
;;;
;;; EIGHTBOL EIGHTBOL I286 Backend Integration Tests
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

(fiveam:def-suite :backend-i286-integration
  :description "I286 integration tests"
  :in :backend-i286)

(in-suite :backend-i286-integration)


(test i286_integration_full_program
  "INTEGRATION: I286 complex programs with all node types produce valid assembly"
  (skip "Implementation pending"))

(test i286_integration_cross_frontend_parity
  "INTEGRATION: I286 produces identical output for same AST from different frontends"
  (skip "Implementation pending"))

(test i286_integration_numeric_precision
  "INTEGRATION: I286 maintains numeric precision across all operations"
  (skip "Implementation pending"))

(test i286_integration_memory_layout
  "INTEGRATION: I286 correctly manages memory layout and data placement"
  (skip "Implementation pending"))

