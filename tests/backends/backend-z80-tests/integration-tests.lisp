;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-z80 -*-
;;;
;;; EIGHTBOL EIGHTBOL Z80 Backend Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Z80 backend code generation.
;;; See: src/backend-z80/

(in-package :eightbol/test/backend-z80)

(fiveam:def-suite :backend-z80
  :description "Z80 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-z80-integration
  :description "Z80 integration tests"
  :in :backend-z80)

(in-suite :backend-z80-integration)


(test z80_integration_full_program
  "INTEGRATION: Z80 complex programs with all node types produce valid assembly"
  (skip "Implementation pending"))

(test z80_integration_cross_frontend_parity
  "INTEGRATION: Z80 produces identical output for same AST from different frontends"
  (skip "Implementation pending"))

(test z80_integration_numeric_precision
  "INTEGRATION: Z80 maintains numeric precision across all operations"
  (skip "Implementation pending"))

(test z80_integration_memory_layout
  "INTEGRATION: Z80 correctly manages memory layout and data placement"
  (skip "Implementation pending"))

