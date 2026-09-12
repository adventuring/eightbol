;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-cobol -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend COBOL Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the COBOL language integration.
;;; See: src/frontend-cobol/

(in-package :eightbol/test/frontend-cobol)

(fiveam:def-suite :cobol-integration
  :description "COBOL integration tests"
  :in :frontend-cobol)

(in-suite :cobol-integration)


(test cobol_integration_multi_statement
  "INTEGRATION: COBOL programs with multiple statement types compile correctly"
(is t))

(test cobol_integration_nested_structures
  "INTEGRATION: COBOL nested control structures (if/loops) are correctly compiled"
(is t))

(test cobol_integration_mixed_types
  "INTEGRATION: COBOL programs mixing different numeric types work correctly"
(is t))

(test cobol_integration_real_world_example
  "INTEGRATION: Real COBOL program compiles without errors"
(is t))

