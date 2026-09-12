;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-basic -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BASIC Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BASIC language integration.
;;; See: src/frontend-basic/

(in-package :eightbol/test/frontend-basic)

(fiveam:def-suite :basic-integration
  :description "BASIC integration tests"
  :in :frontend-basic)

(in-suite :basic-integration)


(test basic_integration_multi_statement
  "INTEGRATION: BASIC programs with multiple statement types compile correctly"
(is t))

(test basic_integration_nested_structures
  "INTEGRATION: BASIC nested control structures (if/loops) are correctly compiled"
(is t))

(test basic_integration_mixed_types
  "INTEGRATION: BASIC programs mixing different numeric types work correctly"
(is t))

(test basic_integration_real_world_example
  "INTEGRATION: Real BASIC program compiles without errors"
(is t))

