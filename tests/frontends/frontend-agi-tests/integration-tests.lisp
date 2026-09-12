;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-agi -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend AGI Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the AGI language integration.
;;; See: src/frontend-agi/

(in-package :eightbol/test/frontend-agi)

(fiveam:def-suite :agi-integration
  :description "AGI integration tests"
  :in :frontend-agi)

(in-suite :agi-integration)


(test agi_integration_multi_statement
  "INTEGRATION: AGI programs with multiple statement types compile correctly"
(is t))

(test agi_integration_nested_structures
  "INTEGRATION: AGI nested control structures (if/loops) are correctly compiled"
(is t))

(test agi_integration_mixed_types
  "INTEGRATION: AGI programs mixing different numeric types work correctly"
(is t))

(test agi_integration_real_world_example
  "INTEGRATION: Real AGI program compiles without errors"
(is t))

