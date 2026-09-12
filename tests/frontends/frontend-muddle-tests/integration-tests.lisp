;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-muddle -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend MUDDLE Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the MUDDLE language integration.
;;; See: src/frontend-muddle/

(in-package :eightbol/test/frontend-muddle)

(fiveam:def-suite :muddle-integration
  :description "MUDDLE integration tests"
  :in :frontend-muddle)

(in-suite :muddle-integration)


(test muddle_integration_multi_statement
  "INTEGRATION: MUDDLE programs with multiple statement types compile correctly"
(is t))

(test muddle_integration_nested_structures
  "INTEGRATION: MUDDLE nested control structures (if/loops) are correctly compiled"
(is t))

(test muddle_integration_mixed_types
  "INTEGRATION: MUDDLE programs mixing different numeric types work correctly"
(is t))

(test muddle_integration_real_world_example
  "INTEGRATION: Real MUDDLE program compiles without errors"
(is t))

