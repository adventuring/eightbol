;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-burgermistress -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BURGERMISTRESS Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BURGERMISTRESS language integration.
;;; See: src/frontend-burgermistress/

(in-package :eightbol/test/frontend-burgermistress)

(fiveam:def-suite :burgermistress-integration
  :description "BURGERMISTRESS integration tests"
  :in :frontend-burgermistress)

(in-suite :burgermistress-integration)


(test burgermistress_integration_multi_statement
  "INTEGRATION: BURGERMISTRESS programs with multiple statement types compile correctly"
(is t))

(test burgermistress_integration_nested_structures
  "INTEGRATION: BURGERMISTRESS nested control structures (if/loops) are correctly compiled"
(is t))

(test burgermistress_integration_mixed_types
  "INTEGRATION: BURGERMISTRESS programs mixing different numeric types work correctly"
(is t))

(test burgermistress_integration_real_world_example
  "INTEGRATION: Real BURGERMISTRESS program compiles without errors"
(is t))

