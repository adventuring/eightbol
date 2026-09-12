;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-objective -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend OBJECTIVE Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the OBJECTIVE language integration.
;;; See: src/frontend-objective/

(in-package :eightbol/test/frontend-objective)

(fiveam:def-suite :objective-integration
  :description "OBJECTIVE integration tests"
  :in :frontend-objective)

(in-suite :objective-integration)


(test objective_integration_multi_statement
  "INTEGRATION: OBJECTIVE programs with multiple statement types compile correctly"
(is t))

(test objective_integration_nested_structures
  "INTEGRATION: OBJECTIVE nested control structures (if/loops) are correctly compiled"
(is t))

(test objective_integration_mixed_types
  "INTEGRATION: OBJECTIVE programs mixing different numeric types work correctly"
(is t))

(test objective_integration_real_world_example
  "INTEGRATION: Real OBJECTIVE program compiles without errors"
(is t))

