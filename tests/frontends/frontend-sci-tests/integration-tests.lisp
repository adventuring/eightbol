;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-sci -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCI Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCI language integration.
;;; See: src/frontend-sci/

(in-package :eightbol/test/frontend-sci)

(fiveam:def-suite :sci-integration
  :description "SCI integration tests"
  :in :frontend-sci)

(in-suite :sci-integration)


(test sci_integration_multi_statement
  "INTEGRATION: SCI programs with multiple statement types compile correctly"
  (skip "Implementation pending"))

(test sci_integration_nested_structures
  "INTEGRATION: SCI nested control structures (if/loops) are correctly compiled"
  (skip "Implementation pending"))

(test sci_integration_mixed_types
  "INTEGRATION: SCI programs mixing different numeric types work correctly"
  (skip "Implementation pending"))

(test sci_integration_real_world_example
  "INTEGRATION: Real SCI program compiles without errors"
  (skip "Implementation pending"))

