;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lingo -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LINGO Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LINGO language integration.
;;; See: src/frontend-lingo/

(in-package :eightbol/test/frontend-lingo)

(fiveam:def-suite :lingo-integration
  :description "LINGO integration tests"
  :in :frontend-lingo)

(in-suite :lingo-integration)


(test lingo_integration_multi_statement
  "INTEGRATION: LINGO programs with multiple statement types compile correctly"
  (skip "Implementation pending"))

(test lingo_integration_nested_structures
  "INTEGRATION: LINGO nested control structures (if/loops) are correctly compiled"
  (skip "Implementation pending"))

(test lingo_integration_mixed_types
  "INTEGRATION: LINGO programs mixing different numeric types work correctly"
  (skip "Implementation pending"))

(test lingo_integration_real_world_example
  "INTEGRATION: Real LINGO program compiles without errors"
  (skip "Implementation pending"))

