;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-smalltalk -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SMALLTALK Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SMALLTALK language integration.
;;; See: src/frontend-smalltalk/

(in-package :eightbol/test/frontend-smalltalk)

(fiveam:def-suite :smalltalk-integration
  :description "SMALLTALK integration tests"
  :in :frontend-smalltalk)

(in-suite :smalltalk-integration)


(test smalltalk_integration_multi_statement
  "INTEGRATION: SMALLTALK programs with multiple statement types compile correctly"
  (skip "Implementation pending"))

(test smalltalk_integration_nested_structures
  "INTEGRATION: SMALLTALK nested control structures (if/loops) are correctly compiled"
  (skip "Implementation pending"))

(test smalltalk_integration_mixed_types
  "INTEGRATION: SMALLTALK programs mixing different numeric types work correctly"
  (skip "Implementation pending"))

(test smalltalk_integration_real_world_example
  "INTEGRATION: Real SMALLTALK program compiles without errors"
  (skip "Implementation pending"))

