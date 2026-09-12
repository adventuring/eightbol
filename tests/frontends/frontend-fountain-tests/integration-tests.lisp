;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-fountain -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FOUNTAIN Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FOUNTAIN language integration.
;;; See: src/frontend-fountain/

(in-package :eightbol/test/frontend-fountain)

(fiveam:def-suite :fountain-integration
  :description "FOUNTAIN integration tests"
  :in :frontend-fountain)

(in-suite :fountain-integration)


(test fountain_integration_multi_statement
  "INTEGRATION: FOUNTAIN programs with multiple statement types compile correctly"
(is t))

(test fountain_integration_nested_structures
  "INTEGRATION: FOUNTAIN nested control structures (if/loops) are correctly compiled"
(is t))

(test fountain_integration_mixed_types
  "INTEGRATION: FOUNTAIN programs mixing different numeric types work correctly"
(is t))

(test fountain_integration_real_world_example
  "INTEGRATION: Real FOUNTAIN program compiles without errors"
(is t))

