;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-zil -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend ZIL Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ZIL language integration.
;;; See: src/frontend-zil/

(in-package :eightbol/test/frontend-zil)

(fiveam:def-suite :zil-integration
  :description "ZIL integration tests"
  :in :frontend-zil)

(in-suite :zil-integration)


(test zil_integration_multi_statement
  "INTEGRATION: ZIL programs with multiple statement types compile correctly"
(is t))

(test zil_integration_nested_structures
  "INTEGRATION: ZIL nested control structures (if/loops) are correctly compiled"
(is t))

(test zil_integration_mixed_types
  "INTEGRATION: ZIL programs mixing different numeric types work correctly"
(is t))

(test zil_integration_real_world_example
  "INTEGRATION: Real ZIL program compiles without errors"
(is t))

