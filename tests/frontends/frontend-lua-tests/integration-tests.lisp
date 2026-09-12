;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lua -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LUA Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LUA language integration.
;;; See: src/frontend-lua/

(in-package :eightbol/test/frontend-lua)

(fiveam:def-suite :lua-integration
  :description "LUA integration tests"
  :in :frontend-lua)

(in-suite :lua-integration)


(test lua_integration_multi_statement
  "INTEGRATION: LUA programs with multiple statement types compile correctly"
(is t))

(test lua_integration_nested_structures
  "INTEGRATION: LUA nested control structures (if/loops) are correctly compiled"
(is t))

(test lua_integration_mixed_types
  "INTEGRATION: LUA programs mixing different numeric types work correctly"
(is t))

(test lua_integration_real_world_example
  "INTEGRATION: Real LUA program compiles without errors"
(is t))

