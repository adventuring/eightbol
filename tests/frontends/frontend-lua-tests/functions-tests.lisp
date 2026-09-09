;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lua -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LUA Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LUA language functions.
;;; See: src/frontend-lua/lua-parser.lisp

(in-package :eightbol/test/frontend-lua)

(fiveam:def-suite :lua-functions
  :description "LUA functions tests"
  :in :frontend-lua)

(in-suite :lua-functions)


(test lua_functions_library
  "FUNCTIONS: LUA library function calls are correctly identified"
  (skip "Implementation pending"))

(test lua_functions_user_defined
  "FUNCTIONS: LUA user-defined function definitions are correctly parsed"
  (skip "Implementation pending"))

(test lua_functions_recursion
  "FUNCTIONS: LUA recursive functions are correctly handled"
  (skip "Implementation pending"))

(test lua_functions_return_values
  "FUNCTIONS: LUA function return values are correctly captured"
  (skip "Implementation pending"))

(test lua_functions_parameter_passing
  "FUNCTIONS: LUA parameters are correctly passed (by value, by reference)"
  (skip "Implementation pending"))

