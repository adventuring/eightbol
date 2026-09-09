;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-agi -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend AGI Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the AGI language functions.
;;; See: src/frontend-agi/agi-parser.lisp

(in-package :eightbol/test/frontend-agi)

(fiveam:def-suite :agi-functions
  :description "AGI functions tests"
  :in :frontend-agi)

(in-suite :agi-functions)


(test agi_functions_library
  "FUNCTIONS: AGI library function calls are correctly identified"
  (skip "Implementation pending"))

(test agi_functions_user_defined
  "FUNCTIONS: AGI user-defined function definitions are correctly parsed"
  (skip "Implementation pending"))

(test agi_functions_recursion
  "FUNCTIONS: AGI recursive functions are correctly handled"
  (skip "Implementation pending"))

(test agi_functions_return_values
  "FUNCTIONS: AGI function return values are correctly captured"
  (skip "Implementation pending"))

(test agi_functions_parameter_passing
  "FUNCTIONS: AGI parameters are correctly passed (by value, by reference)"
  (skip "Implementation pending"))

