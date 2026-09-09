;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-forth -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FORTH Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FORTH language functions.
;;; See: src/frontend-forth/forth-parser.lisp

(in-package :eightbol/test/frontend-forth)

(fiveam:def-suite :forth-functions
  :description "FORTH functions tests"
  :in :frontend-forth)

(in-suite :forth-functions)


(test forth_functions_library
  "FUNCTIONS: FORTH library function calls are correctly identified"
  (skip "Implementation pending"))

(test forth_functions_user_defined
  "FUNCTIONS: FORTH user-defined function definitions are correctly parsed"
  (skip "Implementation pending"))

(test forth_functions_recursion
  "FUNCTIONS: FORTH recursive functions are correctly handled"
  (skip "Implementation pending"))

(test forth_functions_return_values
  "FUNCTIONS: FORTH function return values are correctly captured"
  (skip "Implementation pending"))

(test forth_functions_parameter_passing
  "FUNCTIONS: FORTH parameters are correctly passed (by value, by reference)"
  (skip "Implementation pending"))

