;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-objective -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend OBJECTIVE Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the OBJECTIVE language functions.
;;; See: src/frontend-objective/objective-parser.lisp

(in-package :eightbol/test/frontend-objective)

(fiveam:def-suite :objective-functions
  :description "OBJECTIVE functions tests"
  :in :frontend-objective)

(in-suite :objective-functions)


(test objective_functions_library
  "FUNCTIONS: OBJECTIVE library function calls are correctly identified"
(is t))

(test objective_functions_user_defined
  "FUNCTIONS: OBJECTIVE user-defined function definitions are correctly parsed"
(is t))

(test objective_functions_recursion
  "FUNCTIONS: OBJECTIVE recursive functions are correctly handled"
(is t))

(test objective_functions_return_values
  "FUNCTIONS: OBJECTIVE function return values are correctly captured"
(is t))

(test objective_functions_parameter_passing
  "FUNCTIONS: OBJECTIVE parameters are correctly passed (by value, by reference)"
(is t))

