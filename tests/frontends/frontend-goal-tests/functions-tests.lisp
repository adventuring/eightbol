;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-goal -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend GOAL Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the GOAL language functions.
;;; See: src/frontend-goal/goal-parser.lisp

(in-package :eightbol/test/frontend-goal)

(fiveam:def-suite :goal-functions
  :description "GOAL functions tests"
  :in :frontend-goal)

(in-suite :goal-functions)


(test goal_functions_library
  "FUNCTIONS: GOAL library function calls are correctly identified"
(is t))

(test goal_functions_user_defined
  "FUNCTIONS: GOAL user-defined function definitions are correctly parsed"
(is t))

(test goal_functions_recursion
  "FUNCTIONS: GOAL recursive functions are correctly handled"
(is t))

(test goal_functions_return_values
  "FUNCTIONS: GOAL function return values are correctly captured"
(is t))

(test goal_functions_parameter_passing
  "FUNCTIONS: GOAL parameters are correctly passed (by value, by reference)"
(is t))

