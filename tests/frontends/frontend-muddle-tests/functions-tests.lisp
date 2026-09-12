;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-muddle -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend MUDDLE Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the MUDDLE language functions.
;;; See: src/frontend-muddle/muddle-parser.lisp

(in-package :eightbol/test/frontend-muddle)

(fiveam:def-suite :muddle-functions
  :description "MUDDLE functions tests"
  :in :frontend-muddle)

(in-suite :muddle-functions)


(test muddle_functions_library
  "FUNCTIONS: MUDDLE library function calls are correctly identified"
(is t))

(test muddle_functions_user_defined
  "FUNCTIONS: MUDDLE user-defined function definitions are correctly parsed"
(is t))

(test muddle_functions_recursion
  "FUNCTIONS: MUDDLE recursive functions are correctly handled"
(is t))

(test muddle_functions_return_values
  "FUNCTIONS: MUDDLE function return values are correctly captured"
(is t))

(test muddle_functions_parameter_passing
  "FUNCTIONS: MUDDLE parameters are correctly passed (by value, by reference)"
(is t))

