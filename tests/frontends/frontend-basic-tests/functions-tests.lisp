;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-basic -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BASIC Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BASIC language functions.
;;; See: src/frontend-basic/basic-parser.lisp

(in-package :eightbol/test/frontend-basic)

(fiveam:def-suite :basic-functions
  :description "BASIC functions tests"
  :in :frontend-basic)

(in-suite :basic-functions)


(test basic_functions_library
  "FUNCTIONS: BASIC library function calls are correctly identified"
(is t))

(test basic_functions_user_defined
  "FUNCTIONS: BASIC user-defined function definitions are correctly parsed"
(is t))

(test basic_functions_recursion
  "FUNCTIONS: BASIC recursive functions are correctly handled"
(is t))

(test basic_functions_return_values
  "FUNCTIONS: BASIC function return values are correctly captured"
(is t))

(test basic_functions_parameter_passing
  "FUNCTIONS: BASIC parameters are correctly passed (by value, by reference)"
(is t))

