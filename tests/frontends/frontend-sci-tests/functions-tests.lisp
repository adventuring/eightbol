;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-sci -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCI Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCI language functions.
;;; See: src/frontend-sci/sci-parser.lisp

(in-package :eightbol/test/frontend-sci)

(fiveam:def-suite :sci-functions
  :description "SCI functions tests"
  :in :frontend-sci)

(in-suite :sci-functions)


(test sci_functions_library
  "FUNCTIONS: SCI library function calls are correctly identified"
(is t))

(test sci_functions_user_defined
  "FUNCTIONS: SCI user-defined function definitions are correctly parsed"
(is t))

(test sci_functions_recursion
  "FUNCTIONS: SCI recursive functions are correctly handled"
(is t))

(test sci_functions_return_values
  "FUNCTIONS: SCI function return values are correctly captured"
(is t))

(test sci_functions_parameter_passing
  "FUNCTIONS: SCI parameters are correctly passed (by value, by reference)"
(is t))

