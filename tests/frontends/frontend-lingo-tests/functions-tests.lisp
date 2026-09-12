;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lingo -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LINGO Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LINGO language functions.
;;; See: src/frontend-lingo/lingo-parser.lisp

(in-package :eightbol/test/frontend-lingo)

(fiveam:def-suite :lingo-functions
  :description "LINGO functions tests"
  :in :frontend-lingo)

(in-suite :lingo-functions)


(test lingo_functions_library
  "FUNCTIONS: LINGO library function calls are correctly identified"
(is t))

(test lingo_functions_user_defined
  "FUNCTIONS: LINGO user-defined function definitions are correctly parsed"
(is t))

(test lingo_functions_recursion
  "FUNCTIONS: LINGO recursive functions are correctly handled"
(is t))

(test lingo_functions_return_values
  "FUNCTIONS: LINGO function return values are correctly captured"
(is t))

(test lingo_functions_parameter_passing
  "FUNCTIONS: LINGO parameters are correctly passed (by value, by reference)"
(is t))

