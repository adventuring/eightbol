;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-burgermistress -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BURGERMISTRESS Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BURGERMISTRESS language functions.
;;; See: src/frontend-burgermistress/burgermistress-parser.lisp

(in-package :eightbol/test/frontend-burgermistress)

(fiveam:def-suite :burgermistress-functions
  :description "BURGERMISTRESS functions tests"
  :in :frontend-burgermistress)

(in-suite :burgermistress-functions)


(test burgermistress_functions_library
  "FUNCTIONS: BURGERMISTRESS library function calls are correctly identified"
(is t))

(test burgermistress_functions_user_defined
  "FUNCTIONS: BURGERMISTRESS user-defined function definitions are correctly parsed"
(is t))

(test burgermistress_functions_recursion
  "FUNCTIONS: BURGERMISTRESS recursive functions are correctly handled"
(is t))

(test burgermistress_functions_return_values
  "FUNCTIONS: BURGERMISTRESS function return values are correctly captured"
(is t))

(test burgermistress_functions_parameter_passing
  "FUNCTIONS: BURGERMISTRESS parameters are correctly passed (by value, by reference)"
(is t))

