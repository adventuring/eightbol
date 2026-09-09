;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-fountain -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FOUNTAIN Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FOUNTAIN language functions.
;;; See: src/frontend-fountain/fountain-parser.lisp

(in-package :eightbol/test/frontend-fountain)

(fiveam:def-suite :fountain-functions
  :description "FOUNTAIN functions tests"
  :in :frontend-fountain)

(in-suite :fountain-functions)


(test fountain_functions_library
  "FUNCTIONS: FOUNTAIN library function calls are correctly identified"
  (skip "Implementation pending"))

(test fountain_functions_user_defined
  "FUNCTIONS: FOUNTAIN user-defined function definitions are correctly parsed"
  (skip "Implementation pending"))

(test fountain_functions_recursion
  "FUNCTIONS: FOUNTAIN recursive functions are correctly handled"
  (skip "Implementation pending"))

(test fountain_functions_return_values
  "FUNCTIONS: FOUNTAIN function return values are correctly captured"
  (skip "Implementation pending"))

(test fountain_functions_parameter_passing
  "FUNCTIONS: FOUNTAIN parameters are correctly passed (by value, by reference)"
  (skip "Implementation pending"))

