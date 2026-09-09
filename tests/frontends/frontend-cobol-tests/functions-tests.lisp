;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-cobol -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend COBOL Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the COBOL language functions.
;;; See: src/frontend-cobol/cobol-parser.lisp

(in-package :eightbol/test/frontend-cobol)

(fiveam:def-suite :cobol-functions
  :description "COBOL functions tests"
  :in :frontend-cobol)

(in-suite :cobol-functions)


(test cobol_functions_library
  "FUNCTIONS: COBOL library function calls are correctly identified"
  (skip "Implementation pending"))

(test cobol_functions_user_defined
  "FUNCTIONS: COBOL user-defined function definitions are correctly parsed"
  (skip "Implementation pending"))

(test cobol_functions_recursion
  "FUNCTIONS: COBOL recursive functions are correctly handled"
  (skip "Implementation pending"))

(test cobol_functions_return_values
  "FUNCTIONS: COBOL function return values are correctly captured"
  (skip "Implementation pending"))

(test cobol_functions_parameter_passing
  "FUNCTIONS: COBOL parameters are correctly passed (by value, by reference)"
  (skip "Implementation pending"))

