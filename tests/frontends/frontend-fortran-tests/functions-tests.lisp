;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-fortran -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FORTRAN Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FORTRAN language functions.
;;; See: src/frontend-fortran/fortran-parser.lisp

(in-package :eightbol/test/frontend-fortran)

(fiveam:def-suite :fortran-functions
  :description "FORTRAN functions tests"
  :in :frontend-fortran)

(in-suite :fortran-functions)


(test fortran_functions_library
  "FUNCTIONS: FORTRAN library function calls are correctly identified"
  (skip "Implementation pending"))

(test fortran_functions_user_defined
  "FUNCTIONS: FORTRAN user-defined function definitions are correctly parsed"
  (skip "Implementation pending"))

(test fortran_functions_recursion
  "FUNCTIONS: FORTRAN recursive functions are correctly handled"
  (skip "Implementation pending"))

(test fortran_functions_return_values
  "FUNCTIONS: FORTRAN function return values are correctly captured"
  (skip "Implementation pending"))

(test fortran_functions_parameter_passing
  "FUNCTIONS: FORTRAN parameters are correctly passed (by value, by reference)"
  (skip "Implementation pending"))

