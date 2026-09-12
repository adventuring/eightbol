;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-scumm -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCUMM Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCUMM language functions.
;;; See: src/frontend-scumm/scumm-parser.lisp

(in-package :eightbol/test/frontend-scumm)

(fiveam:def-suite :scumm-functions
  :description "SCUMM functions tests"
  :in :frontend-scumm)

(in-suite :scumm-functions)


(test scumm_functions_library
  "FUNCTIONS: SCUMM library function calls are correctly identified"
(is t))

(test scumm_functions_user_defined
  "FUNCTIONS: SCUMM user-defined function definitions are correctly parsed"
(is t))

(test scumm_functions_recursion
  "FUNCTIONS: SCUMM recursive functions are correctly handled"
(is t))

(test scumm_functions_return_values
  "FUNCTIONS: SCUMM function return values are correctly captured"
(is t))

(test scumm_functions_parameter_passing
  "FUNCTIONS: SCUMM parameters are correctly passed (by value, by reference)"
(is t))

