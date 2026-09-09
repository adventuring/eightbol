;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-smalltalk -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SMALLTALK Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SMALLTALK language functions.
;;; See: src/frontend-smalltalk/smalltalk-parser.lisp

(in-package :eightbol/test/frontend-smalltalk)

(fiveam:def-suite :smalltalk-functions
  :description "SMALLTALK functions tests"
  :in :frontend-smalltalk)

(in-suite :smalltalk-functions)


(test smalltalk_functions_library
  "FUNCTIONS: SMALLTALK library function calls are correctly identified"
  (skip "Implementation pending"))

(test smalltalk_functions_user_defined
  "FUNCTIONS: SMALLTALK user-defined function definitions are correctly parsed"
  (skip "Implementation pending"))

(test smalltalk_functions_recursion
  "FUNCTIONS: SMALLTALK recursive functions are correctly handled"
  (skip "Implementation pending"))

(test smalltalk_functions_return_values
  "FUNCTIONS: SMALLTALK function return values are correctly captured"
  (skip "Implementation pending"))

(test smalltalk_functions_parameter_passing
  "FUNCTIONS: SMALLTALK parameters are correctly passed (by value, by reference)"
  (skip "Implementation pending"))

