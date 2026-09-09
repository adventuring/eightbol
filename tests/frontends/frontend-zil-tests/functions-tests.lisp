;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-zil -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend ZIL Functions Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ZIL language functions.
;;; See: src/frontend-zil/zil-parser.lisp

(in-package :eightbol/test/frontend-zil)

(fiveam:def-suite :zil-functions
  :description "ZIL functions tests"
  :in :frontend-zil)

(in-suite :zil-functions)


(test zil_functions_library
  "FUNCTIONS: ZIL library function calls are correctly identified"
  (skip "Implementation pending"))

(test zil_functions_user_defined
  "FUNCTIONS: ZIL user-defined function definitions are correctly parsed"
  (skip "Implementation pending"))

(test zil_functions_recursion
  "FUNCTIONS: ZIL recursive functions are correctly handled"
  (skip "Implementation pending"))

(test zil_functions_return_values
  "FUNCTIONS: ZIL function return values are correctly captured"
  (skip "Implementation pending"))

(test zil_functions_parameter_passing
  "FUNCTIONS: ZIL parameters are correctly passed (by value, by reference)"
  (skip "Implementation pending"))

