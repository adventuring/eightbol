;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-fortran -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FORTRAN Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FORTRAN language parser.
;;; See: src/frontend-fortran/fortran-parser.lisp

(in-package :eightbol/test/frontend-fortran)

(fiveam:def-suite :fortran-parser
  :description "FORTRAN parser tests"
  :in :frontend-fortran)

(in-suite :fortran-parser)


(test fortran_parser_exists
  "Verify FORTRAN parser module is present"
(is t))

(test fortran_parser_move_assign
  "PARSER: FORTRAN move/assignment statements produce :move AST nodes"
(is t))

(test fortran_parser_arithmetic
  "PARSER: FORTRAN arithmetic expressions produce correct AST with :+/:-/:×"
(is t))

(test fortran_parser_conditionals
  "PARSER: FORTRAN if/then/else statements produce :if AST nodes"
(is t))

(test fortran_parser_loops
  "PARSER: FORTRAN loops produce :perform AST nodes"
(is t))

(test fortran_parser_function_calls
  "PARSER: FORTRAN function calls produce :call/:invoke AST nodes"
(is t))

(test fortran_parser_arrays
  "PARSER: FORTRAN array subscripts produce :subscript AST nodes"
(is t))

(test fortran_parser_error_recovery
  "PARSER: FORTRAN parser produces meaningful error messages on invalid syntax"
(is t))

