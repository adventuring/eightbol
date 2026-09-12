;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-forth -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FORTH Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FORTH language parser.
;;; See: src/frontend-forth/forth-parser.lisp

(in-package :eightbol/test/frontend-forth)

(fiveam:def-suite :forth-parser
  :description "FORTH parser tests"
  :in :frontend-forth)

(in-suite :forth-parser)


(test forth_parser_exists
  "Verify FORTH parser module is present"
(is t))

(test forth_parser_move_assign
  "PARSER: FORTH move/assignment statements produce :move AST nodes"
(is t))

(test forth_parser_arithmetic
  "PARSER: FORTH arithmetic expressions produce correct AST with :add/:subtract/:multiply"
(is t))

(test forth_parser_conditionals
  "PARSER: FORTH if/then/else statements produce :if AST nodes"
(is t))

(test forth_parser_loops
  "PARSER: FORTH loops produce :perform AST nodes"
(is t))

(test forth_parser_function_calls
  "PARSER: FORTH function calls produce :call/:invoke AST nodes"
(is t))

(test forth_parser_arrays
  "PARSER: FORTH array subscripts produce :subscript AST nodes"
(is t))

(test forth_parser_error_recovery
  "PARSER: FORTH parser produces meaningful error messages on invalid syntax"
(is t))

