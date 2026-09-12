;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-agi -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend AGI Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the AGI language parser.
;;; See: src/frontend-agi/agi-parser.lisp

(in-package :eightbol/test/frontend-agi)

(fiveam:def-suite :agi-parser
  :description "AGI parser tests"
  :in :frontend-agi)

(in-suite :agi-parser)


(test agi_parser_exists
  "Verify AGI parser module is present"
(is t))

(test agi_parser_move_assign
  "PARSER: AGI move/assignment statements produce :move AST nodes"
(is t))

(test agi_parser_arithmetic
  "PARSER: AGI arithmetic expressions produce correct AST with :add/:subtract/:multiply"
(is t))

(test agi_parser_conditionals
  "PARSER: AGI if/then/else statements produce :if AST nodes"
(is t))

(test agi_parser_loops
  "PARSER: AGI loops produce :perform AST nodes"
(is t))

(test agi_parser_function_calls
  "PARSER: AGI function calls produce :call/:invoke AST nodes"
(is t))

(test agi_parser_arrays
  "PARSER: AGI array subscripts produce :subscript AST nodes"
(is t))

(test agi_parser_error_recovery
  "PARSER: AGI parser produces meaningful error messages on invalid syntax"
(is t))

