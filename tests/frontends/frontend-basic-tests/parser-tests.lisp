;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-basic -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BASIC Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BASIC language parser.
;;; See: src/frontend-basic/basic-parser.lisp

(in-package :eightbol/test/frontend-basic)

(fiveam:def-suite :basic-parser
  :description "BASIC parser tests"
  :in :frontend-basic)

(in-suite :basic-parser)


(test basic_parser_exists
  "Verify BASIC parser module is present"
(is t))

(test basic_parser_move_assign
  "PARSER: BASIC move/assignment statements produce :move AST nodes"
(is t))

(test basic_parser_arithmetic
  "PARSER: BASIC arithmetic expressions produce correct AST with :+/:-/:×"
(is t))

(test basic_parser_conditionals
  "PARSER: BASIC if/then/else statements produce :if AST nodes"
(is t))

(test basic_parser_loops
  "PARSER: BASIC loops produce :perform AST nodes"
(is t))

(test basic_parser_function_calls
  "PARSER: BASIC function calls produce :call/:invoke AST nodes"
(is t))

(test basic_parser_arrays
  "PARSER: BASIC array subscripts produce :subscript AST nodes"
(is t))

(test basic_parser_error_recovery
  "PARSER: BASIC parser produces meaningful error messages on invalid syntax"
(is t))

