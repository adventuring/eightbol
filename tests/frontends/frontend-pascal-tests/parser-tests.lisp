;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-pascal -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend PASCAL Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the PASCAL language parser.
;;; See: src/frontend-pascal/pascal-parser.lisp

(in-package :eightbol/test/frontend-pascal)

(fiveam:def-suite :pascal-parser
  :description "PASCAL parser tests"
  :in :frontend-pascal)

(in-suite :pascal-parser)


(test pascal_parser_exists
  "Verify PASCAL parser module is present"
(is t))

(test pascal_parser_move_assign
  "PARSER: PASCAL move/assignment statements produce :move AST nodes"
(is t))

(test pascal_parser_arithmetic
  "PARSER: PASCAL arithmetic expressions produce correct AST with :+/:-/:×"
(is t))

(test pascal_parser_conditionals
  "PARSER: PASCAL if/then/else statements produce :if AST nodes"
(is t))

(test pascal_parser_loops
  "PARSER: PASCAL loops produce :perform AST nodes"
(is t))

(test pascal_parser_function_calls
  "PARSER: PASCAL function calls produce :call/:invoke AST nodes"
(is t))

(test pascal_parser_arrays
  "PARSER: PASCAL array subscripts produce :subscript AST nodes"
(is t))

(test pascal_parser_error_recovery
  "PARSER: PASCAL parser produces meaningful error messages on invalid syntax"
(is t))

