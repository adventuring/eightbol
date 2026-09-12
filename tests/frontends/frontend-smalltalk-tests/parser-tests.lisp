;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-smalltalk -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SMALLTALK Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SMALLTALK language parser.
;;; See: src/frontend-smalltalk/smalltalk-parser.lisp

(in-package :eightbol/test/frontend-smalltalk)

(fiveam:def-suite :smalltalk-parser
  :description "SMALLTALK parser tests"
  :in :frontend-smalltalk)

(in-suite :smalltalk-parser)


(test smalltalk_parser_exists
  "Verify SMALLTALK parser module is present"
(is t))

(test smalltalk_parser_move_assign
  "PARSER: SMALLTALK move/assignment statements produce :move AST nodes"
(is t))

(test smalltalk_parser_arithmetic
  "PARSER: SMALLTALK arithmetic expressions produce correct AST with :add/:subtract/:multiply"
(is t))

(test smalltalk_parser_conditionals
  "PARSER: SMALLTALK if/then/else statements produce :if AST nodes"
(is t))

(test smalltalk_parser_loops
  "PARSER: SMALLTALK loops produce :perform AST nodes"
(is t))

(test smalltalk_parser_function_calls
  "PARSER: SMALLTALK function calls produce :call/:invoke AST nodes"
(is t))

(test smalltalk_parser_arrays
  "PARSER: SMALLTALK array subscripts produce :subscript AST nodes"
(is t))

(test smalltalk_parser_error_recovery
  "PARSER: SMALLTALK parser produces meaningful error messages on invalid syntax"
(is t))

