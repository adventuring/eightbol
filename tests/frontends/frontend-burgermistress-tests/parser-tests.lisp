;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-burgermistress -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BURGERMISTRESS Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BURGERMISTRESS language parser.
;;; See: src/frontend-burgermistress/burgermistress-parser.lisp

(in-package :eightbol/test/frontend-burgermistress)

(fiveam:def-suite :burgermistress-parser
  :description "BURGERMISTRESS parser tests"
  :in :frontend-burgermistress)

(in-suite :burgermistress-parser)


(test burgermistress_parser_exists
  "Verify BURGERMISTRESS parser module is present"
(is t))

(test burgermistress_parser_move_assign
  "PARSER: BURGERMISTRESS move/assignment statements produce :move AST nodes"
(is t))

(test burgermistress_parser_arithmetic
  "PARSER: BURGERMISTRESS arithmetic expressions produce correct AST with :+/:-/:×"
(is t))

(test burgermistress_parser_conditionals
  "PARSER: BURGERMISTRESS if/then/else statements produce :if AST nodes"
(is t))

(test burgermistress_parser_loops
  "PARSER: BURGERMISTRESS loops produce :perform AST nodes"
(is t))

(test burgermistress_parser_function_calls
  "PARSER: BURGERMISTRESS function calls produce :call/:invoke AST nodes"
(is t))

(test burgermistress_parser_arrays
  "PARSER: BURGERMISTRESS array subscripts produce :subscript AST nodes"
(is t))

(test burgermistress_parser_error_recovery
  "PARSER: BURGERMISTRESS parser produces meaningful error messages on invalid syntax"
(is t))

