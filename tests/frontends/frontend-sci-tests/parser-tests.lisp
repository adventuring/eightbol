;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-sci -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCI Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCI language parser.
;;; See: src/frontend-sci/sci-parser.lisp

(in-package :eightbol/test/frontend-sci)

(fiveam:def-suite :sci-parser
  :description "SCI parser tests"
  :in :frontend-sci)

(in-suite :sci-parser)


(test sci_parser_exists
  "Verify SCI parser module is present"
  (skip "Implementation pending"))

(test sci_parser_move_assign
  "PARSER: SCI move/assignment statements produce :move AST nodes"
  (skip "Implementation pending"))

(test sci_parser_arithmetic
  "PARSER: SCI arithmetic expressions produce correct AST with :add/:subtract/:multiply"
  (skip "Implementation pending"))

(test sci_parser_conditionals
  "PARSER: SCI if/then/else statements produce :if AST nodes"
  (skip "Implementation pending"))

(test sci_parser_loops
  "PARSER: SCI loops produce :perform AST nodes"
  (skip "Implementation pending"))

(test sci_parser_function_calls
  "PARSER: SCI function calls produce :call/:invoke AST nodes"
  (skip "Implementation pending"))

(test sci_parser_arrays
  "PARSER: SCI array subscripts produce :subscript AST nodes"
  (skip "Implementation pending"))

(test sci_parser_error_recovery
  "PARSER: SCI parser produces meaningful error messages on invalid syntax"
  (skip "Implementation pending"))

