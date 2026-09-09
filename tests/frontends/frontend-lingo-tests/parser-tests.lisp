;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lingo -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LINGO Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LINGO language parser.
;;; See: src/frontend-lingo/lingo-parser.lisp

(in-package :eightbol/test/frontend-lingo)

(fiveam:def-suite :lingo-parser
  :description "LINGO parser tests"
  :in :frontend-lingo)

(in-suite :lingo-parser)


(test lingo_parser_exists
  "Verify LINGO parser module is present"
  (skip "Implementation pending"))

(test lingo_parser_move_assign
  "PARSER: LINGO move/assignment statements produce :move AST nodes"
  (skip "Implementation pending"))

(test lingo_parser_arithmetic
  "PARSER: LINGO arithmetic expressions produce correct AST with :add/:subtract/:multiply"
  (skip "Implementation pending"))

(test lingo_parser_conditionals
  "PARSER: LINGO if/then/else statements produce :if AST nodes"
  (skip "Implementation pending"))

(test lingo_parser_loops
  "PARSER: LINGO loops produce :perform AST nodes"
  (skip "Implementation pending"))

(test lingo_parser_function_calls
  "PARSER: LINGO function calls produce :call/:invoke AST nodes"
  (skip "Implementation pending"))

(test lingo_parser_arrays
  "PARSER: LINGO array subscripts produce :subscript AST nodes"
  (skip "Implementation pending"))

(test lingo_parser_error_recovery
  "PARSER: LINGO parser produces meaningful error messages on invalid syntax"
  (skip "Implementation pending"))

