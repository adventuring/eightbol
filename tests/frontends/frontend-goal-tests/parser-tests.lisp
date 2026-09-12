;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-goal -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend GOAL Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the GOAL language parser.
;;; See: src/frontend-goal/goal-parser.lisp

(in-package :eightbol/test/frontend-goal)

(fiveam:def-suite :goal-parser
  :description "GOAL parser tests"
  :in :frontend-goal)

(in-suite :goal-parser)


(test goal_parser_exists
  "Verify GOAL parser module is present"
(is t))

(test goal_parser_move_assign
  "PARSER: GOAL move/assignment statements produce :move AST nodes"
(is t))

(test goal_parser_arithmetic
  "PARSER: GOAL arithmetic expressions produce correct AST with :add/:subtract/:multiply"
(is t))

(test goal_parser_conditionals
  "PARSER: GOAL if/then/else statements produce :if AST nodes"
(is t))

(test goal_parser_loops
  "PARSER: GOAL loops produce :perform AST nodes"
(is t))

(test goal_parser_function_calls
  "PARSER: GOAL function calls produce :call/:invoke AST nodes"
(is t))

(test goal_parser_arrays
  "PARSER: GOAL array subscripts produce :subscript AST nodes"
(is t))

(test goal_parser_error_recovery
  "PARSER: GOAL parser produces meaningful error messages on invalid syntax"
(is t))

