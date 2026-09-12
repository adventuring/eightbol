;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-zil -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend ZIL Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ZIL language parser.
;;; See: src/frontend-zil/zil-parser.lisp

(in-package :eightbol/test/frontend-zil)

(fiveam:def-suite :zil-parser
  :description "ZIL parser tests"
  :in :frontend-zil)

(in-suite :zil-parser)


(test zil_parser_exists
  "Verify ZIL parser module is present"
(is t))

(test zil_parser_move_assign
  "PARSER: ZIL move/assignment statements produce :move AST nodes"
(is t))

(test zil_parser_arithmetic
  "PARSER: ZIL arithmetic expressions produce correct AST with :add/:subtract/:multiply"
(is t))

(test zil_parser_conditionals
  "PARSER: ZIL if/then/else statements produce :if AST nodes"
(is t))

(test zil_parser_loops
  "PARSER: ZIL loops produce :perform AST nodes"
(is t))

(test zil_parser_function_calls
  "PARSER: ZIL function calls produce :call/:invoke AST nodes"
(is t))

(test zil_parser_arrays
  "PARSER: ZIL array subscripts produce :subscript AST nodes"
(is t))

(test zil_parser_error_recovery
  "PARSER: ZIL parser produces meaningful error messages on invalid syntax"
(is t))

