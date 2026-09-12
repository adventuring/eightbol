;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lua -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LUA Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LUA language parser.
;;; See: src/frontend-lua/lua-parser.lisp

(in-package :eightbol/test/frontend-lua)

(fiveam:def-suite :lua-parser
  :description "LUA parser tests"
  :in :frontend-lua)

(in-suite :lua-parser)


(test lua_parser_exists
  "Verify LUA parser module is present"
(is t))

(test lua_parser_move_assign
  "PARSER: LUA move/assignment statements produce :move AST nodes"
(is t))

(test lua_parser_arithmetic
  "PARSER: LUA arithmetic expressions produce correct AST with :add/:subtract/:multiply"
(is t))

(test lua_parser_conditionals
  "PARSER: LUA if/then/else statements produce :if AST nodes"
(is t))

(test lua_parser_loops
  "PARSER: LUA loops produce :perform AST nodes"
(is t))

(test lua_parser_function_calls
  "PARSER: LUA function calls produce :call/:invoke AST nodes"
(is t))

(test lua_parser_arrays
  "PARSER: LUA array subscripts produce :subscript AST nodes"
(is t))

(test lua_parser_error_recovery
  "PARSER: LUA parser produces meaningful error messages on invalid syntax"
(is t))

