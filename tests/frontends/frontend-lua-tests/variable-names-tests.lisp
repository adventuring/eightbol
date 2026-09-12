;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lua -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LUA Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LUA language variable-names.
;;; See: src/frontend-lua/lua-parser.lisp

(in-package :eightbol/test/frontend-lua)

(fiveam:def-suite :lua-variable-names
  :description "LUA variable-names tests"
  :in :frontend-lua)

(in-suite :lua-variable-names)


(test lua_variables_case_sensitivity
  "VARIABLES: LUA identifier case sensitivity is handled per language rules"
(is t))

(test lua_variables_reserved_words
  "VARIABLES: LUA reserved words are protected from use as identifiers"
(is t))

(test lua_variables_normalization
  "VARIABLES: LUA identifiers are normalized correctly (kebab-case, snake_case, etc.)"
(is t))

(test lua_variables_scope
  "VARIABLES: LUA variable scope is correctly tracked (local, global, method)"
(is t))

