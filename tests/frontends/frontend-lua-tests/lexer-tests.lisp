;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lua -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LUA Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LUA language lexer.
;;; See: src/frontend-lua/lua-lexer.lisp

(in-package :eightbol/test/frontend-lua)

(fiveam:def-suite :lua-lexer
  :description "LUA lexer tests"
  :in :frontend-lua)

(in-suite :lua-lexer)


(test lua_lexer_exists
  "Verify LUA lexer module is present"
  (skip "Implementation pending"))

(test lua_lexer_keywords
  "LEXER: LUA keywords are correctly tokenized"
  (skip "Implementation pending"))

(test lua_lexer_operators
  "LEXER: LUA operators are correctly tokenized"
  (skip "Implementation pending"))

(test lua_lexer_numbers
  "LEXER: LUA numeric formats (int, float, hex) are correctly tokenized"
  (skip "Implementation pending"))

(test lua_lexer_strings
  "LEXER: LUA string literals with escape sequences are correctly tokenized"
  (skip "Implementation pending"))

(test lua_lexer_identifiers
  "LEXER: LUA identifiers and reserved words are correctly tokenized"
  (skip "Implementation pending"))

(test lua_lexer_comments
  "LEXER: LUA comments are correctly handled (skipped or captured)"
  (skip "Implementation pending"))

