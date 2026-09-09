;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-muddle -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend MUDDLE Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the MUDDLE language lexer.
;;; See: src/frontend-muddle/muddle-lexer.lisp

(in-package :eightbol/test/frontend-muddle)

(fiveam:def-suite :muddle-lexer
  :description "MUDDLE lexer tests"
  :in :frontend-muddle)

(in-suite :muddle-lexer)


(test muddle_lexer_exists
  "Verify MUDDLE lexer module is present"
  (skip "Implementation pending"))

(test muddle_lexer_keywords
  "LEXER: MUDDLE keywords are correctly tokenized"
  (skip "Implementation pending"))

(test muddle_lexer_operators
  "LEXER: MUDDLE operators are correctly tokenized"
  (skip "Implementation pending"))

(test muddle_lexer_numbers
  "LEXER: MUDDLE numeric formats (int, float, hex) are correctly tokenized"
  (skip "Implementation pending"))

(test muddle_lexer_strings
  "LEXER: MUDDLE string literals with escape sequences are correctly tokenized"
  (skip "Implementation pending"))

(test muddle_lexer_identifiers
  "LEXER: MUDDLE identifiers and reserved words are correctly tokenized"
  (skip "Implementation pending"))

(test muddle_lexer_comments
  "LEXER: MUDDLE comments are correctly handled (skipped or captured)"
  (skip "Implementation pending"))

