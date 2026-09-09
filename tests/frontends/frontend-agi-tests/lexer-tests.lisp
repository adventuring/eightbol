;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-agi -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend AGI Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the AGI language lexer.
;;; See: src/frontend-agi/agi-lexer.lisp

(in-package :eightbol/test/frontend-agi)

(fiveam:def-suite :agi-lexer
  :description "AGI lexer tests"
  :in :frontend-agi)

(in-suite :agi-lexer)


(test agi_lexer_exists
  "Verify AGI lexer module is present"
  (skip "Implementation pending"))

(test agi_lexer_keywords
  "LEXER: AGI keywords are correctly tokenized"
  (skip "Implementation pending"))

(test agi_lexer_operators
  "LEXER: AGI operators are correctly tokenized"
  (skip "Implementation pending"))

(test agi_lexer_numbers
  "LEXER: AGI numeric formats (int, float, hex) are correctly tokenized"
  (skip "Implementation pending"))

(test agi_lexer_strings
  "LEXER: AGI string literals with escape sequences are correctly tokenized"
  (skip "Implementation pending"))

(test agi_lexer_identifiers
  "LEXER: AGI identifiers and reserved words are correctly tokenized"
  (skip "Implementation pending"))

(test agi_lexer_comments
  "LEXER: AGI comments are correctly handled (skipped or captured)"
  (skip "Implementation pending"))

