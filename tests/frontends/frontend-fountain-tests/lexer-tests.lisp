;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-fountain -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FOUNTAIN Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FOUNTAIN language lexer.
;;; See: src/frontend-fountain/fountain-lexer.lisp

(in-package :eightbol/test/frontend-fountain)

(fiveam:def-suite :fountain-lexer
  :description "FOUNTAIN lexer tests"
  :in :frontend-fountain)

(in-suite :fountain-lexer)


(test fountain_lexer_exists
  "Verify FOUNTAIN lexer module is present"
  (skip "Implementation pending"))

(test fountain_lexer_keywords
  "LEXER: FOUNTAIN keywords are correctly tokenized"
  (skip "Implementation pending"))

(test fountain_lexer_operators
  "LEXER: FOUNTAIN operators are correctly tokenized"
  (skip "Implementation pending"))

(test fountain_lexer_numbers
  "LEXER: FOUNTAIN numeric formats (int, float, hex) are correctly tokenized"
  (skip "Implementation pending"))

(test fountain_lexer_strings
  "LEXER: FOUNTAIN string literals with escape sequences are correctly tokenized"
  (skip "Implementation pending"))

(test fountain_lexer_identifiers
  "LEXER: FOUNTAIN identifiers and reserved words are correctly tokenized"
  (skip "Implementation pending"))

(test fountain_lexer_comments
  "LEXER: FOUNTAIN comments are correctly handled (skipped or captured)"
  (skip "Implementation pending"))

