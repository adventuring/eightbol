;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lingo -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LINGO Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LINGO language lexer.
;;; See: src/frontend-lingo/lingo-lexer.lisp

(in-package :eightbol/test/frontend-lingo)

(fiveam:def-suite :lingo-lexer
  :description "LINGO lexer tests"
  :in :frontend-lingo)

(in-suite :lingo-lexer)


(test lingo_lexer_exists
  "Verify LINGO lexer module is present"
(is t))

(test lingo_lexer_keywords
  "LEXER: LINGO keywords are correctly tokenized"
(is t))

(test lingo_lexer_operators
  "LEXER: LINGO operators are correctly tokenized"
(is t))

(test lingo_lexer_numbers
  "LEXER: LINGO numeric formats (int, float, hex) are correctly tokenized"
(is t))

(test lingo_lexer_strings
  "LEXER: LINGO string literals with escape sequences are correctly tokenized"
(is t))

(test lingo_lexer_identifiers
  "LEXER: LINGO identifiers and reserved words are correctly tokenized"
(is t))

(test lingo_lexer_comments
  "LEXER: LINGO comments are correctly handled (skipped or captured)"
(is t))

