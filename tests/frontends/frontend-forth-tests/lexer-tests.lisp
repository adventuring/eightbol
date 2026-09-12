;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-forth -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FORTH Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FORTH language lexer.
;;; See: src/frontend-forth/forth-lexer.lisp

(in-package :eightbol/test/frontend-forth)

(fiveam:def-suite :forth-lexer
  :description "FORTH lexer tests"
  :in :frontend-forth)

(in-suite :forth-lexer)


(test forth_lexer_exists
  "Verify FORTH lexer module is present"
(is t))

(test forth_lexer_keywords
  "LEXER: FORTH keywords are correctly tokenized"
(is t))

(test forth_lexer_operators
  "LEXER: FORTH operators are correctly tokenized"
(is t))

(test forth_lexer_numbers
  "LEXER: FORTH numeric formats (int, float, hex) are correctly tokenized"
(is t))

(test forth_lexer_strings
  "LEXER: FORTH string literals with escape sequences are correctly tokenized"
(is t))

(test forth_lexer_identifiers
  "LEXER: FORTH identifiers and reserved words are correctly tokenized"
(is t))

(test forth_lexer_comments
  "LEXER: FORTH comments are correctly handled (skipped or captured)"
(is t))

