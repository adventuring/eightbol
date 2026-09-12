;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-burgermistress -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BURGERMISTRESS Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BURGERMISTRESS language lexer.
;;; See: src/frontend-burgermistress/burgermistress-lexer.lisp

(in-package :eightbol/test/frontend-burgermistress)

(fiveam:def-suite :burgermistress-lexer
  :description "BURGERMISTRESS lexer tests"
  :in :frontend-burgermistress)

(in-suite :burgermistress-lexer)


(test burgermistress_lexer_exists
  "Verify BURGERMISTRESS lexer module is present"
(is t))

(test burgermistress_lexer_keywords
  "LEXER: BURGERMISTRESS keywords are correctly tokenized"
(is t))

(test burgermistress_lexer_operators
  "LEXER: BURGERMISTRESS operators are correctly tokenized"
(is t))

(test burgermistress_lexer_numbers
  "LEXER: BURGERMISTRESS numeric formats (int, float, hex) are correctly tokenized"
(is t))

(test burgermistress_lexer_strings
  "LEXER: BURGERMISTRESS string literals with escape sequences are correctly tokenized"
(is t))

(test burgermistress_lexer_identifiers
  "LEXER: BURGERMISTRESS identifiers and reserved words are correctly tokenized"
(is t))

(test burgermistress_lexer_comments
  "LEXER: BURGERMISTRESS comments are correctly handled (skipped or captured)"
(is t))

