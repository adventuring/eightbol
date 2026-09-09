;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-sci -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCI Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCI language lexer.
;;; See: src/frontend-sci/sci-lexer.lisp

(in-package :eightbol/test/frontend-sci)

(fiveam:def-suite :sci-lexer
  :description "SCI lexer tests"
  :in :frontend-sci)

(in-suite :sci-lexer)


(test sci_lexer_exists
  "Verify SCI lexer module is present"
  (skip "Implementation pending"))

(test sci_lexer_keywords
  "LEXER: SCI keywords are correctly tokenized"
  (skip "Implementation pending"))

(test sci_lexer_operators
  "LEXER: SCI operators are correctly tokenized"
  (skip "Implementation pending"))

(test sci_lexer_numbers
  "LEXER: SCI numeric formats (int, float, hex) are correctly tokenized"
  (skip "Implementation pending"))

(test sci_lexer_strings
  "LEXER: SCI string literals with escape sequences are correctly tokenized"
  (skip "Implementation pending"))

(test sci_lexer_identifiers
  "LEXER: SCI identifiers and reserved words are correctly tokenized"
  (skip "Implementation pending"))

(test sci_lexer_comments
  "LEXER: SCI comments are correctly handled (skipped or captured)"
  (skip "Implementation pending"))

