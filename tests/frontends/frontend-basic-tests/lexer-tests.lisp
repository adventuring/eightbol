;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-basic -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BASIC Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BASIC language lexer.
;;; See: src/frontend-basic/basic-lexer.lisp

(in-package :eightbol/test/frontend-basic)

(fiveam:def-suite :basic-lexer
  :description "BASIC lexer tests"
  :in :frontend-basic)

(in-suite :basic-lexer)


(test basic_lexer_exists
  "Verify BASIC lexer module is present"
  (skip "Implementation pending"))

(test basic_lexer_keywords
  "LEXER: BASIC keywords are correctly tokenized"
  (skip "Implementation pending"))

(test basic_lexer_operators
  "LEXER: BASIC operators are correctly tokenized"
  (skip "Implementation pending"))

(test basic_lexer_numbers
  "LEXER: BASIC numeric formats (int, float, hex) are correctly tokenized"
  (skip "Implementation pending"))

(test basic_lexer_strings
  "LEXER: BASIC string literals with escape sequences are correctly tokenized"
  (skip "Implementation pending"))

(test basic_lexer_identifiers
  "LEXER: BASIC identifiers and reserved words are correctly tokenized"
  (skip "Implementation pending"))

(test basic_lexer_comments
  "LEXER: BASIC comments are correctly handled (skipped or captured)"
  (skip "Implementation pending"))

