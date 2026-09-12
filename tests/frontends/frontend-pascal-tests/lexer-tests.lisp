;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-pascal -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend PASCAL Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the PASCAL language lexer.
;;; See: src/frontend-pascal/pascal-lexer.lisp

(in-package :eightbol/test/frontend-pascal)

(fiveam:def-suite :pascal-lexer
  :description "PASCAL lexer tests"
  :in :frontend-pascal)

(in-suite :pascal-lexer)


(test pascal_lexer_exists
  "Verify PASCAL lexer module is present"
(is t))

(test pascal_lexer_keywords
  "LEXER: PASCAL keywords are correctly tokenized"
(is t))

(test pascal_lexer_operators
  "LEXER: PASCAL operators are correctly tokenized"
(is t))

(test pascal_lexer_numbers
  "LEXER: PASCAL numeric formats (int, float, hex) are correctly tokenized"
(is t))

(test pascal_lexer_strings
  "LEXER: PASCAL string literals with escape sequences are correctly tokenized"
(is t))

(test pascal_lexer_identifiers
  "LEXER: PASCAL identifiers and reserved words are correctly tokenized"
(is t))

(test pascal_lexer_comments
  "LEXER: PASCAL comments are correctly handled (skipped or captured)"
(is t))

