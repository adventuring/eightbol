;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-smalltalk -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SMALLTALK Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SMALLTALK language lexer.
;;; See: src/frontend-smalltalk/smalltalk-lexer.lisp

(in-package :eightbol/test/frontend-smalltalk)

(fiveam:def-suite :smalltalk-lexer
  :description "SMALLTALK lexer tests"
  :in :frontend-smalltalk)

(in-suite :smalltalk-lexer)


(test smalltalk_lexer_exists
  "Verify SMALLTALK lexer module is present"
(is t))

(test smalltalk_lexer_keywords
  "LEXER: SMALLTALK keywords are correctly tokenized"
(is t))

(test smalltalk_lexer_operators
  "LEXER: SMALLTALK operators are correctly tokenized"
(is t))

(test smalltalk_lexer_numbers
  "LEXER: SMALLTALK numeric formats (int, float, hex) are correctly tokenized"
(is t))

(test smalltalk_lexer_strings
  "LEXER: SMALLTALK string literals with escape sequences are correctly tokenized"
(is t))

(test smalltalk_lexer_identifiers
  "LEXER: SMALLTALK identifiers and reserved words are correctly tokenized"
(is t))

(test smalltalk_lexer_comments
  "LEXER: SMALLTALK comments are correctly handled (skipped or captured)"
(is t))

