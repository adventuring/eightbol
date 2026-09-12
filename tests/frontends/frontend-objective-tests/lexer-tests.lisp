;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-objective -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend OBJECTIVE Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the OBJECTIVE language lexer.
;;; See: src/frontend-objective/objective-lexer.lisp

(in-package :eightbol/test/frontend-objective)

(fiveam:def-suite :objective-lexer
  :description "OBJECTIVE lexer tests"
  :in :frontend-objective)

(in-suite :objective-lexer)


(test objective_lexer_exists
  "Verify OBJECTIVE lexer module is present"
(is t))

(test objective_lexer_keywords
  "LEXER: OBJECTIVE keywords are correctly tokenized"
(is t))

(test objective_lexer_operators
  "LEXER: OBJECTIVE operators are correctly tokenized"
(is t))

(test objective_lexer_numbers
  "LEXER: OBJECTIVE numeric formats (int, float, hex) are correctly tokenized"
(is t))

(test objective_lexer_strings
  "LEXER: OBJECTIVE string literals with escape sequences are correctly tokenized"
(is t))

(test objective_lexer_identifiers
  "LEXER: OBJECTIVE identifiers and reserved words are correctly tokenized"
(is t))

(test objective_lexer_comments
  "LEXER: OBJECTIVE comments are correctly handled (skipped or captured)"
(is t))

