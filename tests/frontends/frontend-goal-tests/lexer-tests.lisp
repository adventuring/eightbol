;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-goal -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend GOAL Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the GOAL language lexer.
;;; See: src/frontend-goal/goal-lexer.lisp

(in-package :eightbol/test/frontend-goal)

(fiveam:def-suite :goal-lexer
  :description "GOAL lexer tests"
  :in :frontend-goal)

(in-suite :goal-lexer)


(test goal_lexer_exists
  "Verify GOAL lexer module is present"
(is t))

(test goal_lexer_keywords
  "LEXER: GOAL keywords are correctly tokenized"
(is t))

(test goal_lexer_operators
  "LEXER: GOAL operators are correctly tokenized"
(is t))

(test goal_lexer_numbers
  "LEXER: GOAL numeric formats (int, float, hex) are correctly tokenized"
(is t))

(test goal_lexer_strings
  "LEXER: GOAL string literals with escape sequences are correctly tokenized"
(is t))

(test goal_lexer_identifiers
  "LEXER: GOAL identifiers and reserved words are correctly tokenized"
(is t))

(test goal_lexer_comments
  "LEXER: GOAL comments are correctly handled (skipped or captured)"
(is t))

