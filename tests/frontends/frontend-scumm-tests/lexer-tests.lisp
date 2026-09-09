;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-scumm -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCUMM Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCUMM language lexer.
;;; See: src/frontend-scumm/scumm-lexer.lisp

(in-package :eightbol/test/frontend-scumm)

(fiveam:def-suite :scumm-lexer
  :description "SCUMM lexer tests"
  :in :frontend-scumm)

(in-suite :scumm-lexer)


(test scumm_lexer_exists
  "Verify SCUMM lexer module is present"
  (skip "Implementation pending"))

(test scumm_lexer_keywords
  "LEXER: SCUMM keywords are correctly tokenized"
  (skip "Implementation pending"))

(test scumm_lexer_operators
  "LEXER: SCUMM operators are correctly tokenized"
  (skip "Implementation pending"))

(test scumm_lexer_numbers
  "LEXER: SCUMM numeric formats (int, float, hex) are correctly tokenized"
  (skip "Implementation pending"))

(test scumm_lexer_strings
  "LEXER: SCUMM string literals with escape sequences are correctly tokenized"
  (skip "Implementation pending"))

(test scumm_lexer_identifiers
  "LEXER: SCUMM identifiers and reserved words are correctly tokenized"
  (skip "Implementation pending"))

(test scumm_lexer_comments
  "LEXER: SCUMM comments are correctly handled (skipped or captured)"
  (skip "Implementation pending"))

