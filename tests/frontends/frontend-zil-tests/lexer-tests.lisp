;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-zil -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend ZIL Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ZIL language lexer.
;;; See: src/frontend-zil/zil-lexer.lisp

(in-package :eightbol/test/frontend-zil)

(fiveam:def-suite :zil-lexer
  :description "ZIL lexer tests"
  :in :frontend-zil)

(in-suite :zil-lexer)


(test zil_lexer_exists
  "Verify ZIL lexer module is present"
  (skip "Implementation pending"))

(test zil_lexer_keywords
  "LEXER: ZIL keywords are correctly tokenized"
  (skip "Implementation pending"))

(test zil_lexer_operators
  "LEXER: ZIL operators are correctly tokenized"
  (skip "Implementation pending"))

(test zil_lexer_numbers
  "LEXER: ZIL numeric formats (int, float, hex) are correctly tokenized"
  (skip "Implementation pending"))

(test zil_lexer_strings
  "LEXER: ZIL string literals with escape sequences are correctly tokenized"
  (skip "Implementation pending"))

(test zil_lexer_identifiers
  "LEXER: ZIL identifiers and reserved words are correctly tokenized"
  (skip "Implementation pending"))

(test zil_lexer_comments
  "LEXER: ZIL comments are correctly handled (skipped or captured)"
  (skip "Implementation pending"))

