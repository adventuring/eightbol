;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-fortran -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FORTRAN Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FORTRAN language lexer.
;;; See: src/frontend-fortran/fortran-lexer.lisp

(in-package :eightbol/test/frontend-fortran)

(fiveam:def-suite :fortran-lexer
  :description "FORTRAN lexer tests"
  :in :frontend-fortran)

(in-suite :fortran-lexer)


(test fortran_lexer_exists
  "Verify FORTRAN lexer module is present"
  (skip "Implementation pending"))

(test fortran_lexer_keywords
  "LEXER: FORTRAN keywords are correctly tokenized"
  (skip "Implementation pending"))

(test fortran_lexer_operators
  "LEXER: FORTRAN operators are correctly tokenized"
  (skip "Implementation pending"))

(test fortran_lexer_numbers
  "LEXER: FORTRAN numeric formats (int, float, hex) are correctly tokenized"
  (skip "Implementation pending"))

(test fortran_lexer_strings
  "LEXER: FORTRAN string literals with escape sequences are correctly tokenized"
  (skip "Implementation pending"))

(test fortran_lexer_identifiers
  "LEXER: FORTRAN identifiers and reserved words are correctly tokenized"
  (skip "Implementation pending"))

(test fortran_lexer_comments
  "LEXER: FORTRAN comments are correctly handled (skipped or captured)"
  (skip "Implementation pending"))

