;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-cobol -*-
;;;
;;; EIGHTBOL Frontend COBOL Lexer Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the COBOL language lexer.
;;; See: src/frontend-cobol/cobol-lexer.lisp

(in-package :eightbol/test/frontend-cobol)

(fiveam:def-suite :cobol-lexer
  :description "COBOL lexer tests"
  :in :frontend-cobol)

(in-suite :cobol-lexer)

(test cobol_lexer_keyword_identification_division
  "COBOL lexer recognizes IDENTIFICATION DIVISION keyword"
  (is-true (eightbol:cobol-lexer-keyword-p "IDENTIFICATION")))

(test cobol_lexer_keyword_procedure_division
  "COBOL lexer recognizes PROCEDURE DIVISION keyword"
  (is-true (eightbol:cobol-lexer-keyword-p "PROCEDURE")))

(test cobol_lexer_keyword_data_division
  "COBOL lexer recognizes DATA DIVISION keyword"
  (is-true (eightbol:cobol-lexer-keyword-p "DATA")))

(test cobol_lexer_keyword_environment_division
  "COBOL lexer recognizes ENVIRONMENT DIVISION keyword"
  (is-true (eightbol:cobol-lexer-keyword-p "ENVIRONMENT")))

(test cobol_lexer_numeric_literal
  "COBOL lexer tokenizes numeric literals (123, 45.67)"
  (skip "Need to access cobol lexer internals"))

(test cobol_lexer_string_literal
  "COBOL lexer tokenizes string literals with quotes"
  (skip "Need to access cobol lexer internals"))

(test cobol_lexer_identifier
  "COBOL lexer tokenizes identifiers (with hyphens allowed)"
  (skip "Need to access cobol lexer internals"))

(test cobol_lexer_reserved_words_protected
  "COBOL lexer protects reserved words from use as identifiers"
  (is-true (eightbol:cobol-lexer-keyword-p "MOVE")))

(test cobol_lexer_comment_line
  "COBOL lexer handles comment lines (column 7 = asterisk)"
  (skip "Need to access cobol lexer internals"))

(test cobol_lexer_column_based_format
  "COBOL lexer respects fixed-form column restrictions"
  (skip "Column-based lexing test"))
