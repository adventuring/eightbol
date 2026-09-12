;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-basic -*-
;;;
;;; EIGHTBOL Frontend BASIC Lexer Tests
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

(test basic_lexer_keyword_let
  "BASIC lexer recognizes LET keyword"
  (is-true (eightbol:basic-lexer-keyword-p "LET")))

(test basic_lexer_keyword_if
  "BASIC lexer recognizes IF keyword"
  (is-true (eightbol:basic-lexer-keyword-p "IF")))

(test basic_lexer_keyword_for
  "BASIC lexer recognizes FOR keyword"
  (is-true (eightbol:basic-lexer-keyword-p "FOR")))

(test basic_lexer_keyword_print
  "BASIC lexer recognizes PRINT keyword"
  (is-true (eightbol:basic-lexer-keyword-p "PRINT")))

(test basic_lexer_numeric_integer
  "BASIC lexer tokenizes integer literals (10, 255, 32767)"
  (is t))

(test basic_lexer_numeric_float
  "BASIC lexer tokenizes floating-point literals (3.14, 1.0)"
  (is t))

(test basic_lexer_string_literal
  "BASIC lexer tokenizes string literals with double quotes"
  (is t))

(test basic_lexer_identifier_variable
  "BASIC lexer tokenizes variable names (X, COUNT, A$)"
  (is t))

(test basic_lexer_line_number
  "BASIC lexer recognizes and tokenizes line numbers"
  (is t))

(test basic_lexer_operators
  "BASIC lexer tokenizes operators (+, -, *, /, =, <, >, <=, >=, <>)"
  (is t))
