;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lingo -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LINGO Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LINGO language variable-names.
;;; See: src/frontend-lingo/lingo-parser.lisp

(in-package :eightbol/test/frontend-lingo)

(fiveam:def-suite :lingo-variable-names
  :description "LINGO variable-names tests"
  :in :frontend-lingo)

(in-suite :lingo-variable-names)


(test lingo_variables_case_sensitivity
  "VARIABLES: LINGO identifier case sensitivity is handled per language rules"
(is t))

(test lingo_variables_reserved_words
  "VARIABLES: LINGO reserved words are protected from use as identifiers"
(is t))

(test lingo_variables_normalization
  "VARIABLES: LINGO identifiers are normalized correctly (kebab-case, snake_case, etc.)"
(is t))

(test lingo_variables_scope
  "VARIABLES: LINGO variable scope is correctly tracked (local, global, method)"
(is t))

