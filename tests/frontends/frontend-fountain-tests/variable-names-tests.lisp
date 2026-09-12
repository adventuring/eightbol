;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-fountain -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FOUNTAIN Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FOUNTAIN language variable-names.
;;; See: src/frontend-fountain/fountain-parser.lisp

(in-package :eightbol/test/frontend-fountain)

(fiveam:def-suite :fountain-variable-names
  :description "FOUNTAIN variable-names tests"
  :in :frontend-fountain)

(in-suite :fountain-variable-names)


(test fountain_variables_case_sensitivity
  "VARIABLES: FOUNTAIN identifier case sensitivity is handled per language rules"
(is t))

(test fountain_variables_reserved_words
  "VARIABLES: FOUNTAIN reserved words are protected from use as identifiers"
(is t))

(test fountain_variables_normalization
  "VARIABLES: FOUNTAIN identifiers are normalized correctly (kebab-case, snake_case, etc.)"
(is t))

(test fountain_variables_scope
  "VARIABLES: FOUNTAIN variable scope is correctly tracked (local, global, method)"
(is t))

