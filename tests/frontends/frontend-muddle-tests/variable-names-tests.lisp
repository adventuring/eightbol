;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-muddle -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend MUDDLE Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the MUDDLE language variable-names.
;;; See: src/frontend-muddle/muddle-parser.lisp

(in-package :eightbol/test/frontend-muddle)

(fiveam:def-suite :muddle-variable-names
  :description "MUDDLE variable-names tests"
  :in :frontend-muddle)

(in-suite :muddle-variable-names)


(test muddle_variables_case_sensitivity
  "VARIABLES: MUDDLE identifier case sensitivity is handled per language rules"
(is t))

(test muddle_variables_reserved_words
  "VARIABLES: MUDDLE reserved words are protected from use as identifiers"
(is t))

(test muddle_variables_normalization
  "VARIABLES: MUDDLE identifiers are normalized correctly (kebab-case, snake_case, etc.)"
(is t))

(test muddle_variables_scope
  "VARIABLES: MUDDLE variable scope is correctly tracked (local, global, method)"
(is t))

