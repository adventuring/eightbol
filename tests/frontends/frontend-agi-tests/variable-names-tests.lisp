;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-agi -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend AGI Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the AGI language variable-names.
;;; See: src/frontend-agi/agi-parser.lisp

(in-package :eightbol/test/frontend-agi)

(fiveam:def-suite :agi-variable-names
  :description "AGI variable-names tests"
  :in :frontend-agi)

(in-suite :agi-variable-names)


(test agi_variables_case_sensitivity
  "VARIABLES: AGI identifier case sensitivity is handled per language rules"
  (skip "Implementation pending"))

(test agi_variables_reserved_words
  "VARIABLES: AGI reserved words are protected from use as identifiers"
  (skip "Implementation pending"))

(test agi_variables_normalization
  "VARIABLES: AGI identifiers are normalized correctly (kebab-case, snake_case, etc.)"
  (skip "Implementation pending"))

(test agi_variables_scope
  "VARIABLES: AGI variable scope is correctly tracked (local, global, method)"
  (skip "Implementation pending"))

