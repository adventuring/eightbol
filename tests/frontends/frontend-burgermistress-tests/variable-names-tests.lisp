;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-burgermistress -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BURGERMISTRESS Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BURGERMISTRESS language variable-names.
;;; See: src/frontend-burgermistress/burgermistress-parser.lisp

(in-package :eightbol/test/frontend-burgermistress)

(fiveam:def-suite :burgermistress-variable-names
  :description "BURGERMISTRESS variable-names tests"
  :in :frontend-burgermistress)

(in-suite :burgermistress-variable-names)


(test burgermistress_variables_case_sensitivity
  "VARIABLES: BURGERMISTRESS identifier case sensitivity is handled per language rules"
  (skip "Implementation pending"))

(test burgermistress_variables_reserved_words
  "VARIABLES: BURGERMISTRESS reserved words are protected from use as identifiers"
  (skip "Implementation pending"))

(test burgermistress_variables_normalization
  "VARIABLES: BURGERMISTRESS identifiers are normalized correctly (kebab-case, snake_case, etc.)"
  (skip "Implementation pending"))

(test burgermistress_variables_scope
  "VARIABLES: BURGERMISTRESS variable scope is correctly tracked (local, global, method)"
  (skip "Implementation pending"))

