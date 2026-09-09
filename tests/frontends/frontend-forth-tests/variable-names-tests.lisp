;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-forth -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FORTH Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FORTH language variable-names.
;;; See: src/frontend-forth/forth-parser.lisp

(in-package :eightbol/test/frontend-forth)

(fiveam:def-suite :forth-variable-names
  :description "FORTH variable-names tests"
  :in :frontend-forth)

(in-suite :forth-variable-names)


(test forth_variables_case_sensitivity
  "VARIABLES: FORTH identifier case sensitivity is handled per language rules"
  (skip "Implementation pending"))

(test forth_variables_reserved_words
  "VARIABLES: FORTH reserved words are protected from use as identifiers"
  (skip "Implementation pending"))

(test forth_variables_normalization
  "VARIABLES: FORTH identifiers are normalized correctly (kebab-case, snake_case, etc.)"
  (skip "Implementation pending"))

(test forth_variables_scope
  "VARIABLES: FORTH variable scope is correctly tracked (local, global, method)"
  (skip "Implementation pending"))

