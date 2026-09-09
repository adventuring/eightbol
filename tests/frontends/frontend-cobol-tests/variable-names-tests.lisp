;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-cobol -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend COBOL Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the COBOL language variable-names.
;;; See: src/frontend-cobol/cobol-parser.lisp

(in-package :eightbol/test/frontend-cobol)

(fiveam:def-suite :cobol-variable-names
  :description "COBOL variable-names tests"
  :in :frontend-cobol)

(in-suite :cobol-variable-names)


(test cobol_variables_case_sensitivity
  "VARIABLES: COBOL identifier case sensitivity is handled per language rules"
  (skip "Implementation pending"))

(test cobol_variables_reserved_words
  "VARIABLES: COBOL reserved words are protected from use as identifiers"
  (skip "Implementation pending"))

(test cobol_variables_normalization
  "VARIABLES: COBOL identifiers are normalized correctly (kebab-case, snake_case, etc.)"
  (skip "Implementation pending"))

(test cobol_variables_scope
  "VARIABLES: COBOL variable scope is correctly tracked (local, global, method)"
  (skip "Implementation pending"))

