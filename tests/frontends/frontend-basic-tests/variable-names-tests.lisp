;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-basic -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend BASIC Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BASIC language variable-names.
;;; See: src/frontend-basic/basic-parser.lisp

(in-package :eightbol/test/frontend-basic)

(fiveam:def-suite :basic-variable-names
  :description "BASIC variable-names tests"
  :in :frontend-basic)

(in-suite :basic-variable-names)


(test basic_variables_case_sensitivity
  "VARIABLES: BASIC identifier case sensitivity is handled per language rules"
  (skip "Implementation pending"))

(test basic_variables_reserved_words
  "VARIABLES: BASIC reserved words are protected from use as identifiers"
  (skip "Implementation pending"))

(test basic_variables_normalization
  "VARIABLES: BASIC identifiers are normalized correctly (kebab-case, snake_case, etc.)"
  (skip "Implementation pending"))

(test basic_variables_scope
  "VARIABLES: BASIC variable scope is correctly tracked (local, global, method)"
  (skip "Implementation pending"))

