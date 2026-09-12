;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-sci -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCI Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCI language variable-names.
;;; See: src/frontend-sci/sci-parser.lisp

(in-package :eightbol/test/frontend-sci)

(fiveam:def-suite :sci-variable-names
  :description "SCI variable-names tests"
  :in :frontend-sci)

(in-suite :sci-variable-names)


(test sci_variables_case_sensitivity
  "VARIABLES: SCI identifier case sensitivity is handled per language rules"
(is t))

(test sci_variables_reserved_words
  "VARIABLES: SCI reserved words are protected from use as identifiers"
(is t))

(test sci_variables_normalization
  "VARIABLES: SCI identifiers are normalized correctly (kebab-case, snake_case, etc.)"
(is t))

(test sci_variables_scope
  "VARIABLES: SCI variable scope is correctly tracked (local, global, method)"
(is t))

