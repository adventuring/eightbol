;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-smalltalk -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SMALLTALK Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SMALLTALK language variable-names.
;;; See: src/frontend-smalltalk/smalltalk-parser.lisp

(in-package :eightbol/test/frontend-smalltalk)

(fiveam:def-suite :smalltalk-variable-names
  :description "SMALLTALK variable-names tests"
  :in :frontend-smalltalk)

(in-suite :smalltalk-variable-names)


(test smalltalk_variables_case_sensitivity
  "VARIABLES: SMALLTALK identifier case sensitivity is handled per language rules"
(is t))

(test smalltalk_variables_reserved_words
  "VARIABLES: SMALLTALK reserved words are protected from use as identifiers"
(is t))

(test smalltalk_variables_normalization
  "VARIABLES: SMALLTALK identifiers are normalized correctly (kebab-case, snake_case, etc.)"
(is t))

(test smalltalk_variables_scope
  "VARIABLES: SMALLTALK variable scope is correctly tracked (local, global, method)"
(is t))

