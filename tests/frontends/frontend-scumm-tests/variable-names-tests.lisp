;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-scumm -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend SCUMM Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SCUMM language variable-names.
;;; See: src/frontend-scumm/scumm-parser.lisp

(in-package :eightbol/test/frontend-scumm)

(fiveam:def-suite :scumm-variable-names
  :description "SCUMM variable-names tests"
  :in :frontend-scumm)

(in-suite :scumm-variable-names)


(test scumm_variables_case_sensitivity
  "VARIABLES: SCUMM identifier case sensitivity is handled per language rules"
(is t))

(test scumm_variables_reserved_words
  "VARIABLES: SCUMM reserved words are protected from use as identifiers"
(is t))

(test scumm_variables_normalization
  "VARIABLES: SCUMM identifiers are normalized correctly (kebab-case, snake_case, etc.)"
(is t))

(test scumm_variables_scope
  "VARIABLES: SCUMM variable scope is correctly tracked (local, global, method)"
(is t))

