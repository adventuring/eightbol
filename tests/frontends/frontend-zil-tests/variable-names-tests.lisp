;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-zil -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend ZIL Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ZIL language variable-names.
;;; See: src/frontend-zil/zil-parser.lisp

(in-package :eightbol/test/frontend-zil)

(fiveam:def-suite :zil-variable-names
  :description "ZIL variable-names tests"
  :in :frontend-zil)

(in-suite :zil-variable-names)


(test zil_variables_case_sensitivity
  "VARIABLES: ZIL identifier case sensitivity is handled per language rules"
  (skip "Implementation pending"))

(test zil_variables_reserved_words
  "VARIABLES: ZIL reserved words are protected from use as identifiers"
  (skip "Implementation pending"))

(test zil_variables_normalization
  "VARIABLES: ZIL identifiers are normalized correctly (kebab-case, snake_case, etc.)"
  (skip "Implementation pending"))

(test zil_variables_scope
  "VARIABLES: ZIL variable scope is correctly tracked (local, global, method)"
  (skip "Implementation pending"))

