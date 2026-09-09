;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-goal -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend GOAL Variable Names Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the GOAL language variable-names.
;;; See: src/frontend-goal/goal-parser.lisp

(in-package :eightbol/test/frontend-goal)

(fiveam:def-suite :goal-variable-names
  :description "GOAL variable-names tests"
  :in :frontend-goal)

(in-suite :goal-variable-names)


(test goal_variables_case_sensitivity
  "VARIABLES: GOAL identifier case sensitivity is handled per language rules"
  (skip "Implementation pending"))

(test goal_variables_reserved_words
  "VARIABLES: GOAL reserved words are protected from use as identifiers"
  (skip "Implementation pending"))

(test goal_variables_normalization
  "VARIABLES: GOAL identifiers are normalized correctly (kebab-case, snake_case, etc.)"
  (skip "Implementation pending"))

(test goal_variables_scope
  "VARIABLES: GOAL variable scope is correctly tracked (local, global, method)"
  (skip "Implementation pending"))

