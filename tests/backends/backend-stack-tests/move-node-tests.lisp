;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-stack -*-
;;;
;;; EIGHTBOL EIGHTBOL STACK Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the STACK backend code generation.
;;; See: src/backend-stack/

(in-package :eightbol/test/backend-stack)

(fiveam:def-suite :backend-stack
  :description "STACK backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-stack-move-node
  :description "STACK move-node tests"
  :in :backend-stack)

(in-suite :backend-stack-move-node)


(test stack_move_reg_to_reg
  "MOVE: STACK register-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test stack_move_mem_to_reg
  "MOVE: STACK memory-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test stack_move_immediate
  "MOVE: STACK immediate-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test stack_move_multi_byte
  "MOVE: STACK multi-byte moves are correctly generated with proper sequencing"
  (skip "Implementation pending"))

