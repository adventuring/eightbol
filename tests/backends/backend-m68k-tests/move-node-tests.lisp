;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-m68k -*-
;;;
;;; EIGHTBOL EIGHTBOL M68K Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the M68K backend code generation.
;;; See: src/backend-m68k/

(in-package :eightbol/test/backend-m68k)

(fiveam:def-suite :backend-m68k
  :description "M68K backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-m68k-move-node
  :description "M68K move-node tests"
  :in :backend-m68k)

(in-suite :backend-m68k-move-node)


(test m68k_move_reg_to_reg
  "MOVE: M68K register-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test m68k_move_mem_to_reg
  "MOVE: M68K memory-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test m68k_move_immediate
  "MOVE: M68K immediate-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test m68k_move_multi_byte
  "MOVE: M68K multi-byte moves are correctly generated with proper sequencing"
  (skip "Implementation pending"))

