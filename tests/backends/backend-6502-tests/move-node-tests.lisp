;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-6502 -*-
;;;
;;; EIGHTBOL EIGHTBOL 6502 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 6502 backend code generation.
;;; See: src/backend-6502/

(in-package :eightbol/test/backend-6502)

(fiveam:def-suite :backend-6502
  :description "6502 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-6502-move-node
  :description "6502 move-node tests"
  :in :backend-6502)

(in-suite :backend-6502-move-node)


(test 6502_move_reg_to_reg
  "MOVE: 6502 register-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test 6502_move_mem_to_reg
  "MOVE: 6502 memory-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test 6502_move_immediate
  "MOVE: 6502 immediate-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test 6502_move_multi_byte
  "MOVE: 6502 multi-byte moves are correctly generated with proper sequencing"
  (skip "Implementation pending"))

