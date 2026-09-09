;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-z80 -*-
;;;
;;; EIGHTBOL EIGHTBOL Z80 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Z80 backend code generation.
;;; See: src/backend-z80/

(in-package :eightbol/test/backend-z80)

(fiveam:def-suite :backend-z80
  :description "Z80 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-z80-move-node
  :description "Z80 move-node tests"
  :in :backend-z80)

(in-suite :backend-z80-move-node)


(test z80_move_reg_to_reg
  "MOVE: Z80 register-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test z80_move_mem_to_reg
  "MOVE: Z80 memory-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test z80_move_immediate
  "MOVE: Z80 immediate-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test z80_move_multi_byte
  "MOVE: Z80 multi-byte moves are correctly generated with proper sequencing"
  (skip "Implementation pending"))

