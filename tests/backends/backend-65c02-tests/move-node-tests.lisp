;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c02 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C02 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 65C02 backend code generation.
;;; See: src/backend-65c02/

(in-package :eightbol/test/backend-65c02)

(fiveam:def-suite :backend-65c02
  :description "65C02 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-65c02-move-node
  :description "65C02 move-node tests"
  :in :backend-65c02)

(in-suite :backend-65c02-move-node)


(test 65c02_move_reg_to_reg
  "MOVE: 65C02 register-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test 65c02_move_mem_to_reg
  "MOVE: 65C02 memory-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test 65c02_move_immediate
  "MOVE: 65C02 immediate-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test 65c02_move_multi_byte
  "MOVE: 65C02 multi-byte moves are correctly generated with proper sequencing"
  (skip "Implementation pending"))

