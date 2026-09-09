;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-cp1610 -*-
;;;
;;; EIGHTBOL EIGHTBOL CP1610 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the CP1610 backend code generation.
;;; See: src/backend-cp1610/

(in-package :eightbol/test/backend-cp1610)

(fiveam:def-suite :backend-cp1610
  :description "CP1610 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-cp1610-move-node
  :description "CP1610 move-node tests"
  :in :backend-cp1610)

(in-suite :backend-cp1610-move-node)


(test cp1610_move_reg_to_reg
  "MOVE: CP1610 register-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test cp1610_move_mem_to_reg
  "MOVE: CP1610 memory-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test cp1610_move_immediate
  "MOVE: CP1610 immediate-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test cp1610_move_multi_byte
  "MOVE: CP1610 multi-byte moves are correctly generated with proper sequencing"
  (skip "Implementation pending"))

