;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-sm83 -*-
;;;
;;; EIGHTBOL EIGHTBOL SM83 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the SM83 backend code generation.
;;; See: src/backend-sm83/

(in-package :eightbol/test/backend-sm83)

(fiveam:def-suite :backend-sm83
  :description "SM83 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-sm83-move-node
  :description "SM83 move-node tests"
  :in :backend-sm83)

(in-suite :backend-sm83-move-node)


(test sm83_move_reg_to_reg
  "MOVE: SM83 register-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test sm83_move_mem_to_reg
  "MOVE: SM83 memory-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test sm83_move_immediate
  "MOVE: SM83 immediate-to-register moves are correctly generated"
  (skip "Implementation pending"))

(test sm83_move_multi_byte
  "MOVE: SM83 multi-byte moves are correctly generated with proper sequencing"
  (skip "Implementation pending"))

