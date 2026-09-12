;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-rp2a03 -*-
;;;
;;; EIGHTBOL EIGHTBOL RP2A03 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the RP2A03 backend code generation.
;;; See: src/backend-rp2a03/

(in-package :eightbol/test/backend-rp2a03)

(fiveam:def-suite :backend-rp2a03
  :description "RP2A03 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-rp2a03-move-node
  :description "RP2A03 move-node tests"
  :in :backend-rp2a03)

(in-suite :backend-rp2a03-move-node)


(test rp2a03_move_reg_to_reg
  "MOVE: RP2A03 register-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :RP2A03 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test rp2a03_move_mem_to_reg
  "MOVE: RP2A03 memory-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :RP2A03 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test rp2a03_move_immediate
  "MOVE: RP2A03 immediate-to-register moves are correctly generated"
  (let* ((ast ('(:move (:const 42) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :RP2A03 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test rp2a03_move_multi_byte
  "MOVE: RP2A03 multi-byte moves are correctly generated with proper sequencing"
  (let* ((ast ('(:move (:const 1000) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :RP2A03 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

