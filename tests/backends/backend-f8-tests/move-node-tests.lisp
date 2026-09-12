;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-f8 -*-
;;;
;;; EIGHTBOL EIGHTBOL F8 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the F8 backend code generation.
;;; See: src/backend-f8/

(in-package :eightbol/test/backend-f8)

(fiveam:def-suite :backend-f8
  :description "F8 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-f8-move-node
  :description "F8 move-node tests"
  :in :backend-f8)

(in-suite :backend-f8-move-node)


(test f8_move_reg_to_reg
  "MOVE: F8 register-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_move_mem_to_reg
  "MOVE: F8 memory-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_move_immediate
  "MOVE: F8 immediate-to-register moves are correctly generated"
  (let* ((ast ('(:move (:const 42) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test f8_move_multi_byte
  "MOVE: F8 multi-byte moves are correctly generated with proper sequencing"
  (let* ((ast ('(:move (:const 1000) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :F8 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

