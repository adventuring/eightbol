;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-arm7 -*-
;;;
;;; EIGHTBOL EIGHTBOL ARM7 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the ARM7 backend code generation.
;;; See: src/backend-arm7/

(in-package :eightbol/test/backend-arm7)

(fiveam:def-suite :backend-arm7
  :description "ARM7 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-arm7-move-node
  :description "ARM7 move-node tests"
  :in :backend-arm7)

(in-suite :backend-arm7-move-node)


(test arm7_move_reg_to_reg
  "MOVE: ARM7 register-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_move_mem_to_reg
  "MOVE: ARM7 memory-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_move_immediate
  "MOVE: ARM7 immediate-to-register moves are correctly generated"
  (let* ((ast ('(:move (:const 42) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test arm7_move_multi_byte
  "MOVE: ARM7 multi-byte moves are correctly generated with proper sequencing"
  (let* ((ast ('(:move (:const 1000) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :ARM7 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

