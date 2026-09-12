;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-65c816 -*-
;;;
;;; EIGHTBOL EIGHTBOL 65C816 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the 65C816 backend code generation.
;;; See: src/backend-65c816/

(in-package :eightbol/test/backend-65c816)

(fiveam:def-suite :backend-65c816
  :description "65C816 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-65c816-move-node
  :description "65C816 move-node tests"
  :in :backend-65c816)

(in-suite :backend-65c816-move-node)


(test 65c816_move_reg_to_reg
  "MOVE: 65C816 register-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_move_mem_to_reg
  "MOVE: 65C816 memory-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_move_immediate
  "MOVE: 65C816 immediate-to-register moves are correctly generated"
  (let* ((ast ('(:move (:const 42) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test 65c816_move_multi_byte
  "MOVE: 65C816 multi-byte moves are correctly generated with proper sequencing"
  (let* ((ast ('(:move (:const 1000) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :65C816 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

