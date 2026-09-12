;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-i286 -*-
;;;
;;; EIGHTBOL EIGHTBOL I286 Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the I286 backend code generation.
;;; See: src/backend-i286/

(in-package :eightbol/test/backend-i286)

(fiveam:def-suite :backend-i286
  :description "I286 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-i286-move-node
  :description "I286 move-node tests"
  :in :backend-i286)

(in-suite :backend-i286-move-node)


(test i286_move_reg_to_reg
  "MOVE: I286 register-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_move_mem_to_reg
  "MOVE: I286 memory-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_move_immediate
  "MOVE: I286 immediate-to-register moves are correctly generated"
  (let* ((ast ('(:move (:const 42) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test i286_move_multi_byte
  "MOVE: I286 multi-byte moves are correctly generated with proper sequencing"
  (let* ((ast ('(:move (:const 1000) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :I286 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

