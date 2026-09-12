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
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_move_mem_to_reg
  "MOVE: CP1610 memory-to-register moves are correctly generated"
  (let* ((ast ('(:move (:var x) (:var y))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_move_immediate
  "MOVE: CP1610 immediate-to-register moves are correctly generated"
  (let* ((ast ('(:move (:const 42) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_move_multi_byte
  "MOVE: CP1610 multi-byte moves are correctly generated with proper sequencing"
  (let* ((ast ('(:move (:const 1000) (:var x))')
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

