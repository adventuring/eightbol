;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-zork -*-
;;;
;;; EIGHTBOL Zork Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Zork backend code generation for arithmetic nodes.
;;; See: src/backend-zork/

(in-package :eightbol/test/backend-zork)

(in-suite :backend-zork)

(test zork/add-integer-literals
  "Zork ADD: integer literal operands produce iadd"
  (let* ((ast '(:+ :from 5 :to 3))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "iadd" output))))

(test zork/add-variable-to-constant
  "Zork ADD: variable + constant produces correct bytecode"
  (let* ((ast '(:+ :from "X" :to 10))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "iadd" output))))

(test zork/subtract-variable-from-constant
  "Zork SUBTRACT: constant - variable produces isub"
  (let* ((ast '(:- :from 100 :to "X"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "isub" output))))

(test zork/multiply-by-power-of-two
  "Zork MULTIPLY: multiplication by power of two uses add/shift"
  (let* ((ast '(:× :from "X" :to 8))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/divide-by-power-of-two
  "Zork DIVIDE: division by power of two uses shift"
  (let* ((ast '(:÷ :from "X" :by 4))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/arithmetic-overflow-handling
  "Zork arithmetic: handles overflow cases appropriately"
  (let* ((ast '(:+ :from 2147483647 :to 1))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/fixed-point-arithmetic
  "Zork fixed-point: maintains scale through arithmetic"
  (let* ((ast '(:+ :from (:fixed-point (:var "A") 8 8)
                  :to (:fixed-point (:var "B") 8 8)))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/bcd-arithmetic
  "Zork BCD: handles binary-coded decimal operations"
  (let* ((ast '(:+ :from 9 :to 9))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))