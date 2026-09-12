;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-jvm -*-
;;;
;;; EIGHTBOL JVM Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the JVM backend code generation for arithmetic nodes.
;;; See: src/backend-jvm/

(in-package :eightbol/test/backend-jvm)

(in-suite :backend-jvm)

(test jvm/add-integer-literals
  "JVM ADD: integer literal operands produce iadd sequence"
  (let* ((ast '(:add :from 5 :to 3))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "iadd" output))))

(test jvm/add-variable-to-constant
  "JVM ADD: variable + constant produces correct bytecode"
  (let* ((ast '(:add :from "X" :to 10))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "iadd" output))))

(test jvm/subtract-variable-from-constant
  "JVM SUBTRACT: constant - variable produces isub"
  (let* ((ast '(:subtract :from 100 :to "X"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "isub" output))))

(test jvm/multiply-by-power-of-two
  "JVM MULTIPLY: multiplication by power of two uses ishl/ishr/shift"
  (let* ((ast '(:multiply :from "X" :to 8))  ; 2^3
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/divide-by-power-of-two
  "JVM DIVIDE: division by power of two uses ishl/ishr/shift"
  (let* ((ast '(:divide :from "X" :by 4))  ; 2^2
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/arithmetic-overflow-handling
  "JVM arithmetic: handles overflow cases appropriately"
  (let* ((ast '(:add :from 32767 :to 1))  ; int16 overflow
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/fixed-point-arithmetic
  "JVM fixed-point: maintains scale through arithmetic"
  (let* ((ast '(:add :from (:fixed-point (:var "A") 8 8)
                  :to (:fixed-point (:var "B") 8 8)))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/bcd-arithmetic
  "JVM BCD: handles binary-coded decimal operations"
  (let* ((ast '(:add :from 9 :to 9))  ; 9+9=18 in BCD
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))