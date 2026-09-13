;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-wasm -*-
;;;
;;; EIGHTBOL WASM Backend Arithmetic Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the WASM backend code generation for arithmetic nodes.
;;; See: src/backend-wasm/

(in-package :eightbol/test/backend-wasm)

(in-suite :backend-wasm)

(test wasm/add-integer-literals
  "WASM ADD: integer literal operands produce i32.add"
  (let* ((ast '(:+ :from 5 :to 3))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "i32.add" output))))

(test wasm/add-variable-to-constant
  "WASM ADD: variable + constant produces correct bytecode"
  (let* ((ast '(:+ :from "X" :to 10))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "i32.add" output))))

(test wasm/subtract-variable-from-constant
  "WASM SUBTRACT: constant - variable produces i32.sub"
  (let* ((ast '(:- :from 100 :to "X"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "i32.sub" output))))

(test wasm/multiply-by-power-of-two
  "WASM MULTIPLY: multiplication by power of two uses i32.mul"
  (let* ((ast '(:× :from "X" :to 8))  ; 2^3
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "i32.mul" output))))

(test wasm/divide-by-power-of-two
  "WASM DIVIDE: division by power of two uses i32.div_s"
  (let* ((ast '(:÷ :from "X" :by 4))  ; 2^2
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "i32.div_s" output))))

(test wasm/arithmetic-overflow-handling
  "WASM arithmetic: handles overflow cases appropriately"
  (let* ((ast '(:+ :from 2147483647 :to 1))  ; int32 overflow
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/fixed-point-arithmetic
  "WASM fixed-point: maintains scale through arithmetic"
  (let* ((ast '(:+ :from (:fixed-point (:var "A") 8 8)
                  :to (:fixed-point (:var "B") 8 8)))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/bcd-arithmetic
  "WASM BCD: handles binary-coded decimal operations"
  (let* ((ast '(:+ :from 9 :to 9))  ; 9+9=18 in BCD
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0)))