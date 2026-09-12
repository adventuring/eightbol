;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-wasm -*-
;;;
;;; EIGHTBOL WASM Backend Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the WASM backend end-to-end compilation.
;;; See: src/backend-wasm/

(in-package :eightbol/test/backend-wasm)

(in-suite :backend-wasm)

(test wasm/integration-full-program
  "WASM full program: compiles method with multiple statements"
  (let* ((ast '(:program :class-id "Character"
                      :methods ((:method :method-id "Think"
                                  :statements ((:move :from 1 :to "X")
                                               (:add :from "X" :to "Y")
                                               (:if (= "X" 0)
                                                    (:move :from 1 :to "Z")
                                                    (:move :from 2 :to "Z"))
                                               (:goback)))))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :wasm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "(module" output))
      (is (search "i32.add" output))
      (is (search "i32.eqz" output))
      (is (search "return i32.const 0" output))))

(test wasm/integration-numeric-types
  "WASM numeric types: handles various numeric values"
  (let* ((ast '(:program :class-id "Character"
                      :methods ((:method :method-id "Think"
                                  :statements ((:move :from 255 :to "A")
                                               (:move :from 65535 :to "B")
                                               (:move :from -1 :to "C")
                                               (:add :from "A" :to "B")
                                               (:subtract :from "B" :to "C")
                                               (:goback)))))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :wasm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "i32.const 255" output))
      (is (search "i32.const 65535" output))
      (is (search "i32.const -1" output))
      (is (search "i32.add" output))
      (is (search "i32.sub" output))))

(test wasm/integration-conditional-chain
  "WASM conditional chain: nested IF statements"
  (let* ((ast '(:program :class-id "Character"
                      :methods ((:method :method-id "Think"
                                  :statements ((:if (= "X" 0)
                                                      (:if (= "Y" 0)
                                                           (:move :from 1 :to "Z")
                                                           (:move :from 2 :to "Z"))
                                                      (:move :from 3 :to "Z"))
                                               (:goto "Exit")
                                               (:exit-program)))))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :wasm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "i32.eqz" output))
      (is (search "br" output))
      (is (search "return i32.const 0" output))))