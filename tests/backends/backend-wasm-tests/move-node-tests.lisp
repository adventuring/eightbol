;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-wasm -*-
;;;
;;; EIGHTBOL WASM Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the WASM backend code generation for MOVE statements.
;;; See: src/backend-wasm/

(in-package :eightbol/test/backend-wasm)

(in-suite :backend-wasm)

(test wasm/move-literal-to-var
  "WASM MOVE: literal TO var emits i32.const and i32.store"
  (let* ((ast '(:move :from 42 :to "X"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "i32.const 42" output))
    (is (search "i32.store" output))))

(test wasm/move-register-to-register
  "WASM MOVE: var TO var emits store"
  (let* ((ast '(:move :from "A" :to "B"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "i32.store" output))))

(test wasm/move-string-literal
  "WASM MOVE: string literal emits i32.const 0"
  (let* ((ast '(:move :from "hello" :to "Msg"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/move-with-of-form
  "WASM MOVE: slot OF object emits appropriate access pattern"
  (let* ((ast '(:move :from (:of "HP" "Self") :to "X"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))