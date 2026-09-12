;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-wasm -*-
;;;
;;; EIGHTBOL WASM Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the WASM backend code generation for control-flow nodes.
;;; See: src/backend-wasm/

(in-package :eightbol/test/backend-wasm)

(in-suite :backend-wasm)

(test wasm/if-conditional
  "WASM IF: conditionals generate correct branch instructions"
  (let* ((ast '(:if (:const 1)
                    (:move :from 1 :to "X")
                    (:move :from 0 :to "X"))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :wasm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "i32.eqz" output))))

(test wasm/if-else
  "WASM IF: ELSE branch emits correct branch chain"
  (let* ((ast '(:if (:const 1)
                    (:move :from 1 :to "X")
                    (:move :from 0 :to "Y"))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :wasm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "br" output))))

(test wasm/if-is-zero
  "WASM IF: IS ZERO condition emits i32.eqz"
  (let* ((ast '(:if (:is-zero "X")
                    (:move :from 1 :to "X"))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :wasm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "i32.eqz" output))))

(test wasm/goto
  "WASM GO TO: target emits br"
  (let* ((ast '(:goto :target "Exit")))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :wasm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "br Exit" output))))

(test wasm/perform-loop
  "WASM PERFORM: procedure call emits call"
  (let* ((ast '(:perform :procedure "Foo")))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :wasm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "call" output))))

(test wasm/perform-until
  "WASM PERFORM UNTIL: condition-checked loop emits br"
  (let* ((ast '(:perform :procedure "Loop" :until (= "X" 0)
                    :body ((:move :from 1 :to "Y")))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :wasm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "br" output))))

(test wasm/goback
  "WASM GOBACK emits return"
  (let* ((ast '(:goback))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "return i32.const 0" output))))

(test wasm/exit-method
  "WASM EXIT METHOD emits return"
  (let* ((ast '(:exit-method))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "return i32.const 0" output))))

(test wasm/exit-program
  "WASM EXIT PROGRAM emits return"
  (let* ((ast '(:exit-program))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "return i32.const 0" output))))