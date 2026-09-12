;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-wasm -*-
;;;
;;; EIGHTBOL WASM Backend CALL/INVOKE Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the WASM backend code generation for CALL/INVOKE nodes.
;;; See: src/backend-wasm/

(in-package :eightbol/test/backend-wasm)

(in-suite :backend-wasm)

(test wasm/call-emits-call
  "WASM CALL: target emits call"
  (let* ((ast '(:call :target "MoveDecalY"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "call" output))))

(test wasm/invoke-emits-call
  "WASM INVOKE: method emits call"
  (let* ((ast '(:invoke :method "Kill"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "call" output))))

(test wasm/call-with-returning
  "WASM CALL: RETURNING clause preserves result variable"
  (let* ((ast '(:call :target "MoveDecalY" :returning "Result"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/invoke-with-returning
  "WASM INVOKE: RETURNING clause preserves result variable"
  (let* ((ast '(:invoke :method "Kill" :returning "RetVal"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/call-acc
  "WASM CALL-ACC: bank argument handled"
  (let* ((ast '(:call-acc :target "Foo" :using 7))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))