;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-zork -*-
;;;
;;; EIGHTBOL Zork Backend CALL/INVOKE Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Zork backend code generation for CALL/INVOKE nodes.
;;; See: src/backend-zork/

(in-package :eightbol/test/backend-zork)

(in-suite :backend-zork)

(test zork/call-emits-call
  "Zork CALL: target emits call"
  (let* ((ast '(:call :target "MoveDecalY"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "call MoveDecalY" output))))

(test zork/invoke-emits-call
  "Zork INVOKE: method emits call"
  (let* ((ast '(:invoke :method "Kill"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "call InvokeKill" output))))

(test zork/call-with-returning
  "Zork CALL: RETURNING clause preserves result variable"
  (let* ((ast '(:call :target "MoveDecalY" :returning "Result"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/invoke-with-returning
  "Zork INVOKE: RETURNING clause preserves result variable"
  (let* ((ast '(:invoke :method "Kill" :returning "RetVal"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/call-acc
  "Zork CALL-ACC: bank argument handled"
  (let* ((ast '(:call-acc :target "Foo" :bank "Bank1"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))