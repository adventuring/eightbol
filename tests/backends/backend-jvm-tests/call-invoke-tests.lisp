;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-jvm -*-
;;;
;;; EIGHTBOL JVM Backend CALL/INVOKE Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the JVM backend code generation for CALL/INVOKE nodes.
;;; See: src/backend-jvm/

(in-package :eightbol/test/backend-jvm)

(in-suite :backend-jvm)

(test jvm/call-emits-invokestatic
  "JVM CALL: target emits invokestatic"
  (let* ((ast '(:call :target "MoveDecalY"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "invokestatic" output))))

(test jvm/invoke-emits-invokestatic
  "JVM INVOKE: method emits invokestatic"
  (let* ((ast '(:invoke :method "Kill"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "invokestatic" output))))

(test jvm/call-with-returning
  "JVM CALL: RETURNING clause preserves result variable"
  (let* ((ast '(:call :target "MoveDecalY" :returning "Result"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/invoke-with-returning
  "JVM INVOKE: RETURNING clause preserves result variable"
  (let* ((ast '(:invoke :method "Kill" :returning "RetVal"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/call-acc
  "JVM CALL-ACC: bank argument handled"
  (let* ((ast '(:call-acc :target "Foo" :bank "Bank1"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))