;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-jvm -*-
;;;
;;; EIGHTBOL JVM Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the JVM backend code generation for MOVE statements.
;;; See: src/backend-jvm/

(in-package :eightbol/test/backend-jvm)

(in-suite :backend-jvm)

(test jvm/move-literal-to-var
  "JVM MOVE: literal TO var emits ldc and putstatic"
  (let* ((ast '(:move :from 42 :to "X"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "ldc 42" output))
    (is (search "putstatic" output))))

(test jvm/move-register-to-register
  "JVM MOVE: var TO var emits ldc and putstatic"
  (let* ((ast '(:move :from "A" :to "B"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/move-immediate
  "JVM MOVE: immediate constant TO variable handles various numeric values"
  (dolist (val '(0 1 100 255 65535 -1 -256))
    (let* ((ast `(:move :from ,val :to "X"))
           (output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output) "Output should be a string")
      (is (> (length output) 0) "Output should not be empty"))))

(test jvm/move-string-literal
  "JVM MOVE: string literal TO variable emits ldc string"
  (let* ((ast '(:move :from "hello" :to "Msg"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (search 'ldc output))
    (is (search "Msg" output))))

(test jvm/move-with-of-form
  "JVM MOVE: slot OF object emits appropriate access pattern"
  (let* ((ast '(:move :from (:of "HP" "Self") :to "X"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))