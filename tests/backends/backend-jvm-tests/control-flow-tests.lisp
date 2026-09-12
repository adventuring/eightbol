;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-jvm -*-
;;;
;;; EIGHTBOL JVM Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the JVM backend code generation for control-flow nodes.
;;; See: src/backend-jvm/

(in-package :eightbol/test/backend-jvm)

(in-suite :backend-jvm)

(test jvm/if-conditional
  "JVM IF: conditionals generate correct branch instructions"
  (let* ((ast '(:if (:const 1)
                    (:move :from 1 :to "X")
                    (:move :from 0 :to "X"))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "if_icmpne" output))))

(test jvm/if-else
  "JVM IF: ELSE branch emits correct branch chain"
  (let* ((ast '(:if (:const 1)
                    (:move :from 1 :to "X")
                    (:move :from 0 :to "Y"))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "goto" output))))

(test jvm/if-is-zero
  "JVM IF: IS ZERO condition emits ifne"
  (let* ((ast '(:if (:is-zero "X")
                    (:move :from 1 :to "X"))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "ifne" output))))

(test jvm/goto
  "JVM GO TO: target emits goto"
  (let* ((ast '(:goto :target "Exit")))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "goto Exit" output))))

(test jvm/perform-loop
  "JVM PERFORM: procedure call emits invokestatic"
  (let* ((ast '(:perform :procedure "Foo")))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "invokestatic" output))))

(test jvm/perform-until
  "JVM PERFORM UNTIL: condition-checked loop emits branch"
  (let* ((ast '(:perform :procedure "Loop" :until (= "X" 0)
                    :body ((:move :from 1 :to "Y")))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "goto" output))))

(test jvm/goback
  "JVM GOBACK emits return"
  (let* ((ast '(:goback))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "return" output))))

(test jvm/exit-method
  "JVM EXIT METHOD emits return"
  (let* ((ast '(:exit-method))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "return" output))))

(test jvm/exit-program
  "JVM EXIT PROGRAM emits return"
  (let* ((ast '(:exit-program))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "return" output))))