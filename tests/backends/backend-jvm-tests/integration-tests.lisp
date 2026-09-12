;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-jvm -*-
;;;
;;; EIGHTBOL JVM Backend Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the JVM backend end-to-end compilation.
;;; See: src/backend-jvm/

(in-package :eightbol/test/backend-jvm)

(in-suite :backend-jvm)

(test jvm/integration-full-program
  "JVM full program: compiles method with multiple statements"
  (let* ((ast '(:program :class-id "Character"
                      :methods ((:method :method-id "Think"
                                  :statements ((:move :from 1 :to "X")
                                               (:add :from "X" :to "Y")
                                               (:if (= "X" 0)
                                                    (:move :from 1 :to "Z")
                                                    (:move :from 2 :to "Z"))
                                               (:goback)))))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search ".class public Character" output))
      (is (search "iadd" output))
      (is (search "if_icmpne" output))
      (is (search "return" output))))

(test jvm/integration-numeric-types
  "JVM numeric types: handles various numeric values"
  (let* ((ast '(:program :class-id "Character"
                      :methods ((:method :method-id "Think"
                                  :statements ((:move :from 255 :to "A")
                                               (:move :from 65535 :to "B")
                                               (:move :from -1 :to "C")
                                               (:add :from "A" :to "B")
                                               (:subtract :from "B" :to "C")
                                               (:goback)))))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "ldc 255" output))
      (is (search "ldc 65535" output))
      (is (search "ldc -1" output))
      (is (search "iadd" output))
      (is (search "isub" output))))

(test jvm/integration-conditional-chain
  "JVM conditional chain: nested IF statements"
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
                     (eightbol:compile-to-assembly ast :jvm s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "if_icmpne" output))
      (is (search "goto" output))
      (is (search "return" output))))