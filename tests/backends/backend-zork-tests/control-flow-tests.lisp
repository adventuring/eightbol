;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-zork -*-
;;;
;;; EIGHTBOL Zork Backend Control Flow Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Zork backend code generation for control-flow nodes.
;;; See: src/backend-zork/

(in-package :eightbol/test/backend-zork)

(in-suite :backend-zork)

(test zork/if-conditional
  "Zork IF: conditionals generate correct branch instructions"
  (let* ((ast '(:if (:const 1)
                    (:move :from 1 :to "X")
                    (:move :from 0 :to "X"))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :zork s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "branch=" output))))

(test zork/if-else
  "Zork IF: ELSE branch emits correct branch chain"
  (let* ((ast '(:if (:const 1)
                    (:move :from 1 :to "X")
                    (:move :from 0 :to "Y"))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :zork s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "branch" output))))

(test zork/if-is-zero
  "Zork IF: IS ZERO condition emits branch=~"
  (let* ((ast '(:if (:is-zero "X")
                    (:move :from 1 :to "X"))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :zork s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "branch=" output))))

(test zork/goto
  "Zork GO TO: target emits branch"
  (let* ((ast '(:goto :target "Exit")))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :zork s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "branch Exit" output))))

(test zork/perform-loop
  "Zork PERFORM: procedure call emits call"
  (let* ((ast '(:perform :procedure "Foo")))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :zork s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "call" output))))

(test zork/perform-until
  "Zork PERFORM UNTIL: condition-checked loop emits branch"
  (let* ((ast '(:perform :procedure "Loop" :until (= "X" 0)
                    :body ((:move :from 1 :to "Y")))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :zork s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "branch" output))))

(test zork/goback
  "Zork GOBACK emits ret"
  (let* ((ast '(:goback))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "(ret)" output))))

(test zork/exit-method
  "Zork EXIT METHOD emits ret"
  (let* ((ast '(:exit-method))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "(ret)" output))))

(test zork/exit-program
  "Zork EXIT PROGRAM emits ret"
  (let* ((ast '(:exit-program))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "(ret)" output))))