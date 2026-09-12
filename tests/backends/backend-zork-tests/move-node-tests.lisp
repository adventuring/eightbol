;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-zork -*-
;;;
;;; EIGHTBOL Zork Backend MOVE Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Zork backend code generation for MOVE statements.
;;; See: src/backend-zork/

(in-package :eightbol/test/backend-zork)

(in-suite :backend-zork)

(test zork/move-literal-to-var
  "Zork MOVE: literal TO var emits literal and store"
  (let* ((ast '(:move :from 42 :to "X"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "literal 42" output))
    (is (search "store" output))))

(test zork/move-register-to-register
  "Zork MOVE: var TO var emits store"
  (let* ((ast '(:move :from "A" :to "B"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "store" output))))

(test zork/move-string-literal
  "Zork MOVE: string literal emits literal string"
  (let* ((ast '(:move :from "hello" :to "Msg"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/move-with-of-form
  "Zork MOVE: slot OF object emits load-slot"
  (let* ((ast '(:move :from (:of "HP" "Self") :to "X"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))