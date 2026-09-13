;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-zork -*-
;;;
;;; EIGHTBOL Zork Backend Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Zork backend end-to-end compilation.
;;; See: src/backend-zork/

(in-package :eightbol/test/backend-zork)

(in-suite :backend-zork)

(test zork/integration-full-program
  "Zork full program: compiles method with multiple statements"
  (let* ((ast '(:program :class-id "Character"
                      :methods ((:method :method-id "Think"
                                  :statements ((:move :from 1 :to "X")
                                               (:+ :from "X" :to "Y")
                                               (:if (= "X" 0)
                                                    (:move :from 1 :to "Z")
                                                    (:move :from 2 :to "Z"))
                                               (:goback)))))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :zork s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "Zork" output))
      (is (search "iadd" output))
      (is (search "branch=" output))
      (is (search "(ret)" output))))

(test zork/integration-numeric-types
  "Zork numeric types: handles various numeric values"
  (let* ((ast '(:program :class-id "Character"
                      :methods ((:method :method-id "Think"
                                  :statements ((:move :from 255 :to "A")
                                               (:move :from 65535 :to "B")
                                               (:move :from -1 :to "C")
                                               (:+ :from "A" :to "B")
                                               (:- :from "B" :to "C")
                                               (:goback)))))))
    (let* ((output (with-output-to-string (s)
                     (eightbol:compile-to-assembly ast :zork s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "literal 255" output))
      (is (search "literal 65535" output))
      (is (search "literal -1" output))
      (is (search "iadd" output))
      (is (search "isub" output))))

(test zork/integration-conditional-chain
  "Zork conditional chain: nested IF statements"
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
                     (eightbol:compile-to-assembly ast :zork s))))
      (is (stringp output))
      (is (> (length output) 0))
      (is (search "branch=" output))
      (is (search "branch Exit" output))
      (is (search "(ret)" output))))