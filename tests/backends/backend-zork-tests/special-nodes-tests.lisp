;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-zork -*-
;;;
;;; EIGHTBOL Zork Backend Special Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Zork backend code generation for special nodes.
;;; See: src/backend-zork/

(in-package :eightbol/test/backend-zork)

(in-suite :backend-zork)

(test zork/log-fault
  "Zork LOG FAULT emits comment"
  (let* ((ast '(:log-fault :code 1234))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "LOG FAULT" output))))

(test zork/debug-break
  "Zork DEBUG BREAK emits comment"
  (let* ((ast '(:debug-break :code 42))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "DEBUG BREAK" output))))

(test zork/dialogue-emits-comment
  "Zork DIALOGUE emits comment block"
  (let* ((ast '(:dialogue :speaker "NPC" :text "Hello"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/print-emits-literal
  "Zork PRINT emits literal"
  (let* ((ast '(:print :expressions ("Hello")))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/comment-statement
  "Zork COMMENT emits comment"
  (let* ((ast '(:comment "This is a comment"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))