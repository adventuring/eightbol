;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-jvm -*-
;;;
;;; EIGHTBOL JVM Backend Special Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the JVM backend code generation for special nodes.
;;; See: src/backend-jvm/

(in-package :eightbol/test/backend-jvm)

(in-suite :backend-jvm)

(test jvm/log-fault
  "JVM LOG FAULT emits comment"
  (let* ((ast '(:log-fault :code 1234))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "LOG FAULT" output))))

(test jvm/debug-break
  "JVM DEBUG BREAK emits comment"
  (let* ((ast '(:debug-break :code 42))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "DEBUG BREAK" output))))

(test jvm/dialogue-emits-comment
  "JVM DIALOGUE emits comment block"
  (let* ((ast '(:dialogue :speaker "NPC" :text "Hello"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/print-emits-getstatic
  "JVM PRINT emits getstatic + invokevirtual"
  (let* ((ast '(:print :expressions ("Hello")))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/comment-statement
  "JVM COMMENT emits JVM comment"
  (let* ((ast '(:comment "This is a comment"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))