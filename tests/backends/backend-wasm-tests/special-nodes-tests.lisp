;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-wasm -*-
;;;
;;; EIGHTBOL WASM Backend Special Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the WASM backend code generation for special nodes.
;;; See: src/backend-wasm/

(in-package :eightbol/test/backend-wasm)

(in-suite :backend-wasm)

(test wasm/log-fault
  "WASM LOG FAULT emits comment"
  (let* ((ast '(:log-fault :code 1234))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "LOG FAULT" output))))

(test wasm/debug-break
  "WASM DEBUG BREAK emits comment"
  (let* ((ast '(:debug-break :code 42))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))
    (is (search "DEBUG BREAK" output))))

(test wasm/dialogue-emits-comment
  "WASM DIALOGUE emits comment block"
  (let* ((ast '(:dialogue :speaker "NPC" :text "Hello"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/print-emits-module
  "WASM PRINT emits module structure"
  (let* ((ast '(:print :expressions ("Hello")))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/comment-statement
  "WASM COMMENT emits WASM comment"
  (let* ((ast '(:comment "This is a comment"))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))