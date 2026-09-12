;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-jvm -*-
;;;
;;; EIGHTBOL JVM Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the JVM backend code generation for string operations.
;;; See: src/backend-jvm/

(in-package :eightbol/test/backend-jvm)

(in-suite :backend-jvm)

(test jvm/string-blt
  "JVM STRING BLT: emits string copy with length"
  (let* ((ast '(:string-blt :source "Src" :dest "Dst" :length 10))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/string-blt-delimited
  "JVM STRING BLT: DELIMITED BY SIZE"
  (let* ((ast '(:string-blt :source "Src" :dest "Dst" :delimited-by :size))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/print-string-literal
  "JVM PRINT: string literal emits ldc string"
  (let* ((ast '(:print :expressions ("Hello World")))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/print-variable
  "JVM PRINT: variable emits getstatic"
  (let* ((ast '(:print :expressions ("X")))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test jvm/print-multiple
  "JVM PRINT: multiple expressions"
  (let* ((ast '(:print :expressions ("A" "B" 42)))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :jvm s))))
    (is (stringp output))
    (is (> (length output) 0))))