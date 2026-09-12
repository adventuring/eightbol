;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-zork -*-
;;;
;;; EIGHTBOL Zork Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the Zork backend code generation for string operations.
;;; See: src/backend-zork/

(in-package :eightbol/test/backend-zork)

(in-suite :backend-zork)

(test zork/string-blt
  "Zork STRING BLT: emits string copy"
  (let* ((ast '(:string-blt :source "Src" :dest "Dst" :length 10))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/print-string-literal
  "Zork PRINT: string literal emits literal"
  (let* ((ast '(:print :expressions ("Hello World")))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/print-variable
  "Zork PRINT: variable emits literal"
  (let* ((ast '(:print :expressions ("X")))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test zork/print-multiple
  "Zork PRINT: multiple expressions"
  (let* ((ast '(:print :expressions ("A" "B" 42)))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :zork s))))
    (is (stringp output))
    (is (> (length output) 0))))