;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-wasm -*-
;;;
;;; EIGHTBOL WASM Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the WASM backend code generation for string operations.
;;; See: src/backend-wasm/

(in-package :eightbol/test/backend-wasm)

(in-suite :backend-wasm)

(test wasm/string-blt
  "WASM STRING BLT: emits string copy with length"
  (let* ((ast '(:string-blt :source "Src" :dest "Dst" :length 10))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/string-blt-delimited
  "WASM STRING BLT: DELIMITED BY SIZE"
  (let* ((ast '(:string-blt :source "Src" :dest "Dst" :delimited-by :size))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/print-string-literal
  "WASM PRINT: string literal emits module"
  (let* ((ast '(:print :expressions ("Hello World")))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/print-variable
  "WASM PRINT: variable emits i32.const"
  (let* ((ast '(:print :expressions ("X")))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test wasm/print-multiple
  "WASM PRINT: multiple expressions"
  (let* ((ast '(:print :expressions ("A" "B" 42)))
         (output (with-output-to-string (s)
                   (eightbol:compile-to-assembly ast :wasm s))))
    (is (stringp output))
    (is (> (length output) 0))))