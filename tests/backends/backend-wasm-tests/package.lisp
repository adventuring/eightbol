;;; tests/backends/backend-wasm-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-wasm
  (:use :fiveam :common-lisp :eightbol))

(in-package :eightbol/test/backend-wasm)

(fiveam:def-suite :backend-wasm
  :description "WebAssembly backend code generation tests")
