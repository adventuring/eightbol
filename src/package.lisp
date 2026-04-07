;; EIGHTBOL src/package.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC
(common-lisp:defpackage :eightbol
  (:use :cl :cl-change-case :yacc :alexandria :serapeum/bundle)
  (:shadowing-import-from :serapeum #:partition #:comment #:occurs)
  (:import-from :split-sequence :split-sequence)
  (:export :main :compile-eightbol-class
           :+cpu-display-names+ :+supported-cpus+
           :compile-to-assembly-with-ast-passes :parse-eightbol-string-for-codegen
           :source-error :copybook-not-found :copybook-invalid-name
   :copybook-read-error :backend-copy-not-expanded
   :usage-error :unknown-option-error :dangling-option-error :unknown-cpu-error
   :input-file-not-found :parse-failed-error :invalid-ast-error
           :undefined-class-reference :routine-not-terminated
           :validate-eightbol-program :collect-object-reference-classes-from-ast))

(defpackage :eightbol/test
  (:use :fiveam :eightbol :cl :cl-change-case))

(in-package :eightbol/test)
(fiveam:def-suite :eightbol
  :description "Tests for EIGHTBOL compiler (general)")

(in-package :eightbol)

