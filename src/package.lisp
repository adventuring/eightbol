;; EIGHTBOL src/package.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC
(common-lisp:defpackage :eightbol
  (:use :cl :cl-change-case :yacc :alexandria :serapeum/bundle)
  (:shadow #:true)
  (:shadowing-import-from :serapeum #:partition #:comment #:occurs)
  (:import-from :split-sequence :split-sequence)
  (:export :main :compile-eightbol-class
           :+cpu-display-names+ :+supported-cpus+ :+object-reference-storage-width+
           :compile-to-assembly-with-ast-passes :parse-eightbol-string-for-codegen
           :eightbol-basic :read-class-name-designator
           :transpile-basic-to-cobol-string :compile-basic-from-path
           :class-id-from-bas-pathname :*basic-default-game-name*
           :source-error :copybook-not-found :copybook-invalid-name
   :copybook-read-error :backend-copy-not-expanded
   :usage-error :unknown-option-error :dangling-option-error :unknown-cpu-error
   :input-file-not-found :parse-failed-error :invalid-ast-error
           :undefined-class-reference :routine-not-terminated
           :validate-eightbol-program :collect-object-reference-classes-from-ast))

(in-package :eightbol)

