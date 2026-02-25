;; EIGHTBOL src/package.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC
(common-lisp:defpackage :eightbol
  (:use :cl :cl-change-case :yacc :alexandria :serapeum)
  (:shadowing-import-from :serapeum #:partition)
  (:import-from :split-sequence :split-sequence)
  (:export :main :compile-eightbol-class
           :source-error :copybook-not-found :copybook-invalid-name
   :copybook-read-error :backend-copy-not-expanded))

(common-lisp:defpackage :eightbol/test
  (:use :fiveam :eightbol :cl :cl-change-case))

(common-lisp:in-package :eightbol/test)
;; NOTE: fiveam:run! :eightbol may report "Didn't run anything" in some contexts;
;; asdf:test-system :eightbol appears to run correctly. Defer investigation.
(fiveam:def-suite :eightbol
  :description "Tests for EIGHTBOL compiler (general)")
