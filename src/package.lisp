;; src/package.lisp — EIGHTBOL core public API
;;; Copyright © 2026 Interworldly Adventuring, LLC
(cl:in-package :cl-user)

(defpackage #:eightbol
  (:use :cl :cl-change-case :yacc :alexandria :serapeum/bundle)
  ;;--- Shadow settings -------------------------------------------------------
  (:shadow #:true)
  (:shadowing-import-from :serapeum #:partition #:comment #:occurs)
  (:import-from :split-sequence :split-sequence)
  
  ;;--- Public exported symbols -----------------------------------------------
  (:export
   ;; Core entry points used by external tools
   :main
   :compile-eightbol
   :write-ast
   :make-path-name
   :with-open-file*
   :class-id-from-bas-pathname
   :validate-eightbol-program
   :collect-object-reference-classes-from-ast
   
   ;; BASIC‑specific API (exposed for external BASIC drivers)
   :basic-lex
   :parse-basic
   :basic-make-parser
   
   ;; Pascal API (exposed for external Pascal drivers)
   :pascal-lex
   :pascal-parse
   :pascal-tokenize
   
   ;; End‑to‑end compilation helpers (externally invoked)
   :compile-basic-from-path)

  (:shadowing-import-from :serapeum #:partition #:comment #:occurs)
  (:import-from :split-sequence :split-sequence))
