;; src/frontend-garry/package.lisp — Garry frontend package
;;; Copyright © 2026 Interworldly Adventuring, LLC
(cl:in-package :cl-user)

(defpackage #:frontend-garry
  (:use :cl :cl-change-case :yacc :alexandria :serapeum/bundle)
  (:shadow #:describe)
  (:import-from :unix-opts #:define-opts #:get-opts #:option #:exit)
  (:import-from :split-sequence :split-sequence)
  
  (:export
   ;; Lexer functions
   :garry-lex-line
   :garry-lex-source
   
   ;; Parser functions
   :make-garry-parser
   
   ;; API functions
   :compile-garry-from-path
   :compile-garry-string))