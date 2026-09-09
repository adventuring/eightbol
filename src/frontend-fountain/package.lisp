;;;; src/frontend-fountain/package.lisp
;;;; Package definition for Fountain language frontend
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :fountain-frontend
  (:use :cl)
  (:export
   ;; Lexer functions
   :lex-fountain-source
   :lex-fountain-file
   :to-pascal-case
   ;; Token manipulation
   :make-token
   :token-type
   :token-value
   :token-line
   :token-column
   ;; Parser functions
   :parse-fountain-source
   :parse-fountain-file
   :parse-fountain-tokens
   ;; Canonical AST node constructors
   :make-move-node
   :make-add-expr
   :make-subtract-expr
   :make-multiply-expr
   :make-divide-expr
   :make-shift-left-expr
   :make-shift-right-expr
   :make-if-node
   :make-perform-node
   :make-procedure-node
   :make-goto-node
   :make-null-expr
   :make-self-expr
   :make-null-test
   :make-not-null-test
   :make-inspect-node
   :make-subscript-expr
   :make-refmod-expr
   :make-address-of-expr
   :make-string-blt-node
   :make-evaluate-node
   :make-when-clause
   :make-when-other-clause
   ;; Tests
   :run-all-tests))

(in-package :fountain-frontend)
