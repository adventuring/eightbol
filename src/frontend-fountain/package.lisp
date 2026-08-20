;; src/frontend-fountain/package.lisp — Package definition for Fountain lexer/parser
;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :fountain-frontend
  (:use :cl :split-sequence)
  (:export
   ;; Lexer functions
   :lex-fountain-source
   :lex-fountain-file
   ;; Token manipulation
   :make-token
   :token-type
   :token-value
   :token-line
   :token-column
   ;; Utilities
   :to-pascal-case
   :parse-number-literal
   :valid-identifier-p
   ;; Parser functions
   :parse-fountain-source
   :parse-fountain-file
   :parse-fountain-tokens
   ;; AST nodes
   :make-scene-node
   :make-dialogue-node
   :make-action-node
   :make-transition-node
   :make-character-entry-node
   :make-variable-assignment-node
   :make-conditional-node))
