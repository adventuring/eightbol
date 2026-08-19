;; src/package.lisp — EIGHTBOL core public API
;;; Copyright © 2026 Interworldly Adventuring, LLC
(cl:in-package :cl-user)

(defpackage #:eightbol
  (:use :cl :cl-change-case :yacc :alexandria :serapeum/bundle)
  (:shadow #:describe)
  (:import-from :unix-opts #:define-opts #:get-opts #:option #:exit)
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
    
    ;; BASIC-specific API (exposed for external BASIC drivers) -- Derived from Dartmouth BASIC/QBASIC syntax, maps line numbers to labels, GOSUB/RETURN→PERFORM/GOBACK; no graphics/sound commands
    :basic-lex
    :parse-basic
    :basic-make-parser
    
                                        ;(frontend-pascal:package) -- Derived from Turbo Pascal 7.0 syntax; exception handling→:perform; no WITH clauses; records→:dd
    :pascal-lex
    :pascal-parse
    :pascal-tokenize
    
                                        ;(frontend-lingo:package) -- Based on Lingo for Director MX; 8-bit color & sprite constraints; no network/XML support; cast members→:dd
    :lingo-lex
    :lingo-parse
    :lingo-tokenize
    :lingo-make-parser
    
                                        ;(frontend-objective:package) -- Simplified subset with FORTRAN-style syntax; no Cocoa/Foundation frameworks
    :objective-lex
    :objective-parse
    :objective-make-parser
    
                                        ;(frontend-fortran:package) -- FORTRAN 77/90 subset with explicit numeric typing; no OOP, no modern modules
    :fortran-lex
    :fortran-lex-line
    :fortran-lex-source
    :parse-fortran
    :load-fortran-copybook
    
                                         ;(frontend-lua:package) -- Lua 5.3 grammar with 16-bit memory limits; memory mgmt→manual :move ops
     :tokenize-lua
     
     ;; AGI-specific API
     :compile-agi-from-path
     :make-agi-parser
     
     ;; COBOL frontend
     :cobol-lex
     :cobol-lex-line
     
     ;; SmallTalk frontend
     :smalltalk-make-parser
     
      ;; Muddle frontend
      :muddle-lex-line
      :muddle-lex-source
      :muddle-parse-number
      :muddle-normalize-identifier
      :muddle-parser
     
     ;; SCI frontend
     :sci-lex-line
     :sci-lex-source
     :sci-parse-program
     :sci-parser
     :sci-parse-number
     :sci-normalize-identifier
     
     ;; SCUMM frontend
     :scumm-lex-line
     :scumm-lex-source
     :scumm-parse-program
     
     ;; ZIL frontend
     :zil-lex-line
     :zil-lex-source
     
      ;; Burgermistress frontend
      :burgermistress-lex-line
      :burgermistress-lex-source

       ;; GOAL frontend
       :goal-lex-line
       :goal-lex-source
       :goal-parse-source
       :goal-normalize-identifier
       :parse-goal-number
       
       ;; Forth frontend
       :forth-normalize-identifier
       :forth-valid-identifier-p
       :forth-lex-number
       :forth-lex-line
       :forth-tokenize-source
       :forth-token-type
       :forth-get-keyword-token
       :forth-get-dialogue-token
       :forth-parse-tokens
       :forth-parse-token
       :forth-parse-line
       :forth-compile-source
       :forth-compile-file
       :forth-parse-context
       :forth-parse-context-tokens-per-line
       :forth-parse-context-current-line-idx
       :forth-parse-context-current-token-idx
       :forth-parse-context-word-definitions
       :forth-parse-context-stack-depth
       ;; AST node constructors
       :make-forth-push-literal
       :make-forth-push-identifier
       :make-forth-stack-op
       :make-forth-arithmetic
       :make-forth-comparison
       :make-forth-conditional
       :make-forth-loop
       :make-forth-variable-def
       :make-forth-fetch
       :make-forth-store
       :make-forth-print-value
       :make-forth-print-string
       :make-forth-print-char
       :make-forth-print-cr
       :make-forth-print-space
       :make-forth-input-key
       :make-forth-input-line
       :make-forth-dialogue
       :make-forth-word-def
       :make-forth-constant-def
       :make-forth-program
       
       ;; End-to-end compilation helpers (externally invoked)
       :compile-basic-from-path
       ;; Conditions
       :source-error
       :copybook-not-found
       ;; Include path support
       :include-path))
