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
     
     ;; SCI frontend
     :sci-lex-line
     :sci-lex-source
     :sci-parse-program
     
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
     
     ;; End-to-end compilation helpers (externally invoked)
      :compile-basic-from-path
      ;; Conditions
      :source-error
      :copybook-not-found
      ;; Include path support
      :include-path))
