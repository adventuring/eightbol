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
   
   ;; End-to-end compilation helpers (externally invoked)
   :compile-basic-from-path
   :compile-lingo-from-path)
  
  (:shadowing-import-from :serapeum #:partition #:comment #:occurs)
  (:import-from :split-sequence :split-sequence))
