(in-package :cl-user)
(require 'asdf)

(asdf:defsystem :eightbol
  :author "Bruce-Robert Pocock"
  :version "0.8"
  :description "EIGHTBOL compiler for 8-bit and 16-bit systems (6502, Z80, ARM, etc.)"
  :maintainer "Bruce-Robert Pocock"
  :mailto "brpocock+skyline@star-hope.org"
  :licence "MIT"
  :long-name "Eight-Bit-Oriented Language"
  
  :depends-on (;; broken into lines for easier sorting
               :alexandria
               :cl-change-case
               :cl-ppcre
               :local-time
               :serapeum
               :split-sequence
               :uiop
               :yacc
               )
  :encoding :utf-8
  :components
  ((:module "src"
            
            :components ((:file "package")
                         (:file "conditions" :depends-on ("package"))
                         (:file "lexer" :depends-on ("package" "conditions"))
                         (:file "ast" :depends-on ("package"))
                         (:file "ast-optimize" :depends-on ("package" "ast"))
                         (:file "ast-validate"
                          :depends-on ("package" "ast" "ast-optimize" "conditions"))
                         (:file "parser" :depends-on ("package" "conditions" "lexer" "ast"))
                         (:file "backend" :depends-on ("package" "conditions" "ast"))
                         (:file "copybook-load" :depends-on ("package" "conditions" "backend"))
                         (:module "backend-6502"
                          :components ((:file "backend-6502-part1")
                                       (:file "backend-6502-part2")
                                       (:file "backend-6502-part3")
                                       (:file "backend-6502-part4")
                                       (:file "backend-6502-part5")
                                       (:file "backend-6502-part6")))
                         (:module "backend-rp2a03"
                          :components ((:file "backend-rp2a03")))
                         (:module "backend-65c02"
                          :components ((:file "backend-65c02")))
                         (:module "backend-65c816"
                          :components ((:file "backend-65c816")))
                         (:module "backend-huc6280"
                          :components ((:file "backend-huc6280")))
                         (:module "backend-cp1610"
                          :components ((:file "backend-cp1610")))
                         (:module "backend-z80"
                          :components ((:file "backend-z80")))
                         (:module "backend-m68k"
                          :components ((:file "backend-m68k")))
                         (:module "backend-sm83"
                          :components ((:file "backend-sm83")))
                         (:module "backend-m6800"
                          :components ((:file "backend-m6800")))
                         (:module "backend-arm7"
                          :components ((:file "backend-arm7")))
                         (:module "backend-i286"
                          :components ((:file "backend-i286")))
                         (:module "backend-f8"
                          :components ((:file "backend-f8")))
                         (:file "basic-transpile")
                         (:file "eightbol-compile"
                          :depends-on ("package"
                                       "ast"
                                       "ast-optimize"
                                       "ast-validate"
                                       "backend"
                                       "backend-6502"
                                       "backend-65c02"
                                       "backend-65c816"
                                       "backend-arm7"
                                       "backend-cp1610"
                                       "backend-f8"
                                       "backend-huc6280"
                                       "backend-i286"
                                       "backend-m6800"
                                       "backend-m68k"
                                       "backend-rp2a03"
                                       "backend-sm83"
                                       "backend-z80"
                                       "copybook-load"
                                       "parser"
                                       ))
                         (:file "basic-shell"
                          :depends-on ("package" "basic-transpile" "eightbol-compile"))
                         (:file "main"
                          :depends-on ("package" "basic-shell" "eightbol-compile")))))
  :in-order-to ((test-op :eightbol-test)))
