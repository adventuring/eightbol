(in-package :cl-user)
(require 'asdf)

(asdf:defsystem :eightbol
  :description "EIGHTBOL compiler for 8-bit systems"
  :author "Bruce-Robert Pocock"
  :version "0.6"
  :maintainer "Bruce-Robert Pocock"
  :mailto "brpocock+skyline@star-hope.org"
  :licence "MIT"
  :long-name "Eight-Bit-Oriented Language"

  :depends-on (;; broken into lines for easier sorting
               :alexandria
               :uiop
               :cl-change-case
               :cl-ppcre
               :local-time
               :serapeum
               :split-sequence
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
                 (:file "backend-6502"
                  :depends-on ("package" "ast" "backend"))
                 (:file "backend-RP2A03"
                  :depends-on ("package" "ast" "backend" "copybook-load" "backend-6502"))
                 (:file "backend-65c02"
                  :depends-on ("package" "ast" "backend" "backend-6502"))
                 (:file "backend-65c816"
                  :depends-on ("package" "ast" "backend" "backend-6502"))
                 (:file "backend-HuC6280"
                  :depends-on ("package" "ast" "backend" "backend-6502"))
                 (:file "backend-cp1610"
                  :depends-on ("package" "ast" "backend"))
                 (:file "backend-Z80"
                  :depends-on ("package" "ast" "backend" "copybook-load"))
                 (:file "backend-m68k"
                  :depends-on ("package" "ast" "backend" "copybook-load"))
                 (:file "backend-SM83"
                  :depends-on ("package" "ast" "backend" "copybook-load"))
                 (:file "backend-m6800"
                  :depends-on ("package" "ast" "backend" "copybook-load"))
                 (:file "backend-ARM7"
                  :depends-on ("package" "ast" "backend" "copybook-load"))
                 (:file "backend-i286"
                  :depends-on ("package" "ast" "backend" "copybook-load"))
                 (:file "backend-F8"
                  :depends-on ("package" "ast" "backend" "copybook-load"))
                 (:file "basic-transpile" :depends-on ("package"))
                 (:file "eightbol-compile"
                  :depends-on ("package" "parser" "ast" "ast-optimize" "ast-validate"
                                         "backend" "copybook-load" "backend-6502" "backend-RP2A03"
                                         "backend-65c02" "backend-65c816"
                                         "backend-HuC6280"
                                         "backend-cp1610" "backend-Z80"
                                         "backend-m68k" "backend-SM83" "backend-m6800"
                                         "backend-ARM7"
                                         "backend-i286"
                                         "backend-F8"))
                 (:file "basic-shell"
                  :depends-on ("package" "basic-transpile" "eightbol-compile"))
                 (:file "main"
                  :depends-on ("package" "eightbol-compile" "basic-shell")))))
  :in-order-to ((test-op (test-op "eightbol-test"))))
