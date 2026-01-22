(in-package :cl-user)
(require 'asdf)

(asdf:defsystem :eightbol
  :description "EIGHTBOL compiler for 8-bit systems"
  :author "Bruce-Robert Pocock"
  :version "0.2.0"
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
               :yacc
               )
  :encoding :utf-8
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "conditions"  :depends-on ("package"))
                 (:file "main"        :depends-on ("package" "eightbol-compile"))
                 (:file "lexer"       :depends-on ("package" "conditions"))
                 (:file "ast"         :depends-on ("package"))
                 (:file "ast-optimize" :depends-on ("package" "ast"))
                 (:file "parser"      :depends-on ("package" "conditions" "lexer" "ast"))
                 (:file "backend"     :depends-on ("package" "conditions" "ast"))
                 (:file "backend-6502"
                  :depends-on ("package" "ast" "backend"))
                 (:file "backend-RP2A03"
                  :depends-on ("package" "ast" "backend" "backend-6502"))
                 (:file "backend-65c02"
                  :depends-on ("package" "ast" "backend" "backend-6502"))
                 (:file "backend-65c816"
                  :depends-on ("package" "ast" "backend" "backend-6502"))
                 (:file "backend-HuC6280"
                  :depends-on ("package" "ast" "backend" "backend-6502"))
                 (:file "backend-cp1610"
                  :depends-on ("package" "ast" "backend"))
                 (:file "backend-Z80"
                  :depends-on ("package" "ast" "backend"))
                 (:file "backend-m68k"
                  :depends-on ("package" "ast" "backend"))
                 (:file "backend-SM83"
                  :depends-on ("package" "ast" "backend"))
                 (:file "backend-ARM7"
                  :depends-on ("package" "ast" "backend"))
                 (:file "backend-i286"
                  :depends-on ("package" "ast" "backend"))
                 (:file "eightbol-compile"
                  :depends-on ("package" "parser" "ast" "ast-optimize"
                               "backend" "backend-6502" "backend-RP2A03"
                               "backend-65c02" "backend-65c816"
                               "backend-HuC6280"
                               "backend-cp1610" "backend-Z80"
                               "backend-m68k" "backend-SM83"
                               "backend-ARM7"
                               "backend-i286")))))
  :in-order-to ((test-op (test-op #:eightbol/test))))

(asdf:defsystem #:eightbol/test
  :description "Tests for EIGHTBOL compiler"
  :author "Bruce-Robert Pocock"
  :version "0.2.0"
  :depends-on (:eightbol :fiveam :skyline-tool)
  :defsystem-depends-on (:asdf :fiveam)
  :components ((:module "tests"
                :components ((:file "eightbol-tests")
                            (:file "copybook-generation-tests"
                             :depends-on ("eightbol-tests")))))
  :perform (asdf:test-op (o c)
                         (dolist (suite '(:eightbol :copybook-generation))
                           (funcall (intern "RUN!" :fiveam) suite))))
