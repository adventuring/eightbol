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
               :unix-opts
               :yacc
               )
  :encoding :utf-8
  :components
  ((:module "src"
            
            :components ((:file "package")
                         (:file "conditions" :depends-on ("package"))
                         
                         ;; Core compiler infrastructure
                         (:file "ast" :depends-on ("package"))
                         (:file "ast-optimize" :depends-on ("package" "ast"))
                         (:file "ast-validate"
                          :depends-on ("package" "ast" "ast-optimize" "conditions"))
                         (:file "backend" :depends-on ("package" "conditions" "ast"))
                         (:file "grammar-build" :depends-on ("package" "ast"))
                         (:file "cobol-copybook"
                          :depends-on ("package" "conditions" "backend"))
                         (:file "eightbol-compile"
                          :depends-on ("package"
                                       "ast"
                                       "ast-optimize"
                                       "ast-validate"
                                       "backend"
                                       "cobol-copybook"
                                       "frontend-cobol"))

                          ;; COBOL frontend (first, as most mature)
                          (:module "frontend-cobol"
                           :depends-on ("package" "conditions" "ast")
                           :components ((:file "cobol-lexer")
                                        (:file "cobol-parser"
                                         :depends-on ("cobol-lexer"))))

                          ;; AGI frontend
                          (:module "frontend-agi"
                           :depends-on ("package" "conditions" "ast" "eightbol-compile")
                           :components ((:file "agi-lexer")
                                        (:file "agi-parser"
                                         :depends-on ("agi-lexer"))
                                        (:file "agi-transpile"
                                         :depends-on ("agi-parser"))
                                        (:file "make-parser"
                                         :depends-on ("agi-parser"))))

                         ;; BASIC frontend
                         (:module "frontend-basic"
                          :depends-on ("package" "ast" "grammar-build" "eightbol-compile")
                          :components ((:file "basic-lexer")
                                       (:file "basic-parser"
                                        :depends-on ("basic-lexer"))
                                       (:file "basic-shell"
                                        :depends-on ("basic-parser"))
                                       (:file "basic-transpile"
                                        :depends-on ("basic-parser"))))

                         ;; FORTRAN frontend
                         (:module "frontend-fortran"
                          :depends-on ("package" "ast" "grammar-build")
                          :components ((:file "fortran-lexer")
                                       (:file "fortran-parser"
                                        :depends-on ("fortran-lexer"))))

                         ;; Pascal frontend
                         (:module "frontend-pascal"
                          :depends-on ("package" "ast" "grammar-build")
                          :components ((:file "pascal-lexer")
                                       (:file "pascal-parser"
                                        :depends-on ("pascal-lexer"))))
                         
                          ;; Lingo frontend
                          (:module "frontend-lingo"
                           :depends-on ("package" "conditions" "ast" "grammar-build")
                           :components ((:file "lingo-lexer")
                                        (:file "lingo-parser"
                                         :depends-on ("lingo-lexer"))
                                        (:file "lingo-make-parser"
                                         :depends-on ("lingo-parser"))))

                          ;; Burgermistress frontend
                          (:module "frontend-burgermistress"
                           :depends-on ("package" "ast" "grammar-build")
                           :components ((:file "burger-lexer")
                                        (:file "burger-parser"
                                         :depends-on ("burger-lexer"))))

                          ;; Muddle frontend
                          (:module "frontend-muddle"
                           :depends-on ("package" "ast" "grammar-build")
                           :components ((:file "muddle-lexer")
                                        (:file "muddle-parser"
                                         :depends-on ("muddle-lexer"))))

                          ;; SCI frontend
                           (:module "frontend-sci"
                            :depends-on ("package" "ast" "grammar-build" "eightbol-compile")
                            :components ((:file "sci-lexer")
                                         (:file "sci-parser"
                                          :depends-on ("sci-lexer"))
                                         (:file "sci-transpile"
                                          :depends-on ("sci-parser"))))

                           ;; SCUMM frontend
                           (:module "frontend-scumm"
                            :depends-on ("package" "ast" "grammar-build" "eightbol-compile")
                            :components ((:file "scumm-lexer")
                                         (:file "scumm-parser"
                                          :depends-on ("scumm-lexer"))
                                         (:file "scumm-transpile"
                                          :depends-on ("scumm-parser"))))

                          ;; ZIL frontend
                          (:module "frontend-zil"
                           :depends-on ("package" "ast" "grammar-build")
                           :components ((:file "zil-lexer")
                                        (:file "zil-parser"
                                         :depends-on ("zil-lexer"))))

                         ;; SmallTalk frontend
                         (:module "frontend-smalltalk"
                          :depends-on ("package" "conditions" "ast" "grammar-build")
                          :components ((:file "smalltalk-lexer")
                                       (:file "smalltalk-parser"
                                        :depends-on ("smalltalk-lexer"))
                                       (:file "smalltalk-make-parser"
                                        :depends-on ("smalltalk-parser"))))
                         
                         ;; Lua frontend
                         (:module "frontend-lua"
                          :depends-on ("package" "ast" "grammar-build")
                          :components ((:file "lua-lexer")
                                       (:file "lua-parser"
                                        :depends-on ("lua-lexer"))))

                           ;; Objective-C frontend
                           (:module "frontend-objective"
                            :depends-on ("package" "ast" "grammar-build")
                            :components ((:file "objective-lexer")
                                         (:file "objective-parser"
                                          :depends-on ("objective-lexer"))))

                           ;; Forth frontend
                            (:module "frontend-forth"
                             :depends-on ("package" "ast" "eightbol-compile")
                             :components ((:file "forth-lexer")
                                          (:file "forth-parser"
                                           :depends-on ("forth-lexer"))
                                          (:file "forth-transpile"
                                           :depends-on ("forth-parser"))
                                          (:file "forth-tests"
                                           :depends-on ("forth-lexer" "forth-parser"))))

                           ;; Backend modules
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
                          :depends-on ("backend-z80")
                          :components ((:file "backend-m6800")))
                         (:module "backend-arm7"
                          :components ((:file "backend-arm7")))
                         (:module "backend-i286"
                          :components ((:file "backend-i286")))
                         (:module "backend-f8"
                          :components ((:file "backend-f8")))

                         ;; Main entry point
                         (:file "main"
                          :depends-on ("package" "frontend-agi" "frontend-basic" "frontend-cobol"
                                                 "frontend-burgermistress" "frontend-forth" "frontend-fortran"
                                                 "frontend-lingo" "frontend-lua"
                                                 "frontend-muddle" "frontend-objective"
                                                 "frontend-pascal" "frontend-sci"
                                                 "frontend-scumm" "frontend-smalltalk"
                                                 "frontend-zil" "eightbol-compile")))))
  :in-order-to ((test-op (test-op :eightbol-test))))
