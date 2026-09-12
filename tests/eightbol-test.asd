(require 'asdf)

(asdf:defsystem "eightbol-test"
  :description "Tests for EIGHTBOL compiler"
  :author "Bruce-Robert Pocock"
  :version "0.3.0"
  :depends-on (:eightbol :fiveam)
  :defsystem-depends-on (:asdf :fiveam)
  :components ((:module "tests"
                :components ((:file "test-package")
                             (:file "eightbol-tests"
                              :depends-on ("test-package"))
                             (:file "repro-bugs"
                              :depends-on ("eightbol-tests"))
                             (:file "pic-1-bit-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "s-decimal-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-matrix-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-operand-kinds-tests"
                              :depends-on ("eightbol-tests"))
                              (:file "ast-optimize-tests"
                               :depends-on ("eightbol-tests"))
                              (:file "variable-erasure-tests"
                               :depends-on ("eightbol-tests"))
                              (:file "backend-cp1610-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-z80-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-output-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "parser-structure-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "service-bank-lut-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "statement-parity-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "basic-parity-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-6502-classification-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-f8-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-sm83-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "expression-constant-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "numeric-precision-all-backends"
                              :depends-on ("eightbol-tests"))
                             (:file "numeric-precision-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "lua-parser-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "frontend-lexer-parser-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-65c02-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-65c816-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-huc6280-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-rp2a03-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-arm7-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-i286-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-m6800-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-m68k-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "backend-stack-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "frontend-comprehensive-tests"
                              :depends-on ("eightbol-tests"))
                             (:file "optimizer-comprehensive-tests"
                              :depends-on ("eightbol-tests"))
                              (:file "backend-comprehensive-tests"
                               :depends-on ("eightbol-tests"))
                              (:file "calling-convention-and-complexity-tests"
                               :depends-on ("eightbol-tests"))
                              (:file "declarations-tests"
                               :depends-on ("eightbol-tests"))
                              (:file "backend-comprehensive-ast-tests"
                                :depends-on ("eightbol-tests"))
                              (:file "operator-coverage-tests"
                               :depends-on ("eightbol-tests"))
                              
                              ;; Frontend tests (new structure)
                             (:module "frontends/frontend-cobol-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-basic-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-agi-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-zil-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-forth-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-fountain-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-lua-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-pascal-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-lingo-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-smalltalk-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-fortran-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-muddle-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-sci-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-scumm-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-burgermistress-tests" :depends-on ("test-package"))
                             (:module "frontends/frontend-goal-tests" :depends-on ("test-package"))
(:module "frontends/frontend-objective-tests" :depends-on ("test-package"))
                              (:module "frontends/frontend-garry-tests" :depends-on ("test-package"))
                              
                              ;; Backend tests (new structure)
                             (:module "backends/backend-6502-tests" :depends-on ("test-package"))
                             (:module "backends/backend-65c02-tests" :depends-on ("test-package"))
                             (:module "backends/backend-65c816-tests" :depends-on ("test-package"))
                             (:module "backends/backend-huc6280-tests" :depends-on ("test-package"))
                             (:module "backends/backend-rp2a03-tests" :depends-on ("test-package"))
                             (:module "backends/backend-cp1610-tests" :depends-on ("test-package"))
                             (:module "backends/backend-z80-tests" :depends-on ("test-package"))
                             (:module "backends/backend-sm83-tests" :depends-on ("test-package"))
                             (:module "backends/backend-m68k-tests" :depends-on ("test-package"))
                             (:module "backends/backend-i286-tests" :depends-on ("test-package"))
                             (:module "backends/backend-arm7-tests" :depends-on ("test-package"))
                             (:module "backends/backend-f8-tests" :depends-on ("test-package"))
                                                           (:module "backends/backend-stack-tests" :depends-on ("test-package"))
                              (:module "backends/backend-jvm-tests" :depends-on ("test-package"))
                              (:module "backends/backend-wasm-tests" :depends-on ("test-package"))
                              (:module "backends/backend-zork-tests" :depends-on ("test-package"))
                             
                             ;; Optimizer tests (new structure)
                             (:module "optimizers/optimizer-constant-folding-tests" :depends-on ("test-package"))
                             (:module "optimizers/optimizer-strength-reduction-tests" :depends-on ("test-package"))
                             (:module "optimizers/optimizer-dead-code-elimination-tests" :depends-on ("test-package"))
                             (:module "optimizers/optimizer-common-subexpression-elimination-tests" :depends-on ("test-package"))
                             (:module "optimizers/optimizer-loop-unrolling-tests" :depends-on ("test-package"))
                             (:module "optimizers/optimizer-register-allocation-tests" :depends-on ("test-package"))
                             
                             ;; Other tests (new structure)
                             (:module "other"
                              :components ((:file "command-line-tests")
                                           (:file "basic-shell-tests")
                                           (:file "cobol-copybook-tests"))
                              :depends-on ())
                             )))
  :perform (asdf:test-op (o c)
                               (let ((suites '(:eightbol :backend-matrix :ast-optimize
                                               :variable-erasure
                                               :backend-output :parser-structure
                                              :compile-regression :copybook-generation
                                              :service-bank-lut :backend-cp1610
                                              :backend-z80 :backend-operand-kinds
                                              :backend-f8 :backend-sm83
                                              :backend-6502-classification
                                              :backend-65c02 :backend-65c816
                                              :backend-huc6280 :backend-rp2a03
                                              :backend-arm7 :backend-i286
                                              :backend-m6800 :backend-m68k
                                              :backend-stack
                                              :expression-constant :numeric-precision
                                              :numeric-precision-all-backends
                                              :pic-1-bit :s-decimal
                                              :eightbol-cp1610-6502-parity
                                              :dartmouth-basic-parity
                                              :frontend-lexers :frontend-parsers
                                              :lexer-consistency :parser-error-handling
                                              :ast-node-construction :lexer-performance
                                              :cross-frontend-consistency :keyword-coverage
                                              :parser-integration
                                              :frontend-ast-generation
                                              :frontend-comprehensive
                                              :optimizer-comprehensive
                                              :backend-comprehensive
                                               :backend-ast-comprehensive
                                                :declarations-system
                                                :operator-coverage
                                                
                                                ;; New frontend test suites
                                              :frontend-cobol
                                              :frontend-basic
                                              :frontend-agi
                                              :frontend-zil
                                              :frontend-forth
                                              :frontend-fountain
                                              :frontend-lua
                                              :frontend-pascal
                                              :frontend-lingo
                                              :frontend-smalltalk
                                              :frontend-fortran
                                              :frontend-muddle
                                              :frontend-sci
                                              :frontend-scumm
                                               :frontend-burgermistress
                                               :frontend-goal
                                               :frontend-objective
                                               :frontend-garry
                                               
                                               ;; New backend test suites
                                              :backend-6502
                                              :backend-65c02
                                              :backend-65c816
                                              :backend-huc6280
                                              :backend-rp2a03
                                              :backend-cp1610
                                              :backend-z80
                                              :backend-sm83
                                              :backend-m68k
                                              :backend-i286
                                              :backend-arm7
:backend-f8
                                               :backend-jvm
                                               :backend-wasm
                                               :backend-zork
                                               :backend-m6800
                                               :backend-stack
                                              
                                              ;; New optimizer test suites
                                              :optimizer-constant-folding
                                              :optimizer-strength-reduction
                                              :optimizer-dead-code-elimination
                                              :optimizer-common-subexpression-elimination
                                              :optimizer-loop-unrolling
                                              :optimizer-register-allocation
                                              
                                              ;; New other test suites
                                              :command-line-parsing
                                              :basic-shell
                                              :cobol-copybooks
                                              )))
                            (dolist (suite suites)
                              (format t "~&;; Running suite ~s...~%" suite)
                               (funcall (intern "RUN!" :fiveam) suite)))))

