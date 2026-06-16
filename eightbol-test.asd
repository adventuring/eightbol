(in-package :cl-user)
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
                              :depends-on ("eightbol-tests")))))
  :perform (asdf:test-op (o c)
                         (dolist (suite '(
                                          :ast-optimize
                                          :backend-6502-classification
                                          :backend-cp1610
                                          :backend-matrix
                                          :backend-operand-kinds
                                          :backend-output
                                          :backend-z80
                                          :dartmouth-basic-parity
                                          :eightbol
                                          :eightbol-cp1610-6502-parity
                                          :parser-structure
                                          :pic-1-bit
                                          :s-decimal
                                          :service-bank-lut
                                          )
                                        (funcall (intern "RUN!" :fiveam) suite)))))
