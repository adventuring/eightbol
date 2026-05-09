(in-package :cl-user)
(require 'asdf)

(asdf:defsystem "eightbol-test"
  :description "Tests for EIGHTBOL compiler"
  :author "Bruce-Robert Pocock"
  :version "0.3.0"
  :depends-on (:eightbol :fiveam :skyline-tool)
  :defsystem-depends-on (:asdf :fiveam)
  :components ((:module "tests"
                :components ((:file "test-package")
                            (:file "eightbol-tests"
                             :depends-on ("test-package"))
                            (:file "backend-matrix-tests"
                             :depends-on ("eightbol-tests"))
                            (:file "backend-operand-kinds-tests"
                             :depends-on ("eightbol-tests"))
                            (:file "ast-optimize-tests"
                             :depends-on ("eightbol-tests"))
                            (:file "copybook-generation-tests"
                             :depends-on ("eightbol-tests"))
                            (:file "compile-regression-tests"
                             :depends-on ("eightbol-tests"))
                            (:file "phantasia-classes-compile-tests"
                             :depends-on ("eightbol-tests"))
                            (:file "phantasia-method-port-tests"
                             :depends-on ("phantasia-classes-compile-tests"))
                            (:file "backend-output-tests"
                             :depends-on ("eightbol-tests"))
                            (:file "parser-structure-tests"
                             :depends-on ("eightbol-tests"))
                            (:file "service-bank-lut-tests"
                             :depends-on ("eightbol-tests")))))
  :perform (asdf:test-op (o c)
                         (dolist (suite '(:eightbol :backend-matrix :backend-operand-kinds :backend-6502-classification :ast-optimize :copybook-generation :compile-regression
                                          :phantasia-classes-compile :phantasia-method-port :backend-output
                                          :parser-structure :service-bank-lut))
                           (funcall (intern "RUN!" :fiveam) suite))))
