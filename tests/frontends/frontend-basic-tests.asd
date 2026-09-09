(require 'asdf)

(asdf:defsystem "frontend-basic-tests"
  :description "BASIC frontend parser and lexer tests"
  :author "EIGHTBOL Development"
  :version "0.1.0"
  :depends-on (:eightbol :fiveam)
  :components ((:module "frontend-basic"
                :pathname "."
                :components ((:file "package")
                             (:file "lexer-tests" :depends-on ("package"))
                             (:file "parser-tests" :depends-on ("package"))
                             (:file "numeric-types-tests" :depends-on ("package"))
                             (:file "variable-names-tests" :depends-on ("package"))
                             (:file "functions-tests" :depends-on ("package"))
                             (:file "integration-tests" :depends-on ("package")))))
  :in-order-to ((test-op (test-op :eightbol-test))))
