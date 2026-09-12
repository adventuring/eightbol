(require 'asdf)

(asdf:defsystem "backend-jvm-tests"
  :description "JVM backend code generation tests"
  :author "EIGHTBOL Development"
  :version "0.1.0"
  :depends-on (:eightbol :fiveam)
  :components ((:module "backend-jvm"
                :pathname "backend-jvm-tests"
                :components ((:file "package")
                             (:file "move-node-tests" :depends-on ("package"))
                             (:file "arithmetic-node-tests" :depends-on ("package"))
                             (:file "control-flow-tests" :depends-on ("package"))
                             (:file "call-invoke-tests" :depends-on ("package"))
                             (:file "string-operations-tests" :depends-on ("package"))
                             (:file "special-nodes-tests" :depends-on ("package"))
                             (:file "integration-tests" :depends-on ("package")))))
  :in-order-to ((test-op (test-op :eightbol-test))))
