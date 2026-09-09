(require 'asdf)

(asdf:defsystem "optimizer-loop-unrolling-tests"
  :description "loop unrolling optimizer pass tests"
  :author "EIGHTBOL Development"
  :version "0.1.0"
  :depends-on (:eightbol :fiveam)
  :components ((:module "optimizer-loop-unrolling"
                :pathname "."
                :components ((:file "package")
                             (:file "transformation-tests" :depends-on ("package"))
                             (:file "edge-cases-tests" :depends-on ("package"))
                             (:file "regression-tests" :depends-on ("package")))))
  :in-order-to ((test-op (test-op :eightbol-test))))
