;; frontend-garry-tests.asd — Test suite for Garry frontend
;; Copyright © 2026 Interworldly Adventuring, LLC

(require 'asdf)

(asdf:defsystem "frontend-garry-tests"
  :description "Tests for EIGHTBOL Garry frontend"
  :author "Bruce-Robert Pocock"
  :version "0.3.0"
  :depends-on (:eightbol :fiveam)
  :components ((:file "test-package")
               (:file "garry-lexer-tests")
               (:file "garry-parser-tests")))
  :perform (asdf:test-op (o c)
                         (funcall (intern "RUN!" :fiveam) :frontend-garry)))