;;; tests/backends/backend-jvm-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-jvm
  (:use :fiveam :common-lisp :eightbol))

(in-package :eightbol/test/backend-jvm)

(fiveam:def-suite :backend-jvm
  :description "JVM backend code generation tests")
