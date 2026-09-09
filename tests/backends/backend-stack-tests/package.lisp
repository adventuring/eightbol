;;; tests/backends/backend-stack-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-stack
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-stack)

(fiveam:def-suite :backend-stack
  :description "STACK backend tests")
