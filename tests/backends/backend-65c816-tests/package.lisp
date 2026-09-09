;;; tests/backends/backend-65c816-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-65c816
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-65c816)

(fiveam:def-suite :backend-65c816
  :description "65C816 backend tests")
