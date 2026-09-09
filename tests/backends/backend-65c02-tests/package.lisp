;;; tests/backends/backend-65c02-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-65c02
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-65c02)

(fiveam:def-suite :backend-65c02
  :description "65C02 backend tests")
