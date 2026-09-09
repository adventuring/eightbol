;;; tests/backends/backend-f8-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-f8
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-f8)

(fiveam:def-suite :backend-f8
  :description "F8 backend tests")
