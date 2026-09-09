;;; tests/backends/backend-i286-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-i286
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-i286)

(fiveam:def-suite :backend-i286
  :description "I286 backend tests")
