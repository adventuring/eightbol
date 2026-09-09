;;; tests/backends/backend-sm83-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-sm83
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-sm83)

(fiveam:def-suite :backend-sm83
  :description "SM83 backend tests")
