;;; tests/backends/backend-cp1610-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-cp1610
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-cp1610)

(fiveam:def-suite :backend-cp1610
  :description "CP1610 backend tests")
