;;; tests/backends/backend-arm7-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-arm7
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-arm7)

(fiveam:def-suite :backend-arm7
  :description "ARM7 backend tests")
