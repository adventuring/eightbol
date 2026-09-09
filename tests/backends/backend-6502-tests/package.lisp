;;; tests/backends/backend-6502-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-6502
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-6502)

(fiveam:def-suite :backend-6502
  :description "6502 backend tests")
