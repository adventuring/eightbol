;;; tests/backends/backend-z80-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-z80
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-z80)

(fiveam:def-suite :backend-z80
  :description "Z80 backend tests")
