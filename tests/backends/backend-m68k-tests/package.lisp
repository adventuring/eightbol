;;; tests/backends/backend-m68k-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-m68k
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-m68k)

(fiveam:def-suite :backend-m68k
  :description "M68K backend tests")
