;;; tests/backends/backend-rp2a03-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-rp2a03
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-rp2a03)

(fiveam:def-suite :backend-rp2a03
  :description "RP2A03 backend tests")
