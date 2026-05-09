;;; tests/test-package.lisp — test package and FiveAM suite (loaded only by :eightbol-test)
(in-package :cl-user)

(defpackage :eightbol/test
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test)

(fiveam:def-suite :eightbol
  :description "Tests for the EIGHTBOL compiler (general)")
