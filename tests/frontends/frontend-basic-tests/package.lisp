;;; tests/frontends/frontend-basic-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-basic
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-basic)

(fiveam:def-suite :frontend-basic
  :description "BASIC frontend tests"
  :in :eightbol)
