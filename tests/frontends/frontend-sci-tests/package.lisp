;;; tests/frontends/frontend-sci-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-sci
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-sci)

(fiveam:def-suite :frontend-sci
  :description "SCI frontend tests"
  :in :eightbol)
