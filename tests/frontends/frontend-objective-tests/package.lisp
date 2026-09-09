;;; tests/frontends/frontend-objective-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-objective
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-objective)

(fiveam:def-suite :frontend-objective
  :description "OBJECTIVE frontend tests"
  :in :eightbol)
