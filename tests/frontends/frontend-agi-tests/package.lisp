;;; tests/frontends/frontend-agi-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-agi
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-agi)

(fiveam:def-suite :frontend-agi
  :description "AGI frontend tests"
  :in :eightbol)
