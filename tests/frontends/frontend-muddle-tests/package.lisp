;;; tests/frontends/frontend-muddle-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-muddle
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-muddle)

(fiveam:def-suite :frontend-muddle
  :description "MUDDLE frontend tests"
  :in :eightbol)
