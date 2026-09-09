;;; tests/frontends/frontend-smalltalk-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-smalltalk
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-smalltalk)

(fiveam:def-suite :frontend-smalltalk
  :description "SMALLTALK frontend tests"
  :in :eightbol)
