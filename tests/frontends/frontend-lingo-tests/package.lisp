;;; tests/frontends/frontend-lingo-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-lingo
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-lingo)

(fiveam:def-suite :frontend-lingo
  :description "LINGO frontend tests"
  :in :eightbol)
