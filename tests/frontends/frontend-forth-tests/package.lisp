;;; tests/frontends/frontend-forth-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-forth
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-forth)

(fiveam:def-suite :frontend-forth
  :description "FORTH frontend tests"
  :in :eightbol)
