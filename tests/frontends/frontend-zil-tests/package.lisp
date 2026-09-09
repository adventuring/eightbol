;;; tests/frontends/frontend-zil-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-zil
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-zil)

(fiveam:def-suite :frontend-zil
  :description "ZIL frontend tests"
  :in :eightbol)
