;;; tests/frontends/frontend-pascal-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-pascal
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-pascal)

(fiveam:def-suite :frontend-pascal
  :description "PASCAL frontend tests"
  :in :eightbol)
