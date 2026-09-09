;;; tests/frontends/frontend-fortran-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-fortran
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-fortran)

(fiveam:def-suite :frontend-fortran
  :description "FORTRAN frontend tests"
  :in :eightbol)
