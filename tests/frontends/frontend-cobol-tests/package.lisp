;;; tests/frontends/frontend-cobol-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-cobol
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-cobol)

(fiveam:def-suite :frontend-cobol
  :description "COBOL frontend tests"
  :in :eightbol)
