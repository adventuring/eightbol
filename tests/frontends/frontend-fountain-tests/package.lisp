;;; tests/frontends/frontend-fountain-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-fountain
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-fountain)

(fiveam:def-suite :frontend-fountain
  :description "FOUNTAIN frontend tests"
  :in :eightbol)
