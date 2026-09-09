;;; tests/frontends/frontend-scumm-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-scumm
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-scumm)

(fiveam:def-suite :frontend-scumm
  :description "SCUMM frontend tests"
  :in :eightbol)
