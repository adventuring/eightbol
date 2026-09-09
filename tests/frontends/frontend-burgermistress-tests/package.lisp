;;; tests/frontends/frontend-burgermistress-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-burgermistress
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-burgermistress)

(fiveam:def-suite :frontend-burgermistress
  :description "BURGERMISTRESS frontend tests"
  :in :eightbol)
