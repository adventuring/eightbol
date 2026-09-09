;;; tests/frontends/frontend-goal-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-goal
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-goal)

(fiveam:def-suite :frontend-goal
  :description "GOAL frontend tests"
  :in :eightbol)
