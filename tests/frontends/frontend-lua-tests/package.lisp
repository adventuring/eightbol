;;; tests/frontends/frontend-lua-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/frontend-lua
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/frontend-lua)

(fiveam:def-suite :frontend-lua
  :description "LUA frontend tests"
  :in :eightbol)
