;;; tests/optimizers/optimizer-register-allocation-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/optimizer-register-allocation
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/optimizer-register-allocation)

(fiveam:def-suite :optimizer-register-allocation
  :description "Register Allocation optimizer tests"
  :in :ast-optimize)
