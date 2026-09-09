;;; tests/optimizers/optimizer-common-subexpression-elimination-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/optimizer-common-subexpression-elimination
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/optimizer-common-subexpression-elimination)

(fiveam:def-suite :optimizer-common-subexpression-elimination
  :description "Common Subexpression Elimination optimizer tests"
  :in :ast-optimize)
