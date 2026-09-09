;;; tests/optimizers/optimizer-dead-code-elimination-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/optimizer-dead-code-elimination
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/optimizer-dead-code-elimination)

(fiveam:def-suite :optimizer-dead-code-elimination
  :description "Dead Code Elimination optimizer tests"
  :in :ast-optimize)
