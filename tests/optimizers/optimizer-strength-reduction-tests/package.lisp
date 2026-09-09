;;; tests/optimizers/optimizer-strength-reduction-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/optimizer-strength-reduction
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/optimizer-strength-reduction)

(fiveam:def-suite :optimizer-strength-reduction
  :description "Strength Reduction optimizer tests"
  :in :ast-optimize)
