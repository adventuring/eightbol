;;; tests/optimizers/optimizer-constant-folding-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/optimizer-constant-folding
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/optimizer-constant-folding)

(fiveam:def-suite :optimizer-constant-folding
  :description "Constant Folding optimizer tests"
  :in :ast-optimize)
