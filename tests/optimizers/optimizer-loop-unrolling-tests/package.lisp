;;; tests/optimizers/optimizer-loop-unrolling-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/optimizer-loop-unrolling
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/optimizer-loop-unrolling)

(fiveam:def-suite :optimizer-loop-unrolling
  :description "Loop Unrolling optimizer tests"
  :in :ast-optimize)
