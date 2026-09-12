(defpackage :eightbol/test/garry
  (:use :cl :fiveam)
  (:export :run!))

(in-package :eightbol/test/garry)

(in-suite :frontend-garry)

(defmacro run! ()
  "Run all Garry frontend tests."
  '(run! :frontend-garry))