;;; tests/backends/backend-zork-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-zork
  (:use :fiveam :common-lisp :eightbol))

(in-package :eightbol/test/backend-zork)

(fiveam:def-suite :backend-zork
  :description "Zork/Z-Machine backend code generation tests")
