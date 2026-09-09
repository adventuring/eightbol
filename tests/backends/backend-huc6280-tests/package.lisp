;;; tests/backends/backend-huc6280-tests/package.lisp
(in-package :cl-user)

(defpackage :eightbol/test/backend-huc6280
  (:use :fiveam :common-lisp :cl-change-case :eightbol))

(in-package :eightbol/test/backend-huc6280)

(fiveam:def-suite :backend-huc6280
  :description "HUC6280 backend tests")
