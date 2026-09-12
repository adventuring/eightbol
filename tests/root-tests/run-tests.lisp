#!/usr/bin/sbcl --script
;; run-tests.lisp - Run EIGHTBOL test suite and exit with proper status

(asdf:load-system :eightbol)

(format t "~&Running EIGHTBOL test suite...~%~%")

;; Run all tests and capture results
(let* ((result (asdf:test-system :eightbol))
       (exitcode (if (and result (not (listp result))) 0 1)))
  (format t "~&~%Test exit code: ~d~%" exitcode)
  (sb-ext:quit exitcode))
