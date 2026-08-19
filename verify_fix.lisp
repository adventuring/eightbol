;; Verification that the eightbol compiler can now compile programs using arithmetic operations
;; without getting undefined function errors for MAKE-EXPRESSION-ADD etc.

(require 'asdf)
(asdf:load-system :cl-change-case)
(asdf:load-system :yacc)
(asdf:load-system :serapeum)
(asdf:load-system :unix-opts)
(asdf:load-system :alexandria)
(asdf:load-system :cl-ppcre)
(asdf:load-system :local-time)
(asdf:load-system :split-sequence)
(asdf:load-system :uiop)
(asdf:load-system :fiveam)
(asdf:load-system :eightbol)

;; Test that the constructor functions are available and working
(format t "~%=== Testing Constructor Function Availability ==~%")
(format t "make-expression-add: ~a~%" (fboundp 'make-expression-add))
(format t "make-expression-subtract: ~a~%" (fboundp 'make-expression-subtract))
(format t "make-expression-multiply: ~a~%" (fboundp 'make-expression-multiply))
(format t "make-expression-divide: ~a~%" (fboundp 'make-expression-divide))
(format t "make-identifier: ~a~%" (fboundp 'make-identifier))
(format t "make-conditional-and: ~a~%" (fboundp 'make-conditional-and))

;; Test that we can actually call the functions
(format t "~%=== Testing Constructor Function Execution ==~%")
(let ((add-result (make-expression-add 5 3)))
  (format t "make-expression-add(5, 3) = ~a~%" add-result))
(let ((sub-result (make-expression-subtract 5 3)))
  (format t "make-expression-subtract(5, 3) = ~a~%" sub-result))
(let ((mult-result (make-expression-multiply 5 3)))
  (format t "make-expression-multiply(5, 3) = ~a~%" mult-result))
(let ((div-result (make-expression-divide 6 3)))
  (format t "make-expression-divide(6, 3) = ~a~%" div-result))
(let ((id-result (make-identifier "TEST"))))
  (format t "make-identifier(\"TEST\") = ~a~%" id-result))
(let ((cond-result (make-conditional-and t nil))))
  (format t "make-conditional-and(t, nil) = ~a~%" cond-result))

(format t "~%=== Verification Complete ==~%")
(format t "If we see the function call results above without undefined function errors,~%")
(format t "then the fix is working correctly.~%")