;; make-parser.lisp
;; Parser construction for AGI

(in-package #:eightbol)

(defun make-agi-parser ()
  "Create a parser for AGI language."
  #'agi-parse)

(export '(make-agi-parser))
