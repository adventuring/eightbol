;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/garry -*-
;;;
;;; Garry frontend parser tests
;;;

(in-package :eightbol/test/garry)

(test garry-parser-basic
  "Garry parser produces basic AST nodes."
  (let ((parsed-ast (eightbol:garry-parse-program '())))
    (is (eql :program (ast-node-type parsed-ast)))))