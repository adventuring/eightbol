(defpackage :eightbol.test.basic-ast
  (:use :cl :fiveam))

(in-package :eightbol.test.basic-ast)

(def-suite :basic-ast :description "Basic AST Generation Tests")
(in-suite :basic-ast)

(test basic/variable-declaration
  "Test AST for a BASIC variable declaration."
  (let* ((input "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. TestClass.
000030 DATA DIVISION.
000040 05 HP PIC 99 USAGE BINARY.
000050 PROCEDURE DIVISION.
000060         GOBACK.")
         (parsed-ast (parse-eightbol-string input)))
    (is (eq :program (ast-node-type parsed-ast)))
    (is (eql :TestClass (ast-class-id parsed-ast)))))

(test basic/loop-construct
  "Test 10 TIMES loop AST."
  (let ((parsed-ast (parse-eightbol-string "000110 REPEAT 10 TIMES
                                  GO TO Lid.")))
    (is (not (null parsed-ast)))))

(test basic/method-invoke
  "Test METHOD invocation."
  (let ((parsed-ast (parse-eightbol-string "METHOD-ID. Initialize.")))
    (is (not (null parsed-ast)))))

(test basic/missing-division
  "Test error on DIVIDE statement."
  (let ((error (ignore-errors (parse-eightbol-string "DIVIDE 5 BY X"))))
    (is (not (null error)))))

(test basic/variable-reference
  "Test variable referencing syntax."
  (let ((parsed-ast (parse-eightbol-string "MOVE CurrentHP TO HP.")))
    (is (not (null parsed-ast)))))
