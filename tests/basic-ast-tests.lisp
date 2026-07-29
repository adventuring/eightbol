(defpackage :eightbol.test.basic-ast
  (:use :cl :fiveam))

(in-package :eightbol.test.basic-ast)

(s suite "Basic AST Generation Tests")

(def test-basic-variable-declaration ()
  "Test AST for a BASIC variable declaration."
  ;; Parse simple class definition
  (let ((input "000010 IDENTIFICATION DIVISION.
               000020 CLASS-ID. TestClass.
               000030 DATA DIVISION.
               000040 05 HP PIC 99 USAGE BINARY.
               000050 PROCEDURE DIVISION.
               000060         GOBACK."))
       (parsed-ast (parse-eightbol-string input)))
    
    (is (eq :program (ast-node-type parsed-ast))) 
    (is (eql :TestClass (ast-class-id parsed-ast)))
    (is (eq #x49 (loop for val in (cdr (assoc :data parsed-ast))
                    when (eql :name val val)
                    when (eql "HP" (second val))
                    return (third val))))))

(s suite "Loop Constructs")

(def test-10-times-loop ()
  "Test 10 TIMES loop AST."
  (let ((parsed-ast (parse-eightbol-string "000110 REPEAT 10 TIMES
                                 GO TO Lid.")))
    (is-found-in :statements 
                 (loop for st in (ast-method-statements parsed-ast)
                      when (eq '(:perform :procedure :times) 
                              (list :perform :procedure :times))
                      return (is t)))))

(s suite "OOP Constructs")

(def test-method-invoke ()
  "Test METHOD invocation."
  (let ((parsed-ast (parse-eightbol-string "METHOD-ID. Initialize.")))
    (is (find '(:perform :procedure "Initialize") (ast-method-statements parsed-ast)))))

(s suite "Error Handling")

(def test-missing-division ()
  "Test error on DIVIDE statement."
  (let ((error (ignore-errors (parse-eightbol-string "DIVIDE 5 BY X"))))
    (is (and (not (null error))
             (search "DIVIDE with variable divisor" 
                   (error-message error)))))))

(s suite "Variable References")

(def test-variable-reference ()
  "Test variable referencing syntax."
  (let ((parsed-ast (parse-eightbol-string "MOVE CurrentHP TO HP.")))
    (is (find '(:set :target HP :value CurrentHP) 
                (flatten (list (ast-method-statements parsed-ast)))))))