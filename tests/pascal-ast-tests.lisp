(defpackage :eightbol.test.pascal-ast
  (:use :cl :fiveam))

(in-package :eightbol.test.pascal-ast)

(s suite "Pascal AST Generation Tests")

(def test-pascal-variable-declaration ()
  "Test AST for a Pascal variable declaration."
  (let ((input "10 IDENTIFICATION DIVISION.
               20 DATA DIVISION.
               30 WORKING-STORAGE SECTION.
               40 10 VAR PIC 99 USAGE BINARY.")
       (parsed-ast (parse-eightbol-string input)))
    (is (eq :program (ast-node-type parsed-ast)))
    (is (find '(:dd :level 10 :label "VAR") (ast-data parsed-ast)))))

(s suite "Conditional Constructs")

def test-if-then-else ()
  "Test IF-THEN-ELSE AST."
  (let ((input "IF X IS EQUAL TO Y THEN
               MOVE 1 TO Z
               ELSE
               MOVE 2 TO Z
               END-IF."
       (parsed-ast (parse-eightbol-string input))))
    (is (find '(:if :condition (= X Y) :then ((:move :from 1 :to Z)) :else ((:move :from 2 :to Z)))
              (ast-statements parsed-ast)))))

(s suite "Loop Constructs")

def test-repeat-until-loop ()
  "Test REPEAT-UNTIL loop AST."
  (let ((input "PERFORM UNTIL X IS EQUAL TO Y
               MOVE 1 TO Z
               END-PERFORM."
       (parsed-ast (parse-eightbol-string input))))
    (is (find '(:perform :procedure "Repeat" :until (= X Y)) (ast-statements parsed-ast)))))

(s suite "OOP Constructs")

def test-pascal-method-call ()
  "Test Pascal method call."
  (let ((input "INVOKE SELF 'Execute'."
       (parsed-ast (parse-eightbol-string input))))
    (is (find '(:invoke :object :self :method "Execute") (ast-statements parsed-ast)))))

(s suite "Error Handling")

def test-multiply-unsupported ()
  "Test error on MULTIPLY statement with non-power-of-two."
  (let ((input "MULTIPLY 5 BY X"
       (parsed-ast (parse-eightbol-string input))))
    (is (errorp (parse-eightbol-string input)) "MULTIPLY with non-power-of-two is unsupported"))))