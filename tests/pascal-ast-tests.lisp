(defpackage :eightbol.test.pascal-ast
  (:use :cl :fiveam))

(in-package :eightbol.test.pascal-ast)

(def-suite :pascal-ast :description "Pascal AST Generation Tests")
(in-suite :pascal-ast)

(test pascal/variable-declaration
  "Test AST for a Pascal variable declaration."
  (let* ((input "10 IDENTIFICATION DIVISION.
20 DATA DIVISION.
30 WORKING-STORAGE SECTION.
40 10 VAR PIC 99 USAGE BINARY.")
         (parsed-ast (parse-eightbol-string input)))
    (is (eq :program (ast-node-type parsed-ast)))
    (is (find '(:dd :level 10 :label "VAR") (ast-data parsed-ast)))))

(test pascal/if-then-else
  "Test IF-THEN-ELSE AST."
  (let* ((input "IF X IS EQUAL TO Y THEN
MOVE 1 TO Z
ELSE
MOVE 2 TO Z
END-IF.")
         (parsed-ast (parse-eightbol-string input)))
    (is (not (null parsed-ast)))))

(test pascal/repeat-until-loop
  "Test REPEAT-UNTIL loop AST."
  (let* ((input "PERFORM UNTIL X IS EQUAL TO Y
MOVE 1 TO Z
END-PERFORM.")
         (parsed-ast (parse-eightbol-string input)))
    (is (not (null parsed-ast)))))

(test pascal/method-call
  "Test Pascal method call."
  (let* ((input "INVOKE SELF 'Execute'.")
         (parsed-ast (parse-eightbol-string input)))
    (is (not (null parsed-ast)))))

(test pascal/multiply-unsupported
  "Test error on MULTIPLY statement with non-power-of-two."
  (let ((input "MULTIPLY 5 BY X"))
    (is (not (parse-eightbol-string input)))))
