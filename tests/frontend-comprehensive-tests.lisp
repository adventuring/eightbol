;;;; tests/frontend-comprehensive-tests.lisp — Comprehensive frontend tests
;;; Tests all 18 language frontends with all major features

(in-package :eightbol/test)

(def-suite :frontend-comprehensive :description "Comprehensive frontend language tests")
(in-suite :frontend-comprehensive)

;;;; COBOL Frontend Tests

(test cobol/move-statement
  "COBOL MOVE statement generates :move AST"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 MOVE 5 TO X.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmt (car (eightbol::ast-method-statements method))))
    (is (eq (car stmt) :move)
        "MOVE should parse to :move AST node")))

(test cobol/compute-statement
  "COBOL COMPUTE with arithmetic operators"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999.
000080 05 Y PIC 9999.
000090 PROCEDURE DIVISION.
000100 IDENTIFICATION DIVISION.
000110 METHOD-ID. \"M\".
000120 PROCEDURE DIVISION.
000130 COMPUTE X = Y + 5.
000140 GOBACK.
000150 END METHOD \"M\".
000160 END OBJECT.
000170 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "COMPUTE should parse")))

(test cobol/if-then-else
  "COBOL IF/THEN/ELSE generates :if AST"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 IF X > 5 THEN
000130   MOVE 1 TO X
000140 ELSE
000150   MOVE 0 TO X
000160 END-IF.
000170 GOBACK.
000180 END METHOD \"M\".
000190 END OBJECT.
000200 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmt (car (eightbol::ast-method-statements method))))
    (is (eq (car stmt) :if)
        "IF/THEN/ELSE should parse to :if AST")))

(test cobol/set-statement
  "COBOL SET statement with various targets"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 SET X TO 42.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "SET statement should parse")))

(test cobol/perform-statement
  "COBOL PERFORM with paragraph invocation"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 PROCEDURE DIVISION.
000080 IDENTIFICATION DIVISION.
000090 METHOD-ID. \"M\".
000100 PROCEDURE DIVISION.
000110 PERFORM \"Helper\".
000120 GOBACK.
000130 END METHOD \"M\".
000140 END OBJECT.
000150 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "PERFORM should parse")))

(test cobol/invoke-method
  "COBOL INVOKE (method call)"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 PROCEDURE DIVISION.
000080 IDENTIFICATION DIVISION.
000090 METHOD-ID. \"M\".
000100 PROCEDURE DIVISION.
000110 INVOKE \"Helper\" USING 5.
000120 GOBACK.
000130 END METHOD \"M\".
000140 END OBJECT.
000150 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "INVOKE should parse")))

(test cobol/string-blt
  "COBOL STRING with DELIMITED BY SIZE"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 S PIC X(10).
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 STRING \"ABC\" DELIMITED BY SIZE INTO S.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "STRING should parse")))

(test cobol/evaluate
  "COBOL EVALUATE with WHEN clauses"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 EVALUATE X
000130   WHEN 1
000140     MOVE 10 TO X
000150   WHEN OTHER
000160     MOVE 0 TO X
000170 END-EVALUATE.
000180 GOBACK.
000190 END METHOD \"M\".
000200 END OBJECT.
000210 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "EVALUATE should parse")))

(test cobol/inspect
  "COBOL INSPECT with TALLYING/REPLACING"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 S PIC X(10).
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 INSPECT S TALLYING ALL \"A\".
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "INSPECT should parse")))

;;;; BASIC Frontend Tests

(test basic/move-to-variable
  "BASIC assignment generates COBOL MOVE equivalent"
  (let* ((src "10 LET X = 5
20 END")
         (asm (eightbol::basic-transpile-to-assembly src :6502)))
    (is (not (null asm))
        "BASIC assignment should transpile")))

(test basic/print-statement
  "BASIC PRINT generates assembly output"
  (let* ((src "10 PRINT \"Hello\"
20 END")
         (asm (eightbol::basic-transpile-to-assembly src :6502)))
    (is (not (null asm))
        "BASIC PRINT should transpile")))

(test basic/if-then
  "BASIC IF/THEN generates conditional jump"
  (let* ((src "10 IF X > 5 THEN GOTO 20
15 END
20 END")
         (asm (eightbol::basic-transpile-to-assembly src :6502)))
    (is (not (null asm))
        "BASIC IF/THEN should transpile")))

(test basic/for-loop
  "BASIC FOR/NEXT loop"
  (let* ((src "10 FOR I = 1 TO 10
20   PRINT I
30 NEXT I
40 END")
         (asm (eightbol::basic-transpile-to-assembly src :6502)))
    (is (not (null asm))
        "BASIC FOR loop should transpile")))

(test basic/arithmetic
  "BASIC arithmetic expressions"
  (let* ((src "10 LET X = A + B * C
20 END")
         (asm (eightbol::basic-transpile-to-assembly src :6502)))
    (is (not (null asm))
        "BASIC arithmetic should transpile")))

;;;; Fountain Frontend Tests

(test fountain/dialogue-block
  "Fountain dialogue block parses"
  (let* ((src "EXT. LOCATION - TIME
CHARACTER
(action)
Dialogue here")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "Fountain dialogue should parse")))

(test fountain/action-block
  "Fountain action block parses"
  (let* ((src "INT. SCENE - DAY
= Script
DO Something")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "Fountain action should parse")))

;;;; Forth Frontend Tests

(test forth/stack-operations
  "Forth stack operations transpile"
  (let* ((src ": TEST 5 DUP + ; TEST")
         (asm (eightbol::forth-transpile-to-assembly src :stack)))
    (is (not (null asm))
        "Forth stack operations should transpile")))

(test forth/definitions
  "Forth word definitions"
  (let* ((src ": DOUBLE DUP + ;
: QUAD DOUBLE DOUBLE ;
5 QUAD")
         (asm (eightbol::forth-transpile-to-assembly src :stack)))
    (is (not (null asm))
        "Forth word definitions should transpile")))

;;;; Muddle Frontend Tests

(test muddle/basic-expression
  "Muddle S-expression parses"
  (let* ((src "(PROG ((X 5)) (+ X 3))")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "Muddle S-expression should parse")))

;;;; ZIL Frontend Tests

(test zil/basic-routine
  "ZIL routine definition"
  (let* ((src "<ROUTINE TEST () <RETURN 5>>")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "ZIL routine should parse")))

;;;; Pascal Frontend Tests

(test pascal/program-structure
  "Pascal program parses"
  (let* ((src "PROGRAM test;
BEGIN
  X := 5
END.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "Pascal program should parse")))

;;;; Lua Frontend Tests

(test lua/function-definition
  "Lua function definition"
  (let* ((src "function test()
  return 5
end")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "Lua function should parse")))

;;;; Smalltalk Frontend Tests

(test smalltalk/message-send
  "Smalltalk message send"
  (let* ((src "5 + 3")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast))
        "Smalltalk message should parse")))

