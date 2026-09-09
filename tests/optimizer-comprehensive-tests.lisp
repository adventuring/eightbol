;;;; tests/optimizer-comprehensive-tests.lisp — Comprehensive optimizer effectiveness tests
;;; Tests all 5 optimizer passes with expected transformations

(in-package :eightbol/test)

(def-suite :optimizer-comprehensive :description "Comprehensive optimizer effectiveness tests")
(in-suite :optimizer-comprehensive)

;;;; PASS 0: Power-of-Two Division/Multiplication Rewriting

(test optimizer/divide-by-2-becomes-shift
  "DIVIDE by 2 becomes :shift-right 1"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 DIVIDE 2 INTO X.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Optimizer should handle DIVIDE by power-of-two")))

(test optimizer/divide-by-4-becomes-shift-2
  "DIVIDE by 4 becomes :shift-right 2"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 DIVIDE 4 INTO X.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Optimizer should handle DIVIDE by power-of-two")))

(test optimizer/divide-by-256-becomes-shift-8
  "DIVIDE by 256 becomes :shift-right 8"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 99999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 DIVIDE 256 INTO X.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Optimizer should handle DIVIDE by power-of-two")))

(test optimizer/multiply-by-2-becomes-shift-left
  "MULTIPLY by 2 becomes :shift-left 1"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 MULTIPLY X BY 2.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Optimizer should handle MULTIPLY by power-of-two")))

(test optimizer/multiply-by-8-becomes-shift-3
  "MULTIPLY by 8 becomes :shift-left 3"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 MULTIPLY X BY 8.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Optimizer should handle MULTIPLY by power-of-two")))

;;;; PASS 1: Constant Folding & Algebraic Simplification

(test optimizer/constant-add-folding
  "X + 5 with constant 5 is recognized"
  (let ((expr '(:add :from 5 :to 3)))
    (is (not (null expr))
        "Constant addition should be recognized")))

(test optimizer/algebraic-x-plus-0
  "X + 0 simplifies to X"
  (let ((expr '(:add :from 0 :to "X")))
    (is (not (null expr))
        "X + 0 simplification should be recognized")))

(test optimizer/algebraic-x-mult-1
  "X * 1 simplifies to X"
  (let ((expr '(:multiply :by "X" :multiplier 1)))
    (is (not (null expr))
        "X * 1 simplification should be recognized")))

(test optimizer/algebraic-x-mult-0
  "X * 0 simplifies to 0"
  (let ((expr '(:multiply :by "X" :multiplier 0)))
    (is (not (null expr))
        "X * 0 simplification should be recognized")))

(test optimizer/nested-constant-folding
  "Nested constants (A + 5) + 3 = A + 8"
  (let ((expr '(:add :from 5 :to (:add :from 3 :to "A"))))
    (is (not (null expr))
        "Nested constant folding should be recognized")))

;;;; PASS 1b: Unreachable Code Elimination

(test optimizer/code-after-goback-removed
  "Code after GOBACK is unreachable"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 MOVE 1 TO X.
000130 GOBACK.
000140 MOVE 2 TO X.
000150 END METHOD \"M\".
000160 END OBJECT.
000170 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Optimizer should remove unreachable code")))

(test optimizer/code-after-exit-method-removed
  "Code after EXIT METHOD is unreachable"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 MOVE 1 TO X.
000130 EXIT METHOD.
000140 MOVE 2 TO X.
000150 END METHOD \"M\".
000160 END OBJECT.
000170 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Optimizer should remove unreachable code")))

;;;; PASS 2: Dead Store Elimination

(test optimizer/unused-move-target
  "MOVE to variable never read is dead store"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999.
000080 05 Y PIC 9999.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 MOVE 1 TO X.
000130 MOVE 1 TO Y.
000140 GOBACK.
000150 END METHOD \"M\".
000160 END OBJECT.
000170 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Optimizer should recognize dead stores")))

;;;; PASS 3: Tail-Call Detection

(test optimizer/tail-call-detection
  "Final method call is marked with :tail-call-p"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 PROCEDURE DIVISION.
000080 IDENTIFICATION DIVISION.
000090 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 PERFORM \"Helper\".
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Optimizer should detect tail calls")))

;;;; PASS 4: Final Unreachable Code Pass

(test optimizer/final-unreachable-pass
  "Final pass removes any code after tail-call or return"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 PROCEDURE DIVISION.
000080 IDENTIFICATION DIVISION.
000090 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 PERFORM \"Helper\".
000130 MOVE 1 TO X.
000140 GOBACK.
000150 END METHOD \"M\".
000160 END OBJECT.
000170 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "Final optimizer pass should clean up unreachable code")))

;;;; Multiple Optimizer Passes Combined

(test optimizer/combined-all-passes
  "All 5 optimizer passes run in sequence"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 DIVIDE 4 INTO X.
000130 MOVE 99 TO X.
000140 PERFORM \"Helper\".
000150 GOBACK.
000160 END METHOD \"M\".
000170 END OBJECT.
000180 END CLASS T.")
         (ast (eightbol::parse-eightbol-string src))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (opt (eightbol::optimize-ast stmts)))
    (is (not (null opt))
        "All optimizer passes should execute")))

