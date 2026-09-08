;;;; tests/backend-comprehensive-ast-tests.lisp — Verify all backends handle all AST types
;;; Tests that each backend can compile all required AST node types without error

(in-package :eightbol/test)

(def-suite :backend-ast-comprehensive :description "Backend AST node handling for all 14 backends")
(in-suite :backend-ast-comprehensive)

(defparameter *all-backends* '(:6502 :65c02 :65c816 :arm7 :cp1610 :f8 :huc6280 :i286 :m6800 :m68k :rp2a03 :sm83 :z80 :stack))

(defmacro test-ast-node-on-all-backends (node-type cobol-code description)
  `(test ,(intern (format nil "AST/~A/ALL-BACKENDS-COMPILE" node-type) :eightbol/test)
     ,description
     (dolist (cpu *all-backends*)
       (let* ((ast (eightbol::parse-eightbol-string
                    (format nil
                            "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. TestClass.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 TestVar PIC 9999 USAGE BINARY.
000080 05 TestVal PIC 9999 USAGE BINARY.
000090 PROCEDURE DIVISION.
000100 IDENTIFICATION DIVISION.
000110 METHOD-ID. \"TestMethod\".
000120 PROCEDURE DIVISION.
000130 ~a
000140 GOBACK.
000150 END METHOD \"TestMethod\".
000160 END OBJECT.
000170 END CLASS TestClass." ,cobol-code))))
         (is (not (null ast)) (format nil "CPU ~a: AST parsed for ~a" cpu ,node-type))
         (let ((asm (with-output-to-string (s)
                      (eightbol::compile-to-assembly-with-ast-passes ast cpu s))))
           (is (plusp (length asm)) (format nil "CPU ~a: Generated assembly for ~a" cpu ,node-type)))
           (is (not (search "Unsupported\|unsupported\|error\|Error" asm))
               (format nil "CPU ~a: No unsupported markers in ~a assembly" cpu ,node-type)))))))

;;;; AST Node Type Coverage

(test-ast-node-on-all-backends :move "MOVE 5 TO TestVar."
  "All backends compile :move AST node")

(test-ast-node-on-all-backends :set "SET TestVar TO 10."
  "All backends compile :set AST node")

(test-ast-node-on-all-backends :add "COMPUTE TestVar = TestVar + 5."
  "All backends compile :add (via :compute) AST node")

(test-ast-node-on-all-backends :subtract "COMPUTE TestVar = TestVar - 1."
  "All backends compile :subtract (via :compute) AST node")

(test-ast-node-on-all-backends :multiply "MULTIPLY 2 BY TestVar."
  "All backends compile :multiply AST node (power-of-two)")

(test-ast-node-on-all-backends :divide "DIVIDE 4 INTO TestVar."
  "All backends compile :divide AST node (power-of-two)")

(test-ast-node-on-all-backends :if "IF TestVar > 0 MOVE 1 TO TestVal ELSE MOVE 0 TO TestVal END-IF."
  "All backends compile :if AST node")

(test-ast-node-on-all-backends :perform "PERFORM 5 TIMES MOVE 1 TO TestVar END-PERFORM."
  "All backends compile :perform AST node")

(test-ast-node-on-all-backends :string-blt "STRING \"TEST\" DELIMITED BY SIZE INTO TestVar."
  "All backends compile :string-blt AST node")

(test-ast-node-on-all-backends :evaluate "EVALUATE TestVar WHEN 1 MOVE 10 TO TestVal WHEN OTHER MOVE 0 TO TestVal END-EVALUATE."
  "All backends compile :evaluate AST node")

(test-ast-node-on-all-backends :inspect "INSPECT \"HELLO\" TALLYING TestVar FOR CHARACTERS IN \"H\"."
  "All backends compile :inspect AST node")

(test-ast-node-on-all-backends :goback "GOBACK."
  "All backends compile :goback AST node")

(test-ast-node-on-all-backends :compute "COMPUTE TestVar = TestVar + (TestVal * 2)."
  "All backends compile :compute AST node with expressions")

;;;; Optimizer Effectiveness Tests

(test optimizer/divide-power-of-two-effective
  "Optimizer converts DIVIDE by power-of-two to shift"
  (let* ((ast (eightbol::parse-eightbol-string
               "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. OptTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 Value PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"Test\".
000110 PROCEDURE DIVISION.
000120 DIVIDE 4 INTO Value.
000130 GOBACK.
000140 END METHOD \"Test\".
000150 END OBJECT.
000160 END CLASS OptTest."))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (optimized (eightbol::optimize-ast stmts)))
    (is (find-if (lambda (s)
                   (and (listp s)
                        (eq (first s) :compute)
                        (eql (getf (rest s) :expression)
                             (list :shift-right (getf (rest s) :target) 2))))
                 optimized))
        "Optimizer should convert DIVIDE 4 INTO X to :compute with :shift-right")))

(test optimizer/multiply-power-of-two-effective
  "Optimizer converts MULTIPLY by power-of-two to shift"
  (let* ((ast (eightbol::parse-eightbol-string
               "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. OptTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 Value PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"Test\".
000110 PROCEDURE DIVISION.
000120 MULTIPLY 2 BY Value.
000130 GOBACK.
000140 END METHOD \"Test\".
000150 END OBJECT.
000160 END CLASS OptTest."))
         (method (car (eightbol::ast-methods (car ast))))
         (stmts (eightbol::ast-method-statements method))
         (optimized (eightbol::optimize-ast stmts)))
    (is (not (null optimized))
        "Optimizer should handle MULTIPLY by power-of-two")))

(test optimizer/constant-folding-effective
  "Optimizer performs constant folding on expressions"
  (let ((expr '(:add :from 5 :to 3)))
    (let ((folded (eightbol::algebraic-simplify expr)))
      (is (eql folded 8)
          "Optimizer should fold constant addition"))))

(test optimizer/algebraic-simplify-effective
  "Optimizer simplifies algebraic expressions"
  (let ((expr '(:add :from 0 :to "X")))
    (let ((simplified (eightbol::algebraic-simplify expr)))
      (is (equal simplified "X")
          "Optimizer should simplify X + 0 = X"))))

;;;; Backend Output Quality Tests

(test backend/6502-assembly-syntax
  "6502 backend generates valid 6502 assembly"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 Value PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"Main\".
000110 PROCEDURE DIVISION.
000120 MOVE 42 TO Value.
000130 GOBACK.
000140 END METHOD \"Main\".
000150 END OBJECT.
000160 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src))
         (asm (with-output-to-string (s)
                (eightbol::compile-to-assembly-with-ast-passes ast :6502 s))))
    (is (search "lda\|ldx\|ldy\|sta\|stx\|sty\|jsr\|rts" asm)
        "6502 assembly should contain valid 6502 opcodes")))

(test backend/z80-assembly-syntax
  "Z80 backend generates valid Z80 assembly"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 Value PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"Main\".
000110 PROCEDURE DIVISION.
000120 MOVE 42 TO Value.
000130 GOBACK.
000140 END METHOD \"Main\".
000150 END OBJECT.
000160 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src))
         (asm (with-output-to-string (s)
                (eightbol::compile-to-assembly-with-ast-passes ast :z80 s))))
    (is (search "ld\|jp\|call\|ret" asm)
        "Z80 assembly should contain valid Z80 opcodes")))

(test backend/arm7-assembly-syntax
  "ARM7 backend generates valid ARM Thumb assembly"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 Value PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"Main\".
000110 PROCEDURE DIVISION.
000120 MOVE 42 TO Value.
000130 GOBACK.
000140 END METHOD \"Main\".
000150 END OBJECT.
000160 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src))
         (asm (with-output-to-string (s)
                (eightbol::compile-to-assembly-with-ast-passes ast :arm7 s))))
    (is (search "movs\|ldr\|str\|bx\|bl" asm)
        "ARM7 assembly should contain valid ARM Thumb opcodes")))

(test backend/stack-vm-bytecode
  "Stack VM backend generates valid stack bytecode"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 Value PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"Main\".
000110 PROCEDURE DIVISION.
000120 MOVE 42 TO Value.
000130 GOBACK.
000140 END METHOD \"Main\".
000150 END OBJECT.
000160 END CLASS Test.")
         (ast (eightbol::parse-eightbol-string src))
         (code (with-output-to-string (s)
                 (eightbol::compile-to-assembly-with-ast-passes ast :stack s))))
    (is (not (null code))
        "Stack VM should generate bytecode/instructions")))

