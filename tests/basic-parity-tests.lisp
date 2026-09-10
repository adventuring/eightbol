(in-package :eightbol/test)

(fiveam:def-suite :dartmouth-basic-parity
  :description "Dartmouth BASIC transpile parity on :6502 and :cp1610")

(in-suite :dartmouth-basic-parity)

(test basic/transpile-entrypoints-exist
  "Verify BASIC AST compilation entry points are available."
  (is (fboundp 'eightbol::basic-ast-from-source))
  (is (fboundp 'eightbol::compile-basic-from-path)
      "BASIC compilation must use AST output, not COBOL transpile"))

(test basic/cp1610-and-6502-both-compile
  (skip "BASIC must produce AST, not transpile")
  #+skip (when (fboundp 'eightbol::basic-transpile-to-assembly)
    (dolist (cpu '(:6502 :cp1610))
      (finishes
        (let ((asm (eightbol::basic-transpile-to-assembly
                    "10 LET A = 5\n20 LET B = A + 3"
                    :cpu cpu)))
          (is (or (null asm) (stringp asm))))))))

(test basic/statement-transpile-works
  (skip "BASIC must produce AST, not transpile to COBOL")
  #+skip (is (string= "MOVE 5 TO A" 
               (eightbol::basic-transpile-statement-one-line "LET A = 5")))
  #+skip (is (string= "PERFORM 100" 
               (eightbol::basic-transpile-statement-one-line "GOSUB 100")))
  #+skip (is (string= "GOBACK" 
               (eightbol::basic-transpile-statement-one-line "RETURN")))
  #+skip (is (string= "IF condition THEN then-branch" 
               (eightbol::basic-transpile-statement-one-line "IF condition THEN then-branch")))
  #+skip (is (string= "IF condition THEN then-branch ELSE else-branch" 
               (eightbol::basic-transpile-statement-one-line "IF condition THEN then-branch ELSE else-branch")))
  #+skip (is (string= "PERFORM VARYING I FROM 1 UNTIL I > 10" 
               (eightbol::basic-transpile-statement-one-line "FOR I = 1 TO 10")))
  #+skip (is (string= "PERFORM VARYING I FROM 1 BY 2 UNTIL I > 10" 
               (eightbol::basic-transpile-statement-one-line "FOR I = 1 TO 10 STEP 2"))))

(test basic/full-program-transpile
  (skip "BASIC must produce AST, not transpile to COBOL")
  #+skip (let* ((basic-code "10 LET HP = 100\n20 LET MP = 50\n30 RETURN")
         (cobol-text (eightbol::transpile-basic-to-cobol-string "TestGame" basic-code))
         (expected-contains '("IDENTIFICATION DIVISION" 
                               "PROGRAM-ID \"TestGame\""
                               "DATA DIVISION"
                               "WORKING-STORAGE SECTION"
                               "COPY \"Phantasia-Globals\""
                               "PROCEDURE DIVISION"
                               "METHOD-ID \"Class-P\""
                               "MOVE 100 TO HP"
                               "MOVE 50 TO MP"
                               "GOBACK"
                                "END METHOD \"Class-P\"")))
    (dolist (expected expected-contains)
      (is (search expected cobol-text)
          "Generated COBOL should contain: ~A" expected))))

(test basic/assembly-generation
  (skip "BASIC must produce AST, not transpile")
  #+skip (let* ((basic-code "10 LET A = 5\n20 LET B = A + 3")
         (asm (eightbol::basic-transpile-to-assembly basic-code :cpu :6502)))
    (is (stringp asm)
        "Should generate assembly string")
    (is (> (length asm) 0)
        "Assembly should not be empty")))

(test basic/compile-success
  (skip "BASIC must produce AST, not transpile")
  #+skip (let* ((basic-code "10 LET HP = 100\n20 RETURN")
         (result (eightbol::basic-shell-run basic-code :cpu :6502)))
    (is (integerp result)
        "Should return integer result code")
    (is (zerop result)
        "Basic program should compile successfully")))
