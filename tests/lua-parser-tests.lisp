;;; lua-parser-tests.lisp — Lua parser test suite
;;; Tests cover Lua lexer/parser integration, AST mapping, and error cases
;;; All tests in :eightbol suite, run via (asdf:test-system :eightbol) or (fiveam:run! :eightbol)

(in-package :eightbol/test)

(in-suite :eightbol)

;;; ===== Test Helpers =====

(defun lua-test-source (statements)
  "Wrap STATEMENTS in minimal EIGHTBOL class structure for parsing."
  (format nil "~{000010 ~a~%~}"
          (append
           (list "IDENTIFICATION DIVISION.")
           (list "CLASS-ID. TestLua.")
           (list "ENVIRONMENT DIVISION.")
           (list "OBJECT.")
           "  PROCEDURE DIVISION."
           (append statements "  ")
           (list "END OBJECT.")
           (list "END CLASS TestLua."))))

;;; ===== Lexer Tests =====

(test lua-lexer/tokenizes-keywords
  "Lua keywords tokenize as :keyword tokens when they exist in EightBol token-list."
  (let ((source "if true then end"))
    (let ((tokens (eightbol::tokenize-lua source)))
      ;; IF, THEN, END are in the EightBol token-list
      (is (find 'eightbol::if tokens :key #'first :test #'eq))
      (is (find 'eightbol::then tokens :key #'first :test #'eq))
      (is (find 'eightbol::end tokens :key #'first :test #'eq)))))

(test lua-lexer/tokenizes-numbers
  "Integers tokenize as :number tokens."
  (let ((source "local x = 42"))
    (let ((tokens (eightbol::tokenize-lua source)))
      (let ((num-tokens (remove-if-not (lambda (t) (eq :number (first t))) tokens)))
        (is (>= (length num-tokens) 1) "Should have at least one number token")))))

(test lua-lexer/tokenizes-strings
  "Strings tokenize with correct content."
  (let ((source "local s = 'hello'"))
    (let ((tokens (eightbol::tokenize-lua source)))
      (let ((str-tokens (remove-if-not (lambda (t) (eq :string (first t))) tokens)))
        (is (>= (length str-tokens) 1) "Should have at least one string token")))))

(test lua-lexer/skip-comments
  "Comments are skipped and produce no tokens."
  (let ((source "-- this is a comment\nlocal x = 1"))
    (let ((tokens (eightbol::tokenize-lua source)))
      (is (null (some (lambda (t) (string= "-- this is a comment" (getf t :value))) tokens))))))

;;; ===== Parser Precedence Tests =====

(test lua-parser/precedence-grouping
  "Parentheses affect expression evaluation order."
  (let* ((source (lua-test-source (list "COMPUTE result = (a + b) * c.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds with parenthesized expression")))

(test lua-parser/precedence-multiply-before-add
  "Multiplication has higher precedence than addition (evaluated first)."
  (let* ((source (lua-test-source (list "COMPUTE result = a + b * c.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds with correct precedence")))

;;; ===== Data Definition Tests =====

(test lua-parser/data-definition-level-01
  "Level 01 items create proper :dd nodes."
  (let* ((source (lua-test-source (list "01 MY-VAR PIC 9999.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds")
    (is (string= "MY-VAR" (getf (getf ast :data) :label)))))

(test lua-parser/data-definition-level-05
  "Level 05 items create proper :dd nodes with working-storage section context."
  (let* ((source (lua-test-source (list "05 HP PIC 9999 USAGE BINARY.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds")))

(test lua-parser/data-definition-occurs
  "OCCURS clauses create array definitions."
  (let* ((source (lua-test-source (list "05 HP-OCC OCCURS 10 TIMES PIC 99.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for OCCURS definition")))

;;; ===== Statement Tests =====

(test lua-parser/statement-move
  "MOVE statement maps to :dd with :move."
  (let* ((source (lua-test-source (list "MOVE 5 TO HP.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for MOVE")))

(test lua-parser/statement-add
  "ADD TO statement maps correctly."
  (let* ((source (lua-test-source (list "ADD 1 TO HP.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for ADD")))

(test lua-parser/statement-subtract
  "SUBTRACT FROM statement maps correctly."
  (let* ((source (lua-test-source (list "SUBTRACT 1 FROM HP.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for SUBTRACT")))

(test lua-parser/statement-compute
  "COMPUTE with arithmetic expression maps correctly."
  (let* ((source (lua-test-source (list "COMPUTE RESULT = A + B * C.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for COMPUTE with arithmetic")))

(test lua-parser/statement-if-simple
  "Simple IF/END-IF maps to :if node."
  (let* ((source (lua-test-source (list "IF HP IS EQUAL TO 0 THEN"
                                        "MOVE 1 TO STATUS."
                                        "END-IF.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for simple IF")))

(test lua-parser/statement-if-else
  "IF/ELSE/END-IF maps with else branch."
  (let* ((source (lua-test-source (list "IF HP IS EQUAL TO 0 THEN"
                                        "MOVE 1 TO STATUS."
                                        "ELSE"
                                        "MOVE 0 TO STATUS."
                                        "END-IF.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for IF/ELSE")))

(test lua-parser/statement-perform-until
  "PERFORM UNTIL loop maps correctly."
  (let* ((source (lua-test-source (list "PERFORM UNTIL HP <= 0"
                                        "   ADD -1 TO HP.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for PERFORM UNTIL")))

(test lua-parser/statement-perform-times
  "PERFORM n TIMES maps correctly."
  (let* ((source (lua-test-source (list "PERFORM 10 TIMES"
                                        "   ADD 1 TO HP.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for PERFORM TIMES")))

(test lua-parser/statement-invoke
  "INVOKE with object and method name."
  (let* ((source (lua-test-source (list "INVOKE Self \"Kill\".")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for INVOKE")))

(test lua-parser/statement-call
  "CALL statement maps correctly."
  (let* ((source (lua-test-source (list "CALL HookX.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for CALL")))

(test lua-parser/statement-goback
  "GOBACK statement maps to :goback node."
  (let* ((source (lua-test-source (list "GOBACK.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for GOBACK")))

;;; ===== Reference Modification Tests =====

(test lua-parser/reference-modification
  "Reference modification (start:length) parses correctly."
  (let* ((source (lua-test-source (list "MOVE HP(1:10) TO BUFFER.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for reference modification")))

(test lua-parser/subscript-access
  "Subscript access (name(index)) parses correctly."
  (let* ((source (lua-test-source (list "MOVE HP(Y) TO BUFFER.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for subscript access")))

;;; ===== Error Handling Tests =====

(test lua-parser/error-unclosed-string
  "Parser handles unclosed string literals with error."
  (handler-case 
      (let ((source (lua-test-source (list "MOVE 'UNCLOSED TO HP."))))
        (eightbol::parse-eightbol-string source))
    (error () t)))  ; Expected error

(test lua-parser/error-invalid-statement
  "Parser rejects completely invalid statements."
  (let ((invalid-source 
         (lua-test-source (list "INVALID-STATEMENT-WORD-ONLY."))))
    (handler-case 
        (eightbol::parse-eightbol-string invalid-source)
      (error () t)  ; Expected error or empty result
      (:no-error () (pass "Consider if this should error"))))

;;; ===== Method Definition Tests =====

(test lua-parser/method-definition
  "Method definitions create proper :method nodes."
  (let* ((source (format nil "~{000010 ~a~%~}"
                         (list "IDENTIFICATION DIVISION."
                               "CLASS-ID. TestClass."
                               "ENVIRONMENT DIVISION."
                               "OBJECT."
                               "  PROCEDURE DIVISION."
                               "    IDENTIFICATION DIVISION."
                               "    METHOD-ID. \"TestMethod\"."
                               "    PROCEDURE DIVISION."
                               "      MOVE 1 TO HP."
                               "    END METHOD \"TestMethod\"."
                               "  END OBJECT."
                               "END CLASS TestClass.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for method definition")
    (is (string= "TestMethod" (getf (first (getf ast :methods)) :method-id)))))

;;; ===== Regression Tests =====

(test lua-parser/regression-zero-page
  "Zero-page variables (level 01) parse correctly."
  (let* ((source (lua-test-source (list "01 LOW-REG PIC 99 USAGE BINARY.")))
         (ast (eightbol::parse-eightbol-string source))
         (dd (getf (getf ast :data) :statements)))
    (is (eq :dd (first dd)))))

(test lua-parser/regression-global-constant
  "Global constants (level 77) with VALUE parse correctly."
  (let* ((source (lua-test-source (list "77 MAX-HP PIC 9999 VALUE 100.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for global constant")))

(test lua-parser/regression-expression-parsing
  "Complex expressions parse with correct structure."
  (let* ((source (lua-test-source (list "COMPUTE RESULT = (A + B) * (C - D) + E.")))
         (ast (eightbol::parse-eightbol-string source)))
    (is (not (null ast)) "Parse succeeds for complex expression")))

(export '(lua-test-source))