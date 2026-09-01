;;; tests/forth-tests.lisp — FiveAM tests for Forth lexer, parser, and transpiler
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :eightbol/test)

(fiveam:def-suite :forth-frontend
  :description "Tests for the Forth frontend (lexer, parser, transpiler)")

(in-suite :forth-frontend)

;;; ------------------------------------------------------------------
;;; Lexer tests
;;; ------------------------------------------------------------------

(test forth-lexer-basic
  (let ((tokens (eightbol::forth-lex-line "DUP SWAP +")))
    (is (= (length tokens) 3))
    (is (eq (caar tokens) :word))
    (is (string= (cdar tokens) "DUP"))))

(test forth-lexer-numbers
  ;; Decimal
  (multiple-value-bind (type val pos)
      (eightbol::forth-lex-number "42 next" 0)
    (is (eq type :number))
    (is (= val 42)))
  ;; Hex with $
  (multiple-value-bind (type val pos)
      (eightbol::forth-lex-number "$FF00 next" 0)
    (is (eq type :hex-number))
    (is (= val #xFF00)))
  ;; Hex with 0x
  (multiple-value-bind (type val pos)
      (eightbol::forth-lex-number "0x1F next" 0)
    (is (eq type :hex-number))
    (is (= val #x1F)))
  ;; Binary with %
  (multiple-value-bind (type val pos)
      (eightbol::forth-lex-number "%1010 next" 0)
    (is (eq type :binary-number))
    (is (= val #b1010)))
  ;; Binary with 0b
  (multiple-value-bind (type val pos)
      (eightbol::forth-lex-number "0b1111 next" 0)
    (is (eq type :binary-number))
    (is (= val #b1111)))
  ;; Octal
  (multiple-value-bind (type val pos)
      (eightbol::forth-lex-number "0o777 next" 0)
    (is (eq type :octal-number))
    (is (= val #o777))))

(test forth-lexer-identifier-normalization
  (is (string= (eightbol::forth-normalize-identifier "my-word") "MY_WORD"))
  (is (string= (eightbol::forth-normalize-identifier "DUP") "DUP"))
  (is (string= (eightbol::forth-normalize-identifier "myVar") "MYVAR"))
  (is (string= (eightbol::forth-normalize-identifier "hello_world") "HELLO_WORLD")))

(test forth-lexer-strings
  (let ((tokens (eightbol::forth-lex-line ".\" Hello World\"")))
    (is (>= (length tokens) 2))
    (is (eq (caar tokens) :word))
    (is (string= (cdar tokens) ".\""))))

(test forth-lexer-comments
  (let ((tokens (eightbol::forth-lex-line "DUP \\ This is a comment")))
    (is (= (length tokens) 1))
    (is (string= (cdar tokens) "DUP"))))

(test forth-lexer-valid-identifiers
  (is (eightbol::forth-valid-identifier-p "my-word"))
  (is (eightbol::forth-valid-identifier-p "my_var"))
  (is (eightbol::forth-valid-identifier-p "Word"))
  (is (eightbol::forth-valid-identifier-p "#temp"))
  (is (not (eightbol::forth-valid-identifier-p "")))
  (is (not (eightbol::forth-valid-identifier-p "42"))))

;;; ------------------------------------------------------------------
;;; Parser tests
;;; ------------------------------------------------------------------

(test forth-parse-program
  (let* ((source "DUP SWAP +")
         (tokens (eightbol::forth-tokenize-source source))
         (ast (eightbol::forth-parse-tokens tokens)))
    (is (eq (car ast) :program))
    (is (getf (rest ast) :methods))))

(test forth-parse-colon-definition
  (let* ((source ": MY-WORD DUP SWAP + ;")
         (tokens (eightbol::forth-tokenize-source source))
         (ast (eightbol::forth-parse-tokens tokens))
         (methods (getf (rest ast) :methods)))
    (is (= (length methods) 1))
    (let ((method (first methods)))
      (is (string= (getf (rest method) :method-id) "MY_WORD"))
      (let ((stmts (getf (rest method) :statements)))
        (is (= (length stmts) 3))
        (is (eq (caar (first stmts)) :call))
        (is (eq (caar (second stmts)) :call))
        (is (eq (caar (third stmts)) :call))))))

(test forth-parse-variables
  (let* ((source "VARIABLE counter")
         (tokens (eightbol::forth-tokenize-source source))
         (ast (eightbol::forth-parse-tokens tokens)))
    (is (eq (car ast) :program))
    ;; Top-level methods should include the variable definition
    (let ((methods (getf (rest ast) :methods)))
      (is (>= (length methods) 1)))))

;;; ------------------------------------------------------------------
;;; IF/THEN/ELSE parser tests
;;; ------------------------------------------------------------------

(test forth-parse-if-then
  (let* ((source ": CHECK DUP 0> IF 42 EMIT THEN ;")
         (tokens (eightbol::forth-tokenize-source source))
         (ast (eightbol::forth-parse-tokens tokens))
         (methods (getf (rest ast) :methods)))
    (is (= (length methods) 1))
    (let* ((method (first methods))
           (stmts (getf (rest method) :statements)))
      (is (>= (length stmts) 2))
      (let ((if-node (find-if (lambda (s) (eq (car s) :if)) stmts)))
        (is if-node "IF node not found in method body")
        (is (equal (getf (rest if-node) :condition) '(:forth-test)))
        (is (getf (rest if-node) :then) "THEN branch missing")
        (is (null (getf (rest if-node) :else)) "ELSE should be empty")))))

(test forth-parse-if-then-else
  (let* ((source ": SIGN DUP 0< IF -1 ELSE 1 THEN * ;")
         (tokens (eightbol::forth-tokenize-source source))
         (ast (eightbol::forth-parse-tokens tokens))
         (methods (getf (rest ast) :methods)))
    (is (= (length methods) 1))
    (let* ((method (first methods))
           (stmts (getf (rest method) :statements)))
      (let ((if-node (find-if (lambda (s) (eq (car s) :if)) stmts)))
        (is if-node "IF node not found")
        (is (equal (getf (rest if-node) :condition) '(:forth-test)))
        (is (getf (rest if-node) :then) "THEN branch missing")
        (is (getf (rest if-node) :else) "ELSE branch missing")))))

(test forth-parse-begin-until
  (let* ((source ": COUNT DUP BEGIN 1 - DUP 0= UNTIL DROP ;")
         (tokens (eightbol::forth-tokenize-source source))
         (ast (eightbol::forth-parse-tokens tokens))
         (methods (getf (rest ast) :methods)))
    (is (= (length methods) 1))
    (let* ((method (first methods))
           (stmts (getf (rest method) :statements)))
      (let ((loop-node (find-if (lambda (s) (eq (car s) :perform)) stmts)))
        (is loop-node ":perform node not found")
        (is (getf (rest loop-node) :until) ":until missing")
        (is (getf (rest loop-node) :body) ":body missing")))))

(test forth-parse-nested-if-in-loop
  (let* ((source ": EXAMPLE BEGIN DUP 0> IF 1 - ELSE DROP THEN DUP 0= UNTIL ;")
         (tokens (eightbol::forth-tokenize-source source))
         (ast (eightbol::forth-parse-tokens tokens))
         (methods (getf (rest ast) :methods)))
    (is (= (length methods) 1))
    (let* ((method (first methods))
           (stmts (getf (rest method) :statements)))
      (let ((loop-node (find-if (lambda (s) (eq (car s) :perform)) stmts)))
        (is loop-node "Outer loop not found")
        ;; Body should contain an IF node
        (let ((body (getf (rest loop-node) :body)))
          (is (find-if (lambda (s) (eq (car s) :if)) body)
              "IF node not found inside loop body"))))))
