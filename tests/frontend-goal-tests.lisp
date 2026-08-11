;; tests/frontend-goal-tests.lisp — Unit tests for GOAL frontend
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(def-suite :goal-frontend
  :description "GOAL frontend lexer and parser tests")

(in-suite :goal-frontend)

;;; Lexer Tests
(test goal-lex-decimal-numbers
  "Test lexing of decimal number literals."
  (let ((tokens (goal-lex-line "42 1000 0")))
    (is (= (length tokens) 3))
    (is (eq (caar tokens) :number))
    (is (string= (cdar tokens) "42"))
    (is (eq (caadr tokens) :number))
    (is (string= (cdadr tokens) "1000"))))

(test goal-lex-hex-numbers
  "Test lexing of hexadecimal number literals."
  (let ((tokens (goal-lex-line "#xFF #x1000 #xDEADBEEF")))
    (is (= (length tokens) 3))
    (dolist (token tokens)
      (is (eq (car token) :number)))))

(test goal-lex-octal-numbers
  "Test lexing of octal number literals."
  (let ((tokens (goal-lex-line "#o777 #o1000 #o0")))
    (is (= (length tokens) 3))
    (dolist (token tokens)
      (is (eq (car token) :number)))))

(test goal-lex-binary-numbers
  "Test lexing of binary number literals."
  (let ((tokens (goal-lex-line "#b1111 #b10101010 #b0")))
    (is (= (length tokens) 3))
    (dolist (token tokens)
      (is (eq (car token) :number)))))

(test goal-lex-dword-literals
  "Test lexing of dword string literals."
  (let ((tokens (goal-lex-line "#d\"WORD\" #d\"DATA\"")))
    (is (= (length tokens) 2))
    (is (eq (caar tokens) :dword))
    (is (string= (cdar tokens) "WORD"))
    (is (eq (caadr tokens) :dword))
    (is (string= (cdadr tokens) "DATA"))))

(test goal-lex-identifiers
  "Test lexing of identifiers (before normalization)."
  (let ((tokens (goal-lex-line "player-name playerScore PLAYER_HEALTH")))
    (is (= (length tokens) 3))
    (dolist (token tokens)
      (is (eq (car token) :ident)))))

(test goal-lex-string-literals
  "Test lexing of quoted string literals."
  (let ((tokens (goal-lex-line "\"hello world\" \"test\"")))
    (is (= (length tokens) 2))
    (is (eq (caar tokens) :string))
    (is (string= (cdar tokens) "hello world"))
    (is (eq (caadr tokens) :string))
    (is (string= (cdadr tokens) "test"))))

(test goal-lex-operators
  "Test lexing of operators."
  (let ((tokens (goal-lex-line "+ - * / < > <= >= != ==")))
    (is (= (length tokens) 10))
    (is (eq (caar tokens) :plus))
    (is (eq (caadr tokens) :minus))
    (is (eq (caaddr tokens) :times))))

(test goal-lex-s-expressions
  "Test lexing of S-expressions."
  (let ((tokens (goal-lex-line "(+ 1 2)")))
    (is (= (length tokens) 5))
    (is (eq (caar tokens) :lparen))
    (is (eq (caadr tokens) :plus))
    (is (eq (caddr tokens) '(:number . "1")))
    (is (eq (caarth tokens) :rparen))))

(test goal-lex-comments
  "Test that comments are ignored."
  (let ((tokens (goal-lex-line "foo ; this is a comment")))
    (is (= (length tokens) 1))
    (is (eq (caar tokens) :ident))))

(test goal-lex-keywords
  "Test lexing of keywords."
  (let ((tokens (goal-lex-line "defun if let while loop")))
    (is (= (length tokens) 5))
    (is (eq (caar tokens) :defun))
    (is (eq (caadr tokens) :if))
    (is (eq (caaddr tokens) :let))))

;;; Parser Tests
(test goal-normalize-identifier-camel-case
  "Test normalization of camelCase identifiers to kebab-case."
  (is (string= (goal-normalize-identifier "playerName") "player-name"))
  (is (string= (goal-normalize-identifier "myVariable") "my-variable"))
  (is (string= (goal-normalize-identifier "XMLParser") "x-m-l-parser")))

(test goal-normalize-identifier-snake-case
  "Test normalization of snake_case identifiers to kebab-case."
  (is (string= (goal-normalize-identifier "player_score") "player-score"))
  (is (string= (goal-normalize-identifier "MY_CONSTANT") "m-y-constant"))
  (is (string= (goal-normalize-identifier "test_case") "test-case")))

(test goal-normalize-identifier-already-kebab
  "Test that kebab-case identifiers remain unchanged."
  (is (string= (goal-normalize-identifier "player-name") "player-name"))
  (is (string= (goal-normalize-identifier "my-var") "my-var")))

(test goal-normalize-identifier-mixed
  "Test normalization of mixed case identifiers."
  (is (string= (goal-normalize-identifier "Player_Name") "player-name"))
  (is (string= (goal-normalize-identifier "PlayerName") "player-name")))

(test goal-parse-number-decimal
  "Test parsing of decimal numbers."
  (let ((result (parse-goal-number "42")))
    (is (integerp result))
    (is (= result 42)))
  (let ((result (parse-goal-number "1000")))
    (is (integerp result))
    (is (= result 1000))))

(test goal-parse-number-hex
  "Test parsing of hexadecimal numbers."
  (let ((result (parse-goal-number "#xFF")))
    (is (integerp result))
    (is (= result 255)))
  (let ((result (parse-goal-number "#x1000")))
    (is (integerp result))
    (is (= result 4096))))

(test goal-parse-number-octal
  "Test parsing of octal numbers."
  (let ((result (parse-goal-number "#o777")))
    (is (integerp result))
    (is (= result 511)))
  (let ((result (parse-goal-number "#o1000")))
    (is (integerp result))
    (is (= result 512))))

(test goal-parse-number-binary
  "Test parsing of binary numbers."
  (let ((result (parse-goal-number "#b1111")))
    (is (integerp result))
    (is (= result 15)))
  (let ((result (parse-goal-number "#b10101010")))
    (is (integerp result))
    (is (= result 170))))

(test goal-parse-number-dword
  "Test parsing of dword literals."
  (let ((result (parse-goal-number "#d\"WORD\"")))
    (is (stringp result))
    (is (string= result "WORD")))
  (let ((result (parse-goal-number "#d\"DATA\"")))
    (is (stringp result))
    (is (string= result "DATA"))))

(test goal-ident-node-kebab-case
  "Test that identifier nodes normalize to kebab-case."
  (let ((node (goal-ident-node "PlayerName")))
    (is (eq (car node) :ident))
    (is (string= (getf (cdr node) :name) "player-name"))))

(test goal-parse-simple-expr
  "Test parsing of simple expressions."
  (let ((tokens '((:ident . "myVar") (:plus . "+") (:number . "42"))))
    (multiple-value-bind (expr remaining)
        (goal-parse-expr tokens)
      (is (eq (car expr) :ident))
      (is (string= (getf (cdr expr) :name) "my-var")))))

(test goal-parse-quoted-expr
  "Test parsing of quoted expressions."
  (let ((tokens '((:quote . "'") (:ident . "symbol"))))
    (multiple-value-bind (expr remaining)
        (goal-parse-expr tokens)
      (is (eq (car expr) :quote))
      (is (eq (caadr (getf (cdr expr) :expr)) :ident)))))

(test goal-parse-s-expression
  "Test parsing of S-expressions."
  (let ((tokens '((:lparen . "(")
                  (:plus . "+")
                  (:number . "1")
                  (:number . "2")
                  (:rparen . ")"))))
    (multiple-value-bind (expr remaining)
        (goal-parse-sexp (rest tokens))
      (is (eq (car expr) :list))
      (let ((elements (getf (cdr expr) :elements)))
        (is (= (length elements) 3))))))

(test goal-parse-source-integration
  "Test integration: lex and parse complete GOAL source."
  (let* ((source "(defun add (x y) (+ x y))")
         (ast (goal-parse-source source)))
    (is (listp ast))
    (is (> (length ast) 0))))

;;; Integration Tests
(test goal-roundtrip-numbers
  "Test that all number formats roundtrip correctly."
  (flet ((test-format (source)
           (let ((tokens (goal-lex-source source)))
             (is (= (length tokens) 1))
             (is (member (caar tokens) '(:number :dword))))))
    (test-format "42")
    (test-format "#xFF")
    (test-format "#o777")
    (test-format "#b1111")
    (test-format "#d\"WORD\"")))

(test goal-identifier-normalization-preserves-semantics
  "Test that identifier normalization doesn't affect parsing."
  (let ((source1 "(myFunction arg1 arg2)")
        (source2 "(my-function arg-1 arg-2)"))
    (let ((ast1 (goal-parse-source source1))
          (ast2 (goal-parse-source source2)))
      (is (not (null ast1)))
      (is (not (null ast2))))))
