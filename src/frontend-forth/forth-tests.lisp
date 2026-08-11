;; src/frontend-forth/forth-tests.lisp — Unit tests for Forth lexer and parser
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Test fixtures and helper functions

(defun test-forth-lexer-basic ()
  "Test basic Forth tokenization."
  (let ((tokens (forth-lex-line "DUP SWAP +")))
    (assert (= (length tokens) 3))
    (assert (eq (caar tokens) :word))
    (assert (string= (cdar tokens) "DUP"))))

(defun test-forth-numbers ()
  "Test number literal parsing in multiple formats."
  ;; Decimal
  (multiple-value-bind (type val pos)
      (forth-lex-number "42 next" 0)
    (assert (eq type :number))
    (assert (= val 42)))
  
  ;; Hex with $
  (multiple-value-bind (type val pos)
      (forth-lex-number "$FF00 next" 0)
    (assert (eq type :hex-number))
    (assert (= val #xFF00)))
  
  ;; Hex with 0x
  (multiple-value-bind (type val pos)
      (forth-lex-number "0x1F next" 0)
    (assert (eq type :hex-number))
    (assert (= val #x1F)))
  
  ;; Binary with %
  (multiple-value-bind (type val pos)
      (forth-lex-number "%1010 next" 0)
    (assert (eq type :binary-number))
    (assert (= val #b1010)))
  
  ;; Binary with 0b
  (multiple-value-bind (type val pos)
      (forth-lex-number "0b1111 next" 0)
    (assert (eq type :binary-number))
    (assert (= val #b1111)))
  
  ;; Octal
  (multiple-value-bind (type val pos)
      (forth-lex-number "0o777 next" 0)
    (assert (eq type :octal-number))
    (assert (= val #o777))))

(defun test-forth-identifier-normalization ()
  "Test identifier normalization to SHOUT-CASE."
  (assert (string= (forth-normalize-identifier "my-word") "MY_WORD"))
  (assert (string= (forth-normalize-identifier "DUP") "DUP"))
  (assert (string= (forth-normalize-identifier "myVar") "MYVAR"))
  (assert (string= (forth-normalize-identifier "hello_world") "HELLO_WORLD")))

(defun test-forth-lex-strings ()
  "Test string literal tokenization."
  (let ((tokens (forth-lex-line ".\" Hello World\"")))
    (assert (>= (length tokens) 2))
    (assert (eq (caar tokens) :word))
    (assert (string= (cdar tokens) ".\""))))

(defun test-forth-comments ()
  "Test comment handling."
  ;; Line comment with backslash
  (let ((tokens (forth-lex-line "DUP \\ This is a comment")))
    (assert (= (length tokens) 1))
    (assert (string= (cdar tokens) "DUP")))
  
  ;; Note: parenthetical comments are trickier in single-line parsing
  ;; They're primarily handled in forth-lex-comment-line
  )

(defun test-forth-valid-identifiers ()
  "Test identifier validation."
  (assert (forth-valid-identifier-p "my-word"))
  (assert (forth-valid-identifier-p "my_var"))
  (assert (forth-valid-identifier-p "Word"))
  (assert (forth-valid-identifier-p "#temp"))
  (assert (not (forth-valid-identifier-p "")))
  (assert (not (forth-valid-identifier-p "42"))))

(defun test-forth-parse-program ()
  "Test basic Forth program parsing."
  (let* ((source "DUP SWAP +")
         (tokens (forth-tokenize-source source))
         (ast (forth-parse-tokens tokens)))
    (assert (eq (car ast) :forth-program))
    (assert (getf (rest ast) :definitions))))

(defun test-forth-stack-operations ()
  "Test parsing of stack manipulation operations."
  (dolist (word '("DUP" "SWAP" "DROP" "OVER" "ROT"))
    (let* ((tokens (forth-lex-line word))
           (ast-node (forth-parse-token
                      (make-forth-parse-context)
                      (car tokens)
                      0
                      tokens)))
      (assert (first ast-node)))))

(defun test-forth-arithmetic ()
  "Test arithmetic operation parsing."
  (let ((tokens (forth-lex-line "5 3 +")))
    (assert (= (length tokens) 3))
    (assert (eq (car (first tokens)) :number))
    (assert (eq (car (second tokens)) :number))
    (assert (eq (car (third tokens)) :word))))

(defun test-forth-dialogues ()
  "Test dialogue word parsing."
  (let ((tokens (forth-lex-line "SAY \"Hello, world!\"")))
    (assert (>= (length tokens) 1))
    (assert (eq (car (car tokens)) :word))
    (assert (string= (cdr (car tokens)) "SAY"))))

(defun run-all-forth-tests ()
  "Run all Forth frontend tests."
  (format t "~&Running Forth lexer and parser tests...~%")
  (test-forth-lexer-basic)
  (format t "  ✓ Basic tokenization~%")
  
  (test-forth-numbers)
  (format t "  ✓ Number literal parsing~%")
  
  (test-forth-identifier-normalization)
  (format t "  ✓ Identifier normalization~%")
  
  (test-forth-lex-strings)
  (format t "  ✓ String tokenization~%")
  
  (test-forth-comments)
  (format t "  ✓ Comment handling~%")
  
  (test-forth-valid-identifiers)
  (format t "  ✓ Identifier validation~%")
  
  (test-forth-parse-program)
  (format t "  ✓ Program parsing~%")
  
  (test-forth-stack-operations)
  (format t "  ✓ Stack operation parsing~%")
  
  (test-forth-arithmetic)
  (format t "  ✓ Arithmetic parsing~%")
  
  (test-forth-dialogues)
  (format t "  ✓ Dialogue parsing~%")
  
  (format t "~&All Forth tests passed!~%"))

;;; Example programs for manual testing

(defvar *forth-example-simple*
  "DUP SWAP +
CR .
BYE"
  "Simple Forth program: duplicate, swap, add, print.")

(defvar *forth-example-dialogue*
  "SAY \"Welcome to the adventure!\"
CR
SHOW character-sprite
NARRATE \"The forest stretches before you.\"
CR
KEY DROP"
  "Example Forth program with dialogue and narrative.")

(defvar *forth-example-numbers*
  "42
0xFF
0b1010
0o777
$ABCD"
  "Various number literal formats.")

(defvar *forth-example-variables*
  "VARIABLE counter
0 counter !
counter @ .
10 counter +!
counter @ ."
  "Variable creation and manipulation.")
