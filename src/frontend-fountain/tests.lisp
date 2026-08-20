;; src/frontend-fountain/tests.lisp — Unit tests for Fountain lexer and parser
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :fountain-frontend)

;;;; Test Utilities

(defun assert-equal (expected actual test-name)
  "Assert that EXPECTED equals ACTUAL."
  (unless (equal expected actual)
    (error "Test ~A failed: expected ~S, got ~S" test-name expected actual)))

(defun assert-true (value test-name)
  "Assert that VALUE is true."
  (unless value
    (error "Test ~A failed: expected true" test-name)))

(defun assert-token-type (expected-type token test-name)
  "Assert that TOKEN has type EXPECTED-TYPE."
  (unless (eq (token-type token) expected-type)
    (error "Test ~A failed: expected token type ~S, got ~S"
           test-name expected-type (token-type token))))

(defun run-lexer-tests ()
  "Run all lexer unit tests."
  (format t "~%=== Fountain Lexer Tests ===~%")
  
  ;; Test 1: Basic identifier tokenization
  (let ((tokens (lex-fountain-source "PLAYER INNKEEPER")))
    (assert-equal 2 (length tokens) "identifier-count")
    (assert-token-type :identifier (first tokens) "first-identifier"))
  
  ;; Test 2: Decimal number parsing
  (let ((tokens (lex-fountain-source "123")))
    (assert-token-type :decimal-number (first tokens) "decimal-token")
    (assert-equal 123 (token-value (first tokens)) "decimal-value"))
  
  ;; Test 3: Hex number parsing
  (let ((tokens (lex-fountain-source "0xFF")))
    (assert-token-type :hex-number (first tokens) "hex-token")
    (assert-equal 255 (token-value (first tokens)) "hex-value"))
  
  ;; Test 4: Octal number parsing
  (let ((tokens (lex-fountain-source "0o10")))
    (assert-token-type :octal-number (first tokens) "octal-token")
    (assert-equal 8 (token-value (first tokens)) "octal-value"))
  
  ;; Test 5: Binary number parsing
  (let ((tokens (lex-fountain-source "0b1010")))
    (assert-token-type :binary-number (first tokens) "binary-token")
    (assert-equal 10 (token-value (first tokens)) "binary-value"))
  
  ;; Test 6: String literal tokenization
  (let ((tokens (lex-fountain-source "\"Hello World\"")))
    (assert-token-type :string-literal (first tokens) "string-token")
    (assert-equal "Hello World" (token-value (first tokens)) "string-value"))
  
  ;; Test 7: Variable reference ($variable)
  (let ((tokens (lex-fountain-source "$gold")))
    (assert-token-type :variable (first tokens) "variable-token")
    (assert-equal "Gold" (token-value (first tokens)) "variable-pascal-case"))
  
  ;; Test 8: Keywords recognition
  (let ((tokens (lex-fountain-source "INT EXT FADE OUT")))
    (assert-token-type :int (first tokens) "int-keyword")
    (assert-token-type :ext (second tokens) "ext-keyword")
    (assert-token-type :fade (third tokens) "fade-keyword"))
  
  ;; Test 9: PascalCase normalization
  (let ((tokens (lex-fountain-source "my-variable-name")))
    (assert-token-type :identifier (first tokens) "pascal-case-token")
    (assert-equal "MyVariableName" (token-value (first tokens)) "pascal-case-value"))
  
  ;; Test 10: Comment skipping (line comments)
  (let ((tokens (lex-fountain-source ";; This is a comment\nPLAYER")))
    (let ((filtered (remove-if (lambda (t) (eq (token-type t) :line-comment)) tokens)))
      (assert-equal 2 (length filtered) "comment-filtered-tokens")))
  
  ;; Test 11: Block comments [[ ]]
  (let ((tokens (lex-fountain-source "[[ This is a block comment ]] PLAYER")))
    (let ((filtered (remove-if (lambda (t) (eq (token-type t) :block-comment)) tokens)))
      (assert-equal 2 (length filtered) "block-comment-filtered")))
  
  ;; Test 12: Multi-line script
  (let ((source "INT TAVERN - MAIN ROOM
INNKEEPER
Welcome to the tavern!
$gold = 100"))
        (tokens (lex-fountain-source source)))
    (assert-true (> (length tokens) 5) "multi-line-token-count"))
  
  ;; Test 13: Operators and punctuation
  (let ((tokens (lex-fountain-source "( ) [ ] { } , . = + - * /")))
    (assert-true (> (length tokens) 5) "operators-token-count"))
  
  ;; Test 14: Dword number (0d"WORD")
  (let ((tokens (lex-fountain-source "0d\"TEST\"")))
    (assert-token-type :dword-number (first tokens) "dword-token"))
  
  ;; Test 15: Scene header with (BLOB)
  (let ((tokens (lex-fountain-source "INT TITLE CARD (BLOB)")))
    (assert-token-type :int (first tokens) "blob-scene-int")
    ;; The lexer will tokenize "TITLE" and "CARD" separately, and then "(BLOB)"
    (assert-true (find-if (lambda (tok) (eq (token-type tok) :lparen)) tokens) "blob-has-lparen"))
  
  (format t "~%All lexer tests passed!~%"))

(defun run-parser-tests ()
  "Run all parser unit tests."
  (format t "~%=== Fountain Parser Tests ===~%")
  
  ;; Test 1: Parse scene header
  (multiple-value-bind (ast errors)
      (parse-fountain-source "INT TAVERN - MAIN ROOM")
    (declare (ignore errors))
    (assert-true (listp ast) "ast-is-list")
    (assert-equal :program (first ast) "ast-is-program"))
  
  ;; Test 2: Parse dialogue
  (multiple-value-bind (ast errors)
      (parse-fountain-source "PLAYER\nWelcome to the tavern!")
    (declare (ignore errors))
    (assert-true (listp ast) "dialogue-ast-is-list")
    (assert-equal :program (first ast) "dialogue-ast-is-program"))
  
  ;; Test 3: Parse character entry
  (multiple-value-bind (ast errors)
      (parse-fountain-source "Enter PLAYER at \"Tavern Door\"")
    (declare (ignore errors))
    (assert-true (listp ast) "entry-ast-is-list"))
  
  ;; Test 4: Parse variable assignment
  (multiple-value-bind (ast errors)
      (parse-fountain-source "Set $gold to 100")
    (declare (ignore errors))
    (assert-true (listp ast) "assignment-ast-is-list"))
  
  ;; Test 5: Parse conditional (WHEN)
  (multiple-value-bind (ast errors)
      (parse-fountain-source "When $gold is greater than 50 Then\nINNKEEPER\nYou have enough gold!")
    (declare (ignore errors))
    (assert-true (listp ast) "conditional-ast-is-list"))
  
  ;; Test 6: Parse PRINT statement
  (multiple-value-bind (ast errors)
      (parse-fountain-source "PRINT \"Gold: \" $gold")
    (declare (ignore errors))
    (assert-true (listp ast) "print-ast-is-list"))
  
  ;; Test 7: Parse INPUT statement
  (multiple-value-bind (ast errors)
      (parse-fountain-source "INPUT \"Enter name: \" $name")
    (declare (ignore errors))
    (assert-true (listp ast) "input-ast-is-list"))
  
  ;; Test 8: Parse BLOB scene
  (multiple-value-bind (ast errors)
      (parse-fountain-source "INT TITLE CARD (BLOB)")
    (declare (ignore errors))
    (assert-true (listp ast) "blob-ast-is-list"))
  
  ;; Test 9: Complex scene with dialogue and actions
  (let ((source "INT TAVERN - MAIN ROOM
Enter INNKEEPER at \"Bar Counter\"
INNKEEPER
Welcome! What can I do for you?
Set $conversation-started to 1"))
    (multiple-value-bind (ast errors)
        (parse-fountain-source source)
      (declare (ignore errors))
      (assert-true (listp ast) "complex-ast-is-list")
      (assert-true (> (length ast) 2) "complex-ast-has-statements")))
  
  ;; Test 10: Fade out transition
  (multiple-value-bind (ast errors)
      (parse-fountain-source "-- FADE OUT")
    (declare (ignore errors))
    (assert-true (listp ast) "transition-ast-is-list"))
  
  (format t "~%All parser tests passed!~%"))

(defun run-normalization-tests ()
  "Run identifier normalization tests."
  (format t "~%=== Identifier Normalization Tests ===~%")
  
  ;; Test 1: Hyphen to PascalCase
  (assert-equal "MyVariable" (to-pascal-case "my-variable") "hyphen-normalization")
  
  ;; Test 2: Underscore to PascalCase
  (assert-equal "MyVariable" (to-pascal-case "my_variable") "underscore-normalization")
  
  ;; Test 3: Mixed case to PascalCase
  (assert-equal "MyVariable" (to-pascal-case "myVariable") "mixed-case-normalization")
  
  ;; Test 4: Already PascalCase
  (assert-equal "MyVariable" (to-pascal-case "MyVariable") "already-pascal-case")
  
  ;; Test 5: Single word
  (assert-equal "Player" (to-pascal-case "player") "single-word")
  
  ;; Test 6: All caps
  (assert-equal "Player" (to-pascal-case "PLAYER") "all-caps")
  
  ;; Test 7: Complex identifier
  (assert-equal "BattleIntensity" (to-pascal-case "battle-intensity") "complex-identifier")
  
  ;; Test 8: Multiple delimiters
  (assert-equal "MyLongVariable" (to-pascal-case "my--long__variable") "multiple-delimiters")
  
  (format t "~%All normalization tests passed!~%"))

(defun run-number-literal-tests ()
  "Run number literal parsing tests."
  (format t "~%=== Number Literal Tests ===~%")
  
  ;; Test 1: Decimal
  (multiple-value-bind (val type success)
      (parse-number-literal "42")
    (assert-equal 42 val "decimal-value")
    (assert-equal :decimal-number type "decimal-type")
    (assert success "decimal-success"))
  
  ;; Test 2: Hex
  (multiple-value-bind (val type success)
      (parse-number-literal "0xFF")
    (assert-equal 255 val "hex-value")
    (assert-equal :hex-number type "hex-type")
    (assert success "hex-success"))
  
  ;; Test 3: Octal
  (multiple-value-bind (val type success)
      (parse-number-literal "0o77")
    (assert-equal 63 val "octal-value")
    (assert-equal :octal-number type "octal-type")
    (assert success "octal-success"))
  
  ;; Test 4: Binary
  (multiple-value-bind (val type success)
      (parse-number-literal "0b1111")
    (assert-equal 15 val "binary-value")
    (assert-equal :binary-number type "binary-type")
    (assert success "binary-success"))
  
  ;; Test 5: Invalid format (should fail gracefully)
  (multiple-value-bind (val type success)
      (parse-number-literal "0xZZ")
    (assert-true (not success) "invalid-hex-fails"))
  
  (format t "~%All number literal tests passed!~%"))

(defun run-all-tests ()
  "Run all Fountain frontend tests."
  (format t "~%~%╔════════════════════════════════════════╗")
  (format t "~%║  Fountain Frontend Test Suite         ║")
  (format t "~%╚════════════════════════════════════════╝")
  
  (run-normalization-tests)
  (run-number-literal-tests)
  (run-lexer-tests)
  (run-parser-tests)
  
  (format t "~%~%╔════════════════════════════════════════╗")
  (format t "~%║  ✓ All tests passed!                  ║")
  (format t "~%╚════════════════════════════════════════╝~%~%"))

(export '(run-all-tests
          run-lexer-tests
          run-parser-tests
          run-normalization-tests
          run-number-literal-tests))
