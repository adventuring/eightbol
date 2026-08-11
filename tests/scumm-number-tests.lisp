;;; tests/scumm-number-tests.lisp — Tests for SCUMM number literal parsing
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(def-suite scumm-lexer-tests
  :description "Tests for SCUMM lexer and number format support"
  :in eightbol)

(in-suite scumm-lexer-tests)

;;; Number Literal Format Tests

(test decimal-number-literals
  "Test parsing of decimal number literals"
  (let ((tokens (scumm-lex-line "set score 100")))
    (is (equal 3 (length tokens)))
    (is (equal (car (third tokens)) :number))
    (is (equal (cdr (third tokens)) "100"))))

(test hexadecimal-number-literals
  "Test parsing of hexadecimal number literals (0x prefix)"
  (let ((tokens (scumm-lex-line "set address 0xFF")))
    (is (equal 3 (length tokens)))
    (is (equal (car (third tokens)) :number))
    (is (equal (cdr (third tokens)) "0xFF")))
  (let ((tokens (scumm-lex-line "set value 0x1A2B")))
    (is (equal 3 (length tokens)))
    (is (equal (cdr (third tokens)) "0x1A2B"))))

(test octal-number-literals
  "Test parsing of octal number literals (0o prefix)"
  (let ((tokens (scumm-lex-line "set perm 0o755")))
    (is (equal 3 (length tokens)))
    (is (equal (car (third tokens)) :number))
    (is (equal (cdr (third tokens)) "0o755")))
  (let ((tokens (scumm-lex-line "set mask 0o17")))
    (is (equal 3 (length tokens)))
    (is (equal (cdr (third tokens)) "0o17"))))

(test binary-number-literals
  "Test parsing of binary number literals (0b prefix)"
  (let ((tokens (scumm-lex-line "set flags 0b11010010")))
    (is (equal 3 (length tokens)))
    (is (equal (car (third tokens)) :number))
    (is (equal (cdr (third tokens)) "0b11010010")))
  (let ((tokens (scumm-lex-line "set bits 0b1010")))
    (is (equal 3 (length tokens)))
    (is (equal (cdr (third tokens)) "0b1010"))))

(test dword-number-literals
  "Test parsing of DWORD number literals (0d prefix)"
  (let ((tokens (scumm-lex-line "set large 0d65536")))
    (is (equal 3 (length tokens)))
    (is (equal (car (third tokens)) :number))
    (is (equal (cdr (third tokens)) "0d65536")))
  (let ((tokens (scumm-lex-line "set addr 0d16777216")))
    (is (equal 3 (length tokens)))
    (is (equal (cdr (third tokens)) "0d16777216"))))

(test parse-number-literal-decimal
  "Test scumm-parse-number-literal with decimal"
  (multiple-value-bind (val fmt) (scumm-parse-number-literal "100")
    (is (equal val 100))
    (is (equal fmt :decimal))))

(test parse-number-literal-hex
  "Test scumm-parse-number-literal with hexadecimal"
  (multiple-value-bind (val fmt) (scumm-parse-number-literal "0xFF")
    (is (equal val 255))
    (is (equal fmt :hex)))
  (multiple-value-bind (val fmt) (scumm-parse-number-literal "0x1A")
    (is (equal val 26))
    (is (equal fmt :hex))))

(test parse-number-literal-octal
  "Test scumm-parse-number-literal with octal"
  (multiple-value-bind (val fmt) (scumm-parse-number-literal "0o755")
    (is (equal val 493))
    (is (equal fmt :octal)))
  (multiple-value-bind (val fmt) (scumm-parse-number-literal "0o17")
    (is (equal val 15))
    (is (equal fmt :octal))))

(test parse-number-literal-binary
  "Test scumm-parse-number-literal with binary"
  (multiple-value-bind (val fmt) (scumm-parse-number-literal "0b1010")
    (is (equal val 10))
    (is (equal fmt :binary)))
  (multiple-value-bind (val fmt) (scumm-parse-number-literal "0b11111111")
    (is (equal val 255))
    (is (equal fmt :binary))))

(test parse-number-literal-dword
  "Test scumm-parse-number-literal with DWORD"
  (multiple-value-bind (val fmt) (scumm-parse-number-literal "0d65536")
    (is (equal val 65536))
    (is (equal fmt :dword)))
  (multiple-value-bind (val fmt) (scumm-parse-number-literal "0d4294967295")
    (is (equal val 4294967295))
    (is (equal fmt :dword))))

(test parse-number-literal-case-insensitive
  "Test that number prefixes are case-insensitive"
  (multiple-value-bind (val1 fmt1) (scumm-parse-number-literal "0xFF")
    (multiple-value-bind (val2 fmt2) (scumm-parse-number-literal "0XFF")
      (is (equal val1 val2))
      (is (equal fmt1 fmt2))))
  (multiple-value-bind (val1 fmt1) (scumm-parse-number-literal "0b1010")
    (multiple-value-bind (val2 fmt2) (scumm-parse-number-literal "0B1010")
      (is (equal val1 val2))
      (is (equal fmt1 fmt2)))))

;;; Identifier Normalization Tests

(test identifier-to-camel-case-underscore
  "Test conversion of snake_case identifiers to camelCase"
  (is (equal (scumm-to-camel-case "player_position") "playerPosition"))
  (is (equal (scumm-to-camel-case "score_board") "scoreBoard"))
  (is (equal (scumm-to-camel-case "x") "x")))

(test identifier-to-camel-case-kebab
  "Test conversion of kebab-case identifiers to camelCase"
  (is (equal (scumm-to-camel-case "player-position") "playerPosition"))
  (is (equal (scumm-to-camel-case "score-board") "scoreBoard")))

(test identifier-to-camel-case-mixed
  "Test conversion of mixed snake/kebab case identifiers"
  (is (equal (scumm-to-camel-case "player_x_pos") "playerXPos"))
  (is (equal (scumm-to-camel-case "score-table_id") "scoreTableId")))

(test identifier-to-camel-case-already-camel
  "Test identifiers already in camelCase remain unchanged"
  (is (equal (scumm-to-camel-case "playerPosition") "playerPosition"))
  (is (equal (scumm-to-camel-case "scoreBoard") "scoreBoard")))

(test identifier-to-camel-case-single-word
  "Test single-word identifiers remain unchanged"
  (is (equal (scumm-to-camel-case "score") "score"))
  (is (equal (scumm-to-camel-case "lives") "lives")))
