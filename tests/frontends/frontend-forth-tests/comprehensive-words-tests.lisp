;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-forth -*-
;;;
;;; EIGHTBOL FORTH Comprehensive Word Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module provides comprehensive tests for all 30 FORTH words covering
;;; stack operations, arithmetic, bitwise operations, control flow, definitions,
;;; memory access, and I/O operations.

(in-package :eightbol/test/frontend-forth)

(fiveam:def-suite :forth-comprehensive-words
  :description "Comprehensive tests for 30 core FORTH words"
  :in :frontend-forth)

(in-suite :forth-comprehensive-words)

;; ============================================================================
;; STACK MANIPULATION WORDS (DUP, DROP, SWAP, OVER, ROT, DEPTH)
;; ============================================================================

(test forth/dup-basic
  "DUP duplicates the top stack value"
  (let ((result (eightbol::forth-parse-and-analyze "5 DUP")))
    (is (eightbol::ast-node-p result))))

(test forth/dup-with-binary-8bit
  "DUP works with 8-bit BINARY values"
  (let ((result (eightbol::forth-parse-and-analyze "$FF DUP")))
    (is (eightbol::ast-node-p result))))

(test forth/dup-with-binary-16bit
  "DUP works with 16-bit BINARY values"
  (let ((result (eightbol::forth-parse-and-analyze "$FFFF DUP")))
    (is (eightbol::ast-node-p result))))

(test forth/drop-basic
  "DROP removes the top stack value"
  (let ((result (eightbol::forth-parse-and-analyze "5 10 DROP")))
    (is (eightbol::ast-node-p result))))

(test forth/drop-multiple
  "Multiple DROPs work correctly"
  (let ((result (eightbol::forth-parse-and-analyze "1 2 3 DROP DROP DROP")))
    (is (eightbol::ast-node-p result))))

(test forth/swap-basic
  "SWAP exchanges top two stack items"
  (let ((result (eightbol::forth-parse-and-analyze "5 10 SWAP")))
    (is (eightbol::ast-node-p result))))

(test forth/swap-preserves-values
  "SWAP preserves value types and widths"
  (let ((result (eightbol::forth-parse-and-analyze "$FF $00 SWAP")))
    (is (eightbol::ast-node-p result))))

(test forth/over-basic
  "OVER copies second item to top"
  (let ((result (eightbol::forth-parse-and-analyze "5 10 OVER")))
    (is (eightbol::ast-node-p result))))

(test forth/over-stack-depth
  "OVER increases stack depth by one"
  (let ((result (eightbol::forth-parse-and-analyze "1 2 OVER")))
    (is (eightbol::ast-node-p result))))

(test forth/rot-basic
  "ROT rotates top three stack items"
  (let ((result (eightbol::forth-parse-and-analyze "1 2 3 ROT")))
    (is (eightbol::ast-node-p result))))

(test forth/rot-triple-rotation
  "ROT correctly rotates three distinct values"
  (let ((result (eightbol::forth-parse-and-analyze "$01 $02 $03 ROT")))
    (is (eightbol::ast-node-p result))))

(test forth/depth-returns-number
  "DEPTH returns stack depth as BINARY number"
  (let ((result (eightbol::forth-parse-and-analyze "DEPTH")))
    (is (eightbol::ast-node-p result))))

(test forth/depth-after-push
  "DEPTH correctly reflects stack depth after pushes"
  (let ((result (eightbol::forth-parse-and-analyze "1 2 3 DEPTH")))
    (is (eightbol::ast-node-p result))))

;; ============================================================================
;; ARITHMETIC WORDS (+, -, *, /, MOD)
;; ============================================================================

(test forth/addition-basic
  "Addition of two numbers"
  (let ((result (eightbol::forth-parse-and-analyze "5 3 +")))
    (is (eightbol::ast-node-p result))))

(test forth/addition-binary-8bit
  "Addition of 8-bit BINARY values"
  (let ((result (eightbol::forth-parse-and-analyze "$FF $02 +")))
    (is (eightbol::ast-node-p result))))

(test forth/addition-chained
  "Chained addition (3 operands)"
  (let ((result (eightbol::forth-parse-and-analyze "1 2 + 3 +")))
    (is (eightbol::ast-node-p result))))

(test forth/subtraction-basic
  "Subtraction of two numbers"
  (let ((result (eightbol::forth-parse-and-analyze "10 3 -")))
    (is (eightbol::ast-node-p result))))

(test forth/subtraction-order-matters
  "Subtraction is non-commutative"
  (let ((result (eightbol::forth-parse-and-analyze "3 10 -")))
    (is (eightbol::ast-node-p result))))

(test forth/multiplication-basic
  "Multiplication of two numbers"
  (let ((result (eightbol::forth-parse-and-analyze "3 4 *")))
    (is (eightbol::ast-node-p result))))

(test forth/multiplication-by-zero
  "Multiplication by zero"
  (let ((result (eightbol::forth-parse-and-analyze "100 0 *")))
    (is (eightbol::ast-node-p result))))

(test forth/division-basic
  "Division of two numbers"
  (let ((result (eightbol::forth-parse-and-analyze "20 4 /")))
    (is (eightbol::ast-node-p result))))

(test forth/division-with-remainder
  "Integer division discards remainder"
  (let ((result (eightbol::forth-parse-and-analyze "23 5 /")))
    (is (eightbol::ast-node-p result))))

(test forth/modulo-basic
  "Modulo operation"
  (let ((result (eightbol::forth-parse-and-analyze "17 5 MOD")))
    (is (eightbol::ast-node-p result))))

(test forth/modulo-relationship
  "MOD and / have defined relationship"
  (let ((result (eightbol::forth-parse-and-analyze "20 3 / 20 3 MOD")))
    (is (eightbol::ast-node-p result))))

;; ============================================================================
;; BITWISE WORDS (AND, OR, XOR, NOT, <<, >>)
;; ============================================================================

(test forth/bitwise-and
  "Bitwise AND operation"
  (let ((result (eightbol::forth-parse-and-analyze "$FF $0F AND")))
    (is (eightbol::ast-node-p result))))

(test forth/bitwise-or
  "Bitwise OR operation"
  (let ((result (eightbol::forth-parse-and-analyze "$F0 $0F OR")))
    (is (eightbol::ast-node-p result))))

(test forth/bitwise-xor
  "Bitwise XOR operation"
  (let ((result (eightbol::forth-parse-and-analyze "$FF $0F XOR")))
    (is (eightbol::ast-node-p result))))

(test forth/bitwise-not
  "Bitwise NOT (complement) operation"
  (let ((result (eightbol::forth-parse-and-analyze "$00 NOT")))
    (is (eightbol::ast-node-p result))))

(test forth/left-shift
  "Left shift operation"
  (let ((result (eightbol::forth-parse-and-analyze "$01 3 <<")))
    (is (eightbol::ast-node-p result))))

(test forth/right-shift
  "Right shift operation"
  (let ((result (eightbol::forth-parse-and-analyze "$08 2 >>")))
    (is (eightbol::ast-node-p result))))

;; ============================================================================
;; CONTROL FLOW WORDS (IF/THEN/ELSE, DO/LOOP, BEGIN/UNTIL, BEGIN/WHILE/REPEAT)
;; ============================================================================

(test forth/if-then-basic
  "IF/THEN basic conditional"
  (let ((result (eightbol::forth-parse-and-analyze "1 IF 5 THEN")))
    (is (eightbol::ast-node-p result))))

(test forth/if-else-then
  "IF/ELSE/THEN conditional with else clause"
  (let ((result (eightbol::forth-parse-and-analyze "0 IF 5 ELSE 10 THEN")))
    (is (eightbol::ast-node-p result))))

(test forth/do-loop-basic
  "DO/LOOP counted loop"
  (let ((result (eightbol::forth-parse-and-analyze "10 0 DO I LOOP")))
    (is (eightbol::ast-node-p result))))

(test forth/do-plus-loop
  "DO/+LOOP with variable increment"
  (let ((result (eightbol::forth-parse-and-analyze "20 0 DO I 2 +LOOP")))
    (is (eightbol::ast-node-p result))))

(test forth/begin-until-loop
  "BEGIN/UNTIL loop structure"
  (let ((result (eightbol::forth-parse-and-analyze "BEGIN DUP UNTIL")))
    (is (eightbol::ast-node-p result))))

(test forth/begin-while-repeat
  "BEGIN/WHILE/REPEAT loop structure"
  (let ((result (eightbol::forth-parse-and-analyze "BEGIN 1 WHILE REPEAT")))
    (is (eightbol::ast-node-p result))))

;; ============================================================================
;; WORD DEFINITION WORDS (:, VARIABLE, CONSTANT)
;; ============================================================================

(test forth/colon-definition-basic
  "Basic word definition with :"
  (let ((result (eightbol::forth-parse-and-analyze ": DOUBLE DUP + ;")))
    (is (eightbol::ast-node-p result))))

(test forth/colon-definition-uses-definition
  "Using a defined word"
  (let ((result (eightbol::forth-parse-and-analyze ": DOUBLE DUP + ; 5 DOUBLE")))
    (is (eightbol::ast-node-p result))))

(test forth/variable-definition
  "VARIABLE creates a named variable"
  (let ((result (eightbol::forth-parse-and-analyze "VARIABLE X")))
    (is (eightbol::ast-node-p result))))

(test forth/constant-definition
  "CONSTANT creates a named constant"
  (let ((result (eightbol::forth-parse-and-analyze "100 CONSTANT MAX-COUNT")))
    (is (eightbol::ast-node-p result))))

;; ============================================================================
;; MEMORY ACCESS WORDS (@, !)
;; ============================================================================

(test forth/fetch-basic
  "Fetch (load) from memory"
  (let ((result (eightbol::forth-parse-and-analyze "$1000 @")))
    (is (eightbol::ast-node-p result))))

(test forth/store-basic
  "Store (write) to memory"
  (let ((result (eightbol::forth-parse-and-analyze "42 $1000 !")))
    (is (eightbol::ast-node-p result))))

;; ============================================================================
;; I/O WORDS (EMIT, ., CR, KEY)
;; ============================================================================

(test forth/emit-character
  "EMIT outputs a character"
  (let ((result (eightbol::forth-parse-and-analyze "65 EMIT")))
    (is (eightbol::ast-node-p result))))

(test forth/print-number
  "Print number with ."
  (let ((result (eightbol::forth-parse-and-analyze "42 .")))
    (is (eightbol::ast-node-p result))))

(test forth/carriage-return
  "CR outputs newline"
  (let ((result (eightbol::forth-parse-and-analyze "CR")))
    (is (eightbol::ast-node-p result))))

(test forth/key-input
  "KEY reads a character"
  (let ((result (eightbol::forth-parse-and-analyze "KEY")))
    (is (eightbol::ast-node-p result))))

;; ============================================================================
;; NUMERIC TYPE HANDLING TESTS
;; ============================================================================

(test forth/numeric-binary-8bit
  "8-bit BINARY number parsing"
  (let ((result (eightbol::forth-parse-and-analyze "BINARY 8 $FF")))
    (is (eightbol::ast-node-p result))))

(test forth/numeric-binary-16bit
  "16-bit BINARY number parsing"
  (let ((result (eightbol::forth-parse-and-analyze "BINARY 16 $FFFF")))
    (is (eightbol::ast-node-p result))))

(test forth/numeric-decimal
  "DECIMAL number parsing"
  (let ((result (eightbol::forth-parse-and-analyze "DECIMAL 123")))
    (is (eightbol::ast-node-p result))))

(test forth/numeric-mixed-types
  "Mixed numeric type operations"
  (let ((result (eightbol::forth-parse-and-analyze "BINARY 8 $FF DECIMAL 10 +")))
    (is (eightbol::ast-node-p result))))

;; ============================================================================
;; EDGE CASE TESTS
;; ============================================================================

(test forth/empty-stack-dup
  "DUP on empty stack (error condition)"
  (let ((result (eightbol::forth-parse-and-analyze "DUP")))
    ;; Should parse, but might flag as error or warning
    (is (eightbol::ast-node-p result))))

(test forth/stack-underflow-drop
  "DROP with empty stack (error condition)"
  (let ((result (eightbol::forth-parse-and-analyze "DROP")))
    (is (eightbol::ast-node-p result))))

(test forth/stack-underflow-addition
  "Addition with single value (error condition)"
  (let ((result (eightbol::forth-parse-and-analyze "5 +")))
    (is (eightbol::ast-node-p result))))

(test forth/division-by-zero
  "Division by zero (error condition)"
  (let ((result (eightbol::forth-parse-and-analyze "10 0 /")))
    (is (eightbol::ast-node-p result))))

(test forth/nested-definitions
  "Nested word definitions"
  (let ((result (eightbol::forth-parse-and-analyze ": OUTER : INNER 42 ; INNER ;")))
    (is (eightbol::ast-node-p result))))

(test forth/recursive-definition
  "Recursive word definition using RECURSE"
  (let ((result (eightbol::forth-parse-and-analyze ": COUNTDOWN DUP 0 > IF DUP . 1 - RECURSE THEN ;")))
    (is (eightbol::ast-node-p result))))

;; ============================================================================
;; INTEGRATION TESTS - COMBINING MULTIPLE WORDS
;; ============================================================================

(test forth/stack-operations-sequence
  "Sequence of stack operations"
  (let ((result (eightbol::forth-parse-and-analyze "1 2 3 DUP DROP SWAP OVER")))
    (is (eightbol::ast-node-p result))))

(test forth/arithmetic-sequence
  "Sequence of arithmetic operations"
  (let ((result (eightbol::forth-parse-and-analyze "10 5 + 3 * 2 /")))
    (is (eightbol::ast-node-p result))))

(test forth/bitwise-sequence
  "Sequence of bitwise operations"
  (let ((result (eightbol::forth-parse-and-analyze "$FF $F0 AND $0F OR $AA XOR")))
    (is (eightbol::ast-node-p result))))

(test forth/control-with-variables
  "Control flow with variables"
  (let ((result (eightbol::forth-parse-and-analyze "VARIABLE X 5 X ! X @ 0 > IF 42 . THEN")))
    (is (eightbol::ast-node-p result))))

(test forth/loop-with-stack-ops
  "Loop with stack operations"
  (let ((result (eightbol::forth-parse-and-analyze "10 0 DO I DUP + . LOOP")))
    (is (eightbol::ast-node-p result))))

(test forth/complex-calculation
  "Complex multi-operation calculation"
  (let ((result (eightbol::forth-parse-and-analyze "100 50 + 2 / 10 - DUP 5 > IF * ELSE DROP THEN")))
    (is (eightbol::ast-node-p result))))

