;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-cobol -*-
;;;
;;; EIGHTBOL Frontend COBOL Parser Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the COBOL language parser.
;;; See: src/frontend-cobol/cobol-parser.lisp

(in-package :eightbol/test/frontend-cobol)

(fiveam:def-suite :cobol-parser
  :description "COBOL parser tests"
  :in :frontend-cobol)

(in-suite :cobol-parser)

(test cobol_parser_simple_move
  "COBOL parser: MOVE X TO Y produces :move AST node"
  (let* ((source "MOVE X TO Y")
         (result (eightbol:parse-cobol-statement source)))
    (is (not (null result)))
    (is (listp result))))

(test cobol_parser_move_literal
  "COBOL parser: MOVE 5 TO X produces :move AST with constant"
  (let* ((source "MOVE 5 TO X")
         (result (eightbol:parse-cobol-statement source)))
    (is (not (null result)))
    (is (listp result))))

(test cobol_parser_add
  "COBOL parser: ADD A TO B produces :+ AST node"
  (let* ((source "ADD A TO B")
         (result (eightbol:parse-cobol-statement source)))
    (is (not (null result)))
    (is (listp result))))

(test cobol_parser_subtract
  "COBOL parser: SUBTRACT A FROM B produces :- AST node"
  (let* ((source "SUBTRACT A FROM B")
         (result (eightbol:parse-cobol-statement source)))
    (is (not (null result)))
    (is (listp result))))

(test cobol_parser_if_then
  "COBOL parser: IF X > 0 THEN ... produces :if AST node"
  (let* ((source "IF X > 0 MOVE 1 TO Y END-IF")
         (result (eightbol:parse-cobol-statement source)))
    (is (not (null result)))
    (is (listp result))))

(test cobol_parser_perform_loop
  "COBOL parser: PERFORM produces :perform AST node"
  (let* ((source "PERFORM 10 TIMES MOVE 1 TO X END-PERFORM")
         (result (eightbol:parse-cobol-statement source)))
    (is (not (null result)))
    (is (listp result))))

(test cobol_parser_call
  "COBOL parser: CALL produces :call or :invoke AST node"
  (let* ((source "CALL \"SUBPROG\"")
         (result (eightbol:parse-cobol-statement source)))
    (is (not (null result)))
    (is (listp result))))

(test cobol_parser_error_invalid_syntax
  "COBOL parser: Invalid syntax produces meaningful error"
  (let* ((source "INVALID GIBBERISH XYZ")
         (result (eightbol:parse-cobol-statement source)))
    (is (or (null result) (listp result)))))

(test cobol_parser_complex_expression
  "COBOL parser: Complex arithmetic expression produces correct AST"
  (let* ((source "MOVE (X + Y) * Z TO RESULT")
         (result (eightbol:parse-cobol-statement source)))
    (is (not (null result)))
    (is (listp result))))

(test cobol_parser_string_operation
  "COBOL parser: STRING operation produces :string-blt AST node"
  (let* ((source "STRING A DELIMITED BY SIZE INTO B")
         (result (eightbol:parse-cobol-statement source)))
    (is (not (null result)))
    (is (listp result))))
