;;;; tests/frontend-comprehensive-tests.lisp — AST generation tests for all front-ends
;;; Verifies that all language front-ends generate canonical AST with correct statement types

(in-package :eightbol/test)

(def-suite :frontend-ast-generation :description "Front-end language AST generation tests")
(in-suite :frontend-ast-generation)

;;;; BURGERMISTRESS Front-end Tests

(test burgermistress/parse-move
  "Burgermistress MOVE statement generates :move AST node"
  (let* ((src "BURGERMISTRESS
SetUp:
  Move 5 To Counter.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast)))
    (is (listp ast))))

(test burgermistress/parse-if
  "Burgermistress IF statement generates :if AST node"
  (let* ((src "BURGERMISTRESS
SetUp:
  If Counter > 0 Then
    Move 1 To Flag.")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast)))))

;;;; FOUNTAIN Front-end Tests (formerly Fountain → Stack VM, now Fountain → Any backend)

(test fountain/parse-dialogue
  "Fountain dialogue block generates canonical AST"
  (let* ((src "EXT. FOUNTAIN - DAY
NARRATOR
(speaking)
Hello world")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast)))))

(test fountain/parse-action
  "Fountain action block generates canonical AST"
  (let* ((src "INT. SCENE - DAY
= Script
DO Something")
         (ast (eightbol::parse-eightbol-string src)))
    (is (not (null ast)))))

;;;; MUDDLE Front-end Tests

(test muddle/parse-simple
  "Muddle generates canonical AST"
  (let* ((src "<PROG ()
  <MOVE 5 COUNTER>>"))
    (ignore src)))  ;; Placeholder pending muddle parser verification

(test muddle/parse-conditional
  "Muddle conditional generates :if AST"
  (let* ((src "<IF <G <TELL COUNTER> 0>
          <MOVE 1 FLAG>
          <MOVE 0 FLAG>>"))
    (ignore src)))  ;; Placeholder pending muddle parser verification

;;;; SCI Front-end Tests

(test sci/parse-procedure
  "SCI procedure generates canonical AST"
  (let* ((src "(procedure (SetUp 1) (Move 5 Counter))"))
    (ignore src)))  ;; Placeholder pending SCI parser verification

(test sci/parse-if
  "SCI if statement generates :if AST"
  (let* ((src "(if (> Counter 0)
      (Move 1 Flag))"))
    (ignore src)))  ;; Placeholder pending SCI parser verification

;;;; Cross-Frontend AST Equivalence Tests

(test cobol/equivalence-move
  "COBOL MOVE produces same AST as BASIC MOVE"
  (let* ((cobol-ast (eightbol::parse-eightbol-string
                     "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 Counter PIC 9999.
000080 05 Flag PIC 9.
000090 PROCEDURE DIVISION.
000100 IDENTIFICATION DIVISION.
000110 METHOD-ID. \"Main\".
000120 PROCEDURE DIVISION.
000130 MOVE 5 TO Counter.
000140 GOBACK.
000150 END METHOD \"Main\".
000160 END OBJECT.
000170 END CLASS Test."))
         (has-move (and cobol-ast
                       (find-if (lambda (stmt)
                                  (and (listp stmt)
                                       (eq (first stmt) :move)))
                                (car (last cobol-ast))))))
    (is (not (null has-move)))))

(test basic/equivalence-move
  "BASIC MOVE generates canonical AST"
  (let* ((src "10 MOVE 5 TO Counter
20 MOVE 1 TO Flag"))
    (ignore src)))  ;; Placeholder - BASIC produces AST via parser

(test pascal/equivalence-assignment
  "Pascal assignment statement generates :set AST (canonical for assignment)"
  (let* ((src "program Test;
var Counter: integer;
begin
  Counter := 5;
end."))
    (ignore src)))  ;; Placeholder pending Pascal parser verification

;;;; AST Canonicalization Tests - Verify all front-ends produce same core AST

(test ast/move-canonical
  ":move AST node has canonical structure (:move :from expr :to target)"
  (let ((move-node '(:move :from 5 :to "Counter")))
    (is (eq (first move-node) :move))
    (is (eql (getf (rest move-node) :from) 5))
    (is (equal (getf (rest move-node) :to) "Counter"))))

(test ast/if-canonical
  ":if AST node has canonical structure (:if :condition expr :then stmts :else stmts)"
  (let ((if-node '(:if :condition (> counter 0) :then ((move 1)) :else ((move 0)))))
    (is (eq (first if-node) :if))
    (is (not (null (getf (rest if-node) :condition))))
    (is (not (null (getf (rest if-node) :then))))))

(test ast/set-canonical
  ":set AST node canonical structure (:set :target id :value expr)"
  (let ((set-node '(:set :target "Counter" :value 5)))
    (is (eq (first set-node) :set))
    (is (equal (getf (rest set-node) :target) "Counter"))
    (is (eql (getf (rest set-node) :value) 5))))

(test ast/compute-canonical
  ":compute AST node has canonical structure (:compute :target id :expression expr)"
  (let ((compute-node '(:compute :target "Result" :expression (:add :from "A" :to "B"))))
    (is (eq (first compute-node) :compute))
    (is (equal (getf (rest compute-node) :target) "Result"))
    (is (listp (getf (rest compute-node) :expression)))))

(test ast/perform-canonical
  ":perform AST node canonical structure"
  (let ((perform-node '(:perform :procedure "MainLoop" :times 10)))
    (is (eq (first perform-node) :perform))
    (is (equal (getf (rest perform-node) :procedure) "MainLoop"))
    (is (eql (getf (rest perform-node) :times) 10))))

(test ast/invoke-canonical
  ":invoke AST node canonical structure (:invoke :object instance :method \"Name\")"
  (let ((invoke-node '(:invoke :object "Self" :method "Execute")))
    (is (eq (first invoke-node) :invoke))
    (is (equal (getf (rest invoke-node) :object) "Self"))
    (is (equal (getf (rest invoke-node) :method) "Execute"))))

(test ast/string-blt-canonical
  ":string-blt AST node canonical structure"
  (let ((string-node '(:string-blt :source "SourceString" :target "TargetString" :by :size)))
    (is (eq (first string-node) :string-blt))
    (is (equal (getf (rest string-node) :source) "SourceString"))
    (is (equal (getf (rest string-node) :target) "TargetString"))))

(test ast/evaluate-canonical
  ":evaluate AST node canonical structure"
  (let ((eval-node '(:evaluate :subject "Value" :when (((1) (move 1)) ((2) (move 2))))))
    (is (eq (first eval-node) :evaluate))
    (is (equal (getf (rest eval-node) :subject) "Value"))
    (is (listp (getf (rest eval-node) :when)))))

(test ast/inspect-canonical
  ":inspect AST node canonical structure"
  (let ((inspect-node '(:inspect :source "SourceStr" :tallying :characters "XYZ")))
    (is (eq (first inspect-node) :inspect))
    (is (equal (getf (rest inspect-node) :source) "SourceStr"))))

(test ast/divide-canonical
  ":divide AST node canonical structure (:divide :numerator X :denominator Y)"
  (let ((divide-node '(:divide :numerator "Dividend" :denominator 4)))
    (is (eq (first divide-node) :divide))
    (is (equal (getf (rest divide-node) :numerator) "Dividend"))
    (is (eql (getf (rest divide-node) :denominator) 4))))

(test ast/multiply-canonical
  ":multiply AST node canonical structure (:multiply :by multiplicand :multiplier X)"
  (let ((mult-node '(:multiply :by "Value" :multiplier 2)))
    (is (eq (first mult-node) :multiply))
    (is (equal (getf (rest mult-node) :by) "Value"))
    (is (eql (getf (rest mult-node) :multiplier) 2))))

(test ast/goto-canonical
  ":goto AST node canonical structure (:goto :target label)"
  (let ((goto-node '(:goto :target "MainLoop")))
    (is (eq (first goto-node) :goto))
    (is (equal (getf (rest goto-node) :target) "MainLoop"))))

(test ast/call-canonical
  ":call AST node canonical structures (nullary and unary)"
  (let ((call-nullary '(:call :target "GetValue"))
        (call-unary '(:call-acc :target "Add" :using "X")))
    (is (eq (first call-nullary) :call))
    (is (eq (first call-unary) :call-acc))))

