;; tests/parser-structure-tests.lisp — Unit tests for parser output structures
;;
;; Verifies that each parser production produces the expected AST shape.
;; Uses parse-procedure-stmts from eightbol-tests. Covers condition types,
;; expression types, and statement variants not fully exercised elsewhere.
;;
;; To run: (asdf:test-system :eightbol)
;;        — or — (fiveam:run! :parser-structure)

(in-package :eightbol/test)

(fiveam:def-suite :parser-structure
  :description "Parser output structure tests for AST node shapes")
(in-suite :parser-structure)

;;;; Condition structure tests

(test parser-structure/condition-and
  "IF cond1 AND cond2 produces (:and cond1 cond2) in condition."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt
                "IF HP IS EQUAL TO 0 AND HP IS LESS THAN 10 THEN MOVE 1 TO HP. END-IF.")))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (if-stmt (find :if (eightbol::ast-method-statements think) :key #'first))
         (cond (getf (rest if-stmt) :condition)))
    (is (not (null cond)))
    (is (eq :and (first cond)))
    (is (= 3 (length cond)))))

(test parser-structure/condition-or
  "IF cond1 OR cond2 produces (:or cond1 cond2) in condition."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt
                "IF HP IS EQUAL TO 0 OR HP IS GREATER THAN 10 THEN MOVE 1 TO HP. END-IF.")))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (if-stmt (find :if (eightbol::ast-method-statements think) :key #'first))
         (cond (getf (rest if-stmt) :condition)))
    (is (not (null cond)))
    (is (eq :or (first cond)))
    (is (= 3 (length cond)))))

(test parser-structure/condition-is-null
  "IF ptr IS NULL produces (= expr :null) in condition."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt
                "IF HP IS NULL THEN MOVE 1 TO HP. END-IF.")))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (if-stmt (find :if (eightbol::ast-method-statements think) :key #'first))
         (cond (getf (rest if-stmt) :condition)))
    (is (not (null cond)))
    (is (member :null cond :test #'equal))))

(test parser-structure/condition-is-not-null
  "IF ptr IS NOT NULL produces (:not (= expr :null)) in condition."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt
                "IF HP IS NOT NULL THEN MOVE 1 TO HP. END-IF.")))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (if-stmt (find :if (eightbol::ast-method-statements think) :key #'first))
         (cond (getf (rest if-stmt) :condition)))
    (is (not (null cond)))
    (is (eq :not (first cond)))
    (is (listp (second cond)))))

(test parser-structure/condition-relation-equal
  "IF x IS EQUAL TO y produces (= lhs rhs) in condition."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "IF HP IS EQUAL TO 0 THEN MOVE 1 TO HP. END-IF.")))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (if-stmt (find :if (eightbol::ast-method-statements think) :key #'first))
         (cond (getf (rest if-stmt) :condition)))
    (is (not (null cond)))
    (is (or (eq '= (first cond)) (eq 'equal (first cond)) (member (first cond) '(= equal))))))

(test parser-structure/condition-relation-less
  "IF x IS LESS THAN y produces (< lhs rhs) in condition."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt
                "IF HP IS LESS THAN 10 THEN MOVE 1 TO HP. END-IF.")))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (if-stmt (find :if (eightbol::ast-method-statements think) :key #'first))
         (cond (getf (rest if-stmt) :condition)))
    (is (not (null cond)))
    (is (or (eq '< (first cond)) (eq 'less (first cond))
            (member (first cond) '(< less) :test #'equal)))))

(test parser-structure/condition-relation-greater
  "IF x IS GREATER THAN y produces (> lhs rhs) in condition."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt
                "IF HP IS GREATER THAN 0 THEN MOVE 1 TO HP. END-IF.")))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (cond (getf (rest (find :if (eightbol::ast-method-statements think)
                                 :key #'first))
                     :condition)))
    (is (not (null cond)))
    (is (or (eq '> (first cond)) (eq 'greater (first cond))
            (member (first cond) '(> greater) :test #'equal)))))

;;;; Expression structure tests

(test parser-structure/subtract-giving-structure
  "SUBTRACT x FROM y GIVING z produces (:subtract :from :from-target :giving)."
  (let* ((stmts (parse-procedure-stmts
                 "000200             SUBTRACT 1 FROM A GIVING C."))
         (sub (find :subtract stmts :key #'first)))
    (is (not (null sub)))
    (is (eql 1 (getf (rest sub) :from)))
    (is (string= "A" (getf (rest sub) :from-target)))
    (is (string= "C" (getf (rest sub) :giving)))))

(test parser-structure/invoke-returning-structure
  "INVOKE obj method RETURNING var produces (:invoke :object :method :returning)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "INVOKE Self \"Kill\" RETURNING HP.")))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (inv (find :invoke (eightbol::ast-method-statements think) :key #'first)))
    (is (not (null inv)))
    (is (string= "Kill" (getf (rest inv) :method)))
    (is (string= "HP" (getf (rest inv) :returning)))))

(test parser-structure/perform-times-structure
  "PERFORM proc TIMES n produces (:perform :procedure :times)."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             PERFORM Loop TIMES 5.
000130             GOBACK.
000140 Loop.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character.")
         (ast (eightbol::parse-eightbol-string src))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (perf (find :perform (eightbol::ast-method-statements think) :key #'first)))
    (is (not (null perf)))
    (is (string= "Loop" (format nil "~a" (getf (rest perf) :procedure))))
    (is (eql 5 (getf (rest perf) :times)))))

(test parser-structure/perform-until-structure
  "PERFORM proc UNTIL cond produces (:perform :procedure :until)."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             PERFORM Loop UNTIL HP IS EQUAL TO 0.
000130             GOBACK.
000140 Loop.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character.")
         (ast (eightbol::parse-eightbol-string src))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (perf (find :perform (eightbol::ast-method-statements think) :key #'first)))
    (is (not (null perf)))
    (is (string= "Loop" (format nil "~a" (getf (rest perf) :procedure))))
    (is (not (null (getf (rest perf) :until))))))

(test parser-structure/add-expr-in-compute
  "COMPUTE x = a + b produces :add-expr in expression."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = A + B."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :add-expr (first expr))))
      (is (= 3 (length expr))))))

(test parser-structure/subtract-expr-in-compute
  "COMPUTE x = a - b produces :subtract-expr in expression."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = A - B."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :subtract-expr (first expr)))))))

(test parser-structure/multiply-expr-in-compute
  "COMPUTE x = a * b produces :multiply-expr in expression."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = A * 2."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :multiply-expr (first expr)))))))

(test parser-structure/divide-expr-in-compute
  "COMPUTE x = a / b produces :divide-expr in expression."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = A / 2."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :divide-expr (first expr)))))))

(test parser-structure/if-else-structure
  "IF cond THEN stmts ELSE stmts produces :else in AST."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt
                "IF HP IS EQUAL TO 0 THEN MOVE 1 TO HP. ELSE MOVE 0 TO HP. END-IF.")))
         (think (find "Think" (eightbol::ast-methods ast)
                      :key #'eightbol::ast-method-name :test #'string=))
         (if-stmt (find :if (eightbol::ast-method-statements think) :key #'first)))
    (is (not (null (getf (rest if-stmt) :else))))
    (is (find :move (getf (rest if-stmt) :else) :key #'first))))

(test parser-structure/call-target-bank-structure
  "CALL target IN BANK bank produces (:call :target :bank) without :service."
  (let* ((stmts (parse-procedure-stmts
                 "000200             CALL Foo IN BANK BankCode."))
         (call (find :call stmts :key #'first)))
    (is (not (null call)))
    (is (not (null (getf (rest call) :target))))
    (is (not (null (getf (rest call) :bank))))
    (is (null (getf (rest call) :service)))))
