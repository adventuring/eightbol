;; tests/ast-optimize-tests.lisp — AST optimization and codegen-path tests
;;
;; Run: (fiveam:run! :ast-optimize)

(in-package :eightbol/test)

(fiveam:def-suite :ast-optimize
  :description "AST-phase optimizations (unreachable, div/mul→shift, literal fold, algebra)")
(in-suite :ast-optimize)

(defun ast-first-method-statements (ast)
  "Return :statements plist of first :method in AST, or NIL."
  (let ((methods (eightbol::ast-methods ast)))
    (when methods
      (eightbol::ast-method-statements (first methods)))))

(test ast-optimize/removes-unreachable-after-goback
  "Statements after GOBACK are dropped from optimized AST."
  (let* ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Unreach-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"T\".
000110         PROCEDURE DIVISION.
000120             GOBACK.
000130             DEBUG BREAK 42.
000140         END METHOD \"T\".
000150 END OBJECT.
000160 END CLASS Unreach-Test.
")
         (raw (eightbol::parse-eightbol-string cob))
         (opt (eightbol::optimize-ast raw))
         (stmts (ast-first-method-statements opt)))
    (is (not (null stmts)))
    (is (null (member :debug-break stmts :key #'first))
        "DEBUG BREAK after GOBACK must be eliminated")))

(test ast-optimize/goback-after-invoke-self-retained
  "INVOKE Self uses .CallMethod (returns); GOBACK after INVOKE is not treated as unreachable."
  (let* ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Pierce-Style-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"T\".
000110         PROCEDURE DIVISION.
000120             INVOKE Self \"Hurt\".
000130             GOBACK.
000140         END METHOD \"T\".
000150 END OBJECT.
000160 END CLASS Pierce-Style-Test.
")
         (opt (eightbol::optimize-ast (eightbol::parse-eightbol-string cob)))
         (stmts (ast-first-method-statements opt)))
    (is (= 2 (length stmts)) "INVOKE then GOBACK")
    (is (eq :invoke (first (first stmts))))
    (is-true (getf (rest (first stmts)) :tail-call-p))
    (is (eq :goback (first (second stmts))))))

(test ast-optimize/two-sequential-invokes-only-last-is-tail
  "Two INVOKEs in one paragraph: only the last gets :tail-call-p (prefix must not jmp)."
  (let* ((raw '((:invoke :object "Current-Actor" :method "Stuck")
                (:invoke :object "Current-Course" :method "Disconnect")))
         (out (eightbol::annotate-tail-calls-in-list raw)))
    (is (eq :invoke (first (first out))))
    (is (null (getf (rest (first out)) :tail-call-p))
        "first INVOKE is not tail — control falls through to second")
    (is-true (getf (rest (second out)) :tail-call-p)
             "last INVOKE is tail")))

(test ast-optimize/singleton-call-not-tail-marked
  "Sole CALL is not annotated with :tail-call-p — 6502 must emit jsr, not jmp (unless CALL GOBACK)."
  (let* ((raw '((:call :target "Some-Routine")))
         (out (eightbol::annotate-tail-calls-in-list raw)))
    (is (eq :call (first (first out))))
    (is (null (getf (rest (first out)) :tail-call-p)))))

(test ast-optimize/call-goback-annotates-tail-call-p
  "CALL … GOBACK still sets :tail-call-p on CALL so jmp can replace jsr+redundant return."
  (let* ((raw '((:call :target "Some-Routine") (:goback)))
         (out (eightbol::annotate-tail-calls-in-list raw)))
    (is (eq :call (first (first out))))
    (is-true (getf (rest (first out)) :tail-call-p))))

(test ast-optimize/two-invokes-compile-6502-first-jsr-not-jmp
  "6502: first of two INVOKEs on Current-Actor / Current-Course uses .CallMethod; last is tail."
  (let* ((ast (eightbol::make-program-node
               "MummyCourse"
               :methods
               (list (eightbol::make-method-node
                      "Stuck"
                      :statements
                      '((:invoke :object "Current-Actor" :method "Stuck")
                        (:invoke :object "Current-Course" :method "Disconnect"))))))
         (asm (with-output-to-string (s)
                (eightbol::compile-to-assembly-with-ast-passes ast :6502 s :validate-termination nil))))
    (is (search ".CallMethod CallCharacterStuck" asm))
    (is (search "CurrentActor" asm))
    (is (search ".CallMethod CallCourseDisconnect" asm))
    (is (search "CurrentCourse" asm))
    (is (null (search "CallStuckMethod" asm)))
    (is (null (search "CallDisconnectMethod" asm)))))

(test ast-optimize/divide-multiply-power-of-two-to-compute
  "DIVIDE/MULTIPLY by constant power-of-two become :compute with shift."
  (let* ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. DivMul-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 X PIC 9 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"T\".
000110         PROCEDURE DIVISION.
000120             DIVIDE 4 INTO X.
000130             MULTIPLY 2 BY X.
000140             GOBACK.
000150         END METHOD \"T\".
000160 END OBJECT.
000170 END CLASS DivMul-Test.
")
         (opt (eightbol::optimize-ast (eightbol::parse-eightbol-string cob)))
         (stmts (ast-first-method-statements opt))
         (c1 (first stmts))
         (c2 (second stmts)))
    (is (eq :compute (first c1)))
    (is (equal (list :shift-right "X" 2) (getf (rest c1) :expression)))
    (is (eq :compute (first c2)))
    (is (equal (list :shift-left "X" 1) (getf (rest c2) :expression)))))

(test ast-optimize/algebraic-add-negative-to-subtract
  "ADD with negative integer constant rewrites to SUBTRACT (canonical form)."
  (let ((ast (eightbol::make-program-node
              "Alg-Add"
              :methods
              (list (eightbol::make-method-node
                     "T"
                     :statements
                     (list (list :compute :target "X"
                                 :expression (list :add-expr "Y" -4))))))))
    (let* ((opt (eightbol::optimize-ast ast))
           (stmt (first (ast-first-method-statements opt))))
      (is (eq :compute (first stmt)))
      (is (equal (list :subtract-expr "Y" 4) (getf (rest stmt) :expression))))))

(test ast-optimize/algebraic-subtract-negative-to-add
  "SUBTRACT with negative constant rewrites to ADD."
  (let ((ast (eightbol::make-program-node
              "Alg-Sub"
              :methods
              (list (eightbol::make-method-node
                     "T"
                     :statements
                     (list (list :compute :target "X"
                                 :expression (list :subtract-expr "Y" -2))))))))
    (let* ((opt (eightbol::optimize-ast ast))
           (stmt (first (ast-first-method-statements opt))))
      (is (eq :compute (first stmt)))
      (is (equal (list :add-expr "Y" 2) (getf (rest stmt) :expression))))))

(test ast-optimize/algebraic-multiply-zero-one-identities
  "MULTIPLY folds *0 and *1 when the constant side is an integer."
  (let ((ast (eightbol::make-program-node
              "Alg-Mul"
              :methods
              (list (eightbol::make-method-node
                     "T"
                     :statements
                     (list (list :compute :target "A"
                                 :expression (list :multiply-expr 0 42))
                           (list :compute :target "B"
                                 :expression (list :multiply-expr 1 9))))))))
    (let* ((opt (eightbol::optimize-ast ast))
           (stmts (ast-first-method-statements opt)))
      (is (eq :move (first (first stmts))))
      (is (eql 0 (getf (rest (first stmts)) :from)))
      (is (eq :move (first (second stmts))))
      (is (eql 9 (getf (rest (second stmts)) :from))))))

(test ast-optimize/algebraic-divide-by-one
  "DIVIDE by 1 rewrites to the dividend when divisor is integer 1."
  (let ((ast (eightbol::make-program-node
              "Alg-Div"
              :methods
              (list (eightbol::make-method-node
                     "T"
                     :statements
                     (list (list :compute :target "X"
                                 :expression (list :divide-expr "Counter" 1))))))))
    (let* ((opt (eightbol::optimize-ast ast))
           (stmt (first (ast-first-method-statements opt))))
      (is (eq :compute (first stmt)))
      (is (equal "Counter" (getf (rest stmt) :expression))))))

(test ast-optimize/folds-compute-literal-sum
  "COMPUTE with pure literal arithmetic becomes MOVE of folded constant."
  (let* ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Fold-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 X PIC 9 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"T\".
000110         PROCEDURE DIVISION.
000120             COMPUTE X = 1 + 2.
000130             GOBACK.
000140         END METHOD \"T\".
000150 END OBJECT.
000160 END CLASS Fold-Test.
")
         (opt (eightbol::optimize-ast (eightbol::parse-eightbol-string cob)))
         (stmts (ast-first-method-statements opt))
         (first-stmt (first stmts)))
    (is (eq :move (first first-stmt)))
    (is (eql 3 (getf (rest first-stmt) :from)))
    (is (string= "X" (getf (rest first-stmt) :to)))))

(test ast-optimize/all-cpus-compile-divmul-rewritten
  "After AST rewrite, DIVIDE/MULTIPLY compile on every supported backend."
  (let* ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. DivMulAll-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 X PIC 9 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"T\".
000110         PROCEDURE DIVISION.
000120             DIVIDE 2 INTO X.
000130             MULTIPLY 4 BY X.
000140             GOBACK.
000150         END METHOD \"T\".
000160 END OBJECT.
000170 END CLASS DivMulAll-Test.
")
         (ast (eightbol::parse-eightbol-string cob)))
    (dolist (cpu +supported-cpus+)
      (finishes
        (with-output-to-string (s)
          (eightbol::compile-to-assembly-with-ast-passes ast cpu s))
        "CPU ~a should compile AST with DIVIDE/MULTIPLY lowered to shifts" cpu))))

(test ast-optimize/parse-for-codegen-matches-optimize-ast
  "parse-eightbol-string-for-codegen equals optimize-ast ∘ parse."
  (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Eq-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 X PIC 9 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"T\".
000110         PROCEDURE DIVISION.
000120             GOBACK.
000130         END METHOD \"T\".
000140 END OBJECT.
000150 END CLASS Eq-Test.
"))
    (is (equal (eightbol::parse-eightbol-string-for-codegen cob)
               (eightbol::optimize-ast (eightbol::parse-eightbol-string cob))))))
