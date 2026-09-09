;; tests/declarations-tests.lisp — Tests for pragmatic declarations system
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;
;; Run: (fiveam:run! :declarations-system)

(in-package :eightbol/test)

(fiveam:def-suite :declarations-system
  :description "Pragmatic Declarations — Optimize hints and temp variables")

(in-suite :declarations-system)

;;; ============================================================================
;;; Test: parse-declare-annotation
;;; ============================================================================

(fiveam:test parse-declare-empty-string
  (fiveam:is (null (eightbol::parse-declare-annotation "")))
  (fiveam:is (null (eightbol::parse-declare-annotation nil))))

(fiveam:test parse-declare-no-declaration
  (fiveam:is (null (eightbol::parse-declare-annotation "just a comment")))
  (fiveam:is (null (eightbol::parse-declare-annotation "some random text"))))

(fiveam:test parse-declare-optimize-only
  (let ((result (eightbol::parse-declare-annotation "(declare (optimize (speed 3) (space 1)))")))
    (fiveam:is (listp result))
    (fiveam:is (member 'optimize result :key #'first :test #'string-equal))
    (let ((optimize (assoc 'optimize result :test #'string-equal)))
      (fiveam:is (equal (cadr optimize) '(speed 3)))
      (fiveam:is (equal (caddr optimize) '(space 1))))))

(fiveam:test parse-declare-temp-only
  (let ((result (eightbol::parse-declare-annotation "(declare (temp TempX TempY TempZ))")))
    (fiveam:is (listp result))
    (fiveam:is (member 'temp result :key #'first :test #'string-equal))
    (let ((temp (assoc 'temp result :test #'string-equal)))
      (fiveam:is (member 'TempX temp))
      (fiveam:is (member 'TempY temp))
      (fiveam:is (member 'TempZ temp)))))

(fiveam:test parse-declare-both-forms
  (let ((result (eightbol::parse-declare-annotation 
                  "(declare (optimize (speed 3) (space 2) (safety 1)) (temp IntA IntB))")))
    (fiveam:is (listp result))
    (fiveam:is (= 2 (length result)))
    (fiveam:is (member 'optimize result :key #'first :test #'string-equal))
    (fiveam:is (member 'temp result :key #'first :test #'string-equal))))

(fiveam:test parse-declare-with-surrounding-text
  (let ((result (eightbol::parse-declare-annotation 
                  "REM This is a comment (declare (optimize (speed 3))) and more text")))
    (fiveam:is (listp result))
    (fiveam:is (member 'optimize result :key #'first :test #'string-equal))))

;;; ============================================================================
;;; Test: validate-declare-form
;;; ============================================================================

(fiveam:test validate-optimize-valid
  (fiveam:is (eightbol::validate-declare-form '(optimize (speed 3) (space 2) (safety 1))))
  (fiveam:is (eightbol::validate-declare-form '(optimize (speed 0))))
  (fiveam:is (eightbol::validate-declare-form '(optimize (space 3))))
  (fiveam:is (eightbol::validate-declare-form '(optimize (safety 0) (speed 3) (space 1)))))

(fiveam:test validate-temp-valid
  (fiveam:is (eightbol::validate-declare-form '(temp TempX)))
  (fiveam:is (eightbol::validate-declare-form '(temp TempX TempY TempZ)))
  (fiveam:is (eightbol::validate-declare-form '(temp A B C D E F))))

;;; ============================================================================
;;; Test: make-procedure-node with declarations
;;; ============================================================================

(fiveam:test make-procedure-no-declare
  (let ((node (eightbol::make-procedure-node "TestProc")))
    (fiveam:is (eq (first node) :procedure))
    (fiveam:is (null (eightbol::safe-getf (rest node) :declare)))))

(fiveam:test make-procedure-with-optimize
  (let* ((decls '((optimize (speed 3) (space 1))))
         (node (eightbol::make-procedure-node "TestProc" :declare decls)))
    (fiveam:is (eq (first node) :procedure))
    (let ((node-decls (eightbol::safe-getf (rest node) :declare)))
      (fiveam:is (listp node-decls))
      (fiveam:is (member 'optimize node-decls :key #'first :test #'string-equal)))))

(fiveam:test make-procedure-with-temp
  (let* ((decls '((temp TempX TempY)))
         (node (eightbol::make-procedure-node "TestProc" :declare decls)))
    (fiveam:is (eq (first node) :procedure))
    (let ((node-decls (eightbol::safe-getf (rest node) :declare)))
      (fiveam:is (listp node-decls))
      (fiveam:is (member 'temp node-decls :key #'first :test #'string-equal)))))

(fiveam:test make-procedure-with-both
  (let* ((decls '((optimize (speed 3) (space 1)) (temp TempX TempY)))
         (node (eightbol::make-procedure-node "TestProc" :declare decls)))
    (fiveam:is (eq (first node) :procedure))
    (let ((node-decls (eightbol::safe-getf (rest node) :declare)))
      (fiveam:is (= 2 (length node-decls)))
      (fiveam:is (member 'optimize node-decls :key #'first :test #'string-equal))
      (fiveam:is (member 'temp node-decls :key #'first :test #'string-equal)))))

;;; ============================================================================
;;; Test: make-method-node with declarations
;;; ============================================================================

(fiveam:test make-method-no-declare
  (let ((node (eightbol::make-method-node "TestMethod")))
    (fiveam:is (eq (first node) :method))
    (fiveam:is (null (eightbol::safe-getf (rest node) :declare)))))

(fiveam:test make-method-with-declare
  (let* ((decls '((optimize (speed 3) (space 1) (safety 0)) (temp LocalTemp1)))
         (node (eightbol::make-method-node "UpdateState" :declare decls)))
    (fiveam:is (eq (first node) :method))
    (let ((node-decls (eightbol::safe-getf (rest node) :declare)))
      (fiveam:is (= 2 (length node-decls))))))

;;; ============================================================================
;;; Test: make-program-node with declarations
;;; ============================================================================

(fiveam:test make-program-no-declare
  (let ((node (eightbol::make-program-node "MyApp")))
    (fiveam:is (eq (first node) :program))
    (fiveam:is (null (eightbol::safe-getf (rest node) :declare)))))

(fiveam:test make-program-with-declare
  (let* ((decls '((optimize (speed 3) (space 2) (safety 3))))
         (node (eightbol::make-program-node "MyApp" :declare decls)))
    (fiveam:is (eq (first node) :program))
    (let ((node-decls (eightbol::safe-getf (rest node) :declare)))
      (fiveam:is (listp node-decls))
      (fiveam:is (member 'optimize node-decls :key #'first :test #'string-equal)))))

;;; ============================================================================
;;; Test: Convenience wrappers
;;; ============================================================================

(fiveam:test make-procedure-with-declarations-helper
  (let ((node (eightbol::make-procedure-with-declarations "Helper"
                :statements '((dummy-stmt))
                :preceding-comment "(declare (optimize (speed 3)))")))
    (fiveam:is (eq (first node) :procedure))
    (let ((decls (eightbol::safe-getf (rest node) :declare)))
      (fiveam:is (listp decls))
      (fiveam:is (member 'optimize decls :key #'first :test #'string-equal)))))

(fiveam:test make-method-with-declarations-helper
  (let ((node (eightbol::make-method-with-declarations "Update"
                :statements '((dummy-stmt))
                :preceding-comment "(declare (optimize (speed 3) (space 1)))")))
    (fiveam:is (eq (first node) :method))
    (let ((decls (eightbol::safe-getf (rest node) :declare)))
      (fiveam:is (= 1 (length decls)))
      (fiveam:is (string-equal (first (first decls)) "optimize")))))

(fiveam:test make-program-with-declarations-helper
  (let ((node (eightbol::make-program-with-declarations "MyApp"
                :preceding-comment "(declare (optimize (speed 2) (safety 0)))")))
    (fiveam:is (eq (first node) :program))
    (let ((decls (eightbol::safe-getf (rest node) :declare)))
      (fiveam:is (listp decls))
      (fiveam:is (member 'optimize decls :key #'first :test #'string-equal)))))

;;; ============================================================================
;;; Test: Extract and clear mechanism
;;; ============================================================================

(fiveam:test extract-and-clear-declaration-empty
  (setf eightbol::*last-line-comment* nil)
  (fiveam:is (null (eightbol::extract-and-clear-declaration)))
  (fiveam:is (null eightbol::*last-line-comment*)))

(fiveam:test extract-and-clear-declaration-populated
  (setf eightbol::*last-line-comment* "(declare (optimize (speed 3)))")
  (let ((decls (eightbol::extract-and-clear-declaration)))
    (fiveam:is (listp decls))
    (fiveam:is (member 'optimize decls :key #'first :test #'string-equal))
    (fiveam:is (null eightbol::*last-line-comment*))))

(fiveam:test extract-and-clear-declaration-no-declare
  (setf eightbol::*last-line-comment* "just a comment")
  (let ((decls (eightbol::extract-and-clear-declaration)))
    (fiveam:is (null decls))
    (fiveam:is (null eightbol::*last-line-comment*))))

;;; ============================================================================
;;; Test: Integration — declarations preserved through AST
;;; ============================================================================

(fiveam:test declarations-roundtrip-optimize
  (let* ((original-decls '((optimize (speed 3) (space 1) (safety 2))))
         (proc (eightbol::make-procedure-node "TestProc" :declare original-decls))
         (retrieved (eightbol::safe-getf (rest proc) :declare)))
    (fiveam:is (equal original-decls retrieved))))

(fiveam:test declarations-roundtrip-temp
  (let* ((original-decls '((temp Var1 Var2 Var3)))
         (method (eightbol::make-method-node "TestMethod" :declare original-decls))
         (retrieved (eightbol::safe-getf (rest method) :declare)))
    (fiveam:is (equal original-decls retrieved))))

(fiveam:test declarations-roundtrip-mixed
  (let* ((original-decls '((optimize (speed 2) (safety 1)) (temp A B C)))
         (prog (eightbol::make-program-node "MyApp" :declare original-decls))
         (retrieved (eightbol::safe-getf (rest prog) :declare)))
    (fiveam:is (equal original-decls retrieved))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(defun run-declarations-tests ()
  "Run all declarations system tests."
  (fiveam:run! :declarations-system))
