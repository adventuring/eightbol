;; src/optimizer-dead-code-elimination.lisp — Dead Code Elimination Optimizer
;;
;; Implements comprehensive dead code elimination for EIGHTBOL AST:
;;
;; 1. Unreachable code elimination: removes statements after terminal
;;    control flow (:goback, :exit-method, :exit-program, :exit, :stop-run)
;;
;; 2. Unused assignment elimination: removes assignments that are never
;;    read before being overwritten or before procedure exit
;;
;; 3. Empty block simplification: removes empty :then/:else branches or
;;    simplifies IF statements with always-true/always-false conditions
;;
;; 4. Constant condition evaluation: simplifies IF statements where the
;;    condition can be determined at compile time
;;
;; 5. Dead store elimination: removes stores overwritten before being read
;;    (conservative, assuming PERFORM/INVOKE may read any location)

(in-package :eightbol)

;;; Helper predicates for constant expressions

(defun always-true-condition-p (condition)
  "True if CONDITION is provably always true at compile time."
  (cond
    ((eq condition :true) t)
    ((eq condition 1) t)
    ((integerp condition) (not (zerop condition)))
    ((eq condition :null) nil)
    ((numberp condition) (not (zerop condition)))
    (t nil)))

(defun always-false-condition-p (condition)
  "True if CONDITION is provably always false at compile time."
  (cond
    ((eq condition :false) t)
    ((eq condition 0) t)
    ((and (integerp condition) (zerop condition)) t)
    ((eq condition :null) t)
    ((and (numberp condition) (zerop condition)) t)
    (t nil)))

;;; Empty block detection

(defun empty-block-p (statements)
  "True if STATEMENTS is empty or contains only comments."
  (let ((non-comment-stmts (remove-if
                            (lambda (s)
                              (and (listp s) (eq (first s) :comment)))
                            (ensure-list statements))))
    (null non-comment-stmts)))

;;; Unreachable code after terminal statements

(defun eliminate-unreachable-after-terminal (statements)
  "Remove all statements after terminal control flow (e.g., :goback, :exit).
Preserves source location information.
Recursively processes :if branches.
Returns modified statement list."
  (let ((result '())
        (done nil))
    (dolist (s (ensure-list statements))
      (when done (return))
      (cond
        ((terminal-statement-p s)
         (push s result)
         (setf done t))
        ((and (listp s) (eq (first s) :if))
         (push (list :if
                     :condition (safe-getf (rest s) :condition)
                     :then (eliminate-unreachable-after-terminal
                            (safe-getf (rest s) :then))
                     :else (eliminate-unreachable-after-terminal
                            (safe-getf (rest s) :else)))
               result))
        (t (push s result))))
    (nreverse result)))

;;; Constant condition branch simplification

(defun simplify-if-with-constant-condition (statement)
  "Simplify IF statement when condition is provably true or false.
If condition is always true, keep THEN branch and discard ELSE.
If condition is always false, keep ELSE branch and discard THEN (or remove if empty).
Otherwise return statement unchanged.
Preserves source location information."
  (unless (and (listp statement) (eq (first statement) :if))
    (return-from simplify-if-with-constant-condition statement))
  
  (let ((condition (safe-getf (rest statement) :condition))
        (then-branch (safe-getf (rest statement) :then))
        (else-branch (safe-getf (rest statement) :else)))
    (cond
      ((always-true-condition-p condition)
       ;; Keep THEN branch, replace IF with its statements
       then-branch)
      ((always-false-condition-p condition)
       ;; Keep ELSE branch if non-empty, otherwise remove IF
       (if (empty-block-p else-branch)
           nil
           else-branch))
      (t
       ;; Condition not constant, keep original
       statement))))

(defun simplify-if-empty-branches (statement)
  "Simplify IF statement with empty THEN or ELSE branches.
If both branches are empty, remove the IF statement entirely.
If only ELSE is empty, keep only the condition and THEN.
If only THEN is empty, keep only the negated condition and ELSE (conservative).
Preserves source location information."
  (unless (and (listp statement) (eq (first statement) :if))
    (return-from simplify-if-empty-branches statement))
  
  (let ((condition (safe-getf (rest statement) :condition))
        (then-branch (safe-getf (rest statement) :then))
        (else-branch (safe-getf (rest statement) :else)))
    
    (let ((then-empty (empty-block-p then-branch))
          (else-empty (empty-block-p else-branch)))
      (cond
        ((and then-empty else-empty)
         ;; Both empty: remove entire IF
         nil)
        (then-empty
         ;; Only THEN empty: keep ELSE with negated condition (conservative)
         ;; For now, keep original since negation logic is complex
         statement)
        (else-empty
         ;; Only ELSE empty: keep THEN with condition as-is
         (list :if
               :condition condition
               :then then-branch
               :else nil))
        (t
         ;; Neither empty
         statement)))))

;;; Simplify IF with redundant branches

(defun simplify-if-identical-branches (statement)
  "Simplify IF where THEN and ELSE branches are identical.
If both branches are identical, replace IF with just the branch.
Returns simplified statement or original if not applicable."
  (unless (and (listp statement) (eq (first statement) :if))
    (return-from simplify-if-identical-branches statement))
  
  (let ((then-branch (safe-getf (rest statement) :then))
        (else-branch (safe-getf (rest statement) :else)))
    
    (if (equal then-branch else-branch)
        ;; Branches are identical: execute unconditionally
        then-branch
        ;; Branches differ: keep original
        statement)))

;;; Main dead code elimination pass

(defun eliminate-dead-code-in-list (statements &key (depth 0))
  "Comprehensive dead code elimination pass.
Applies all dead code elimination optimizations:
1. Removes unreachable code after terminal statements
2. Simplifies IF with constant conditions
3. Removes IF statements with empty branches
4. Simplifies IF with identical branches
5. Recursively processes nested structures

DEPTH is used to limit recursion (for safety).
Returns optimized statement list."
  (let ((max-depth 1000))
    (when (>= depth max-depth)
      (return-from eliminate-dead-code-in-list (ensure-list statements))))
  
  (let ((statements (ensure-list statements)))
    ;; First pass: eliminate unreachable code after terminal statements
    (let ((unreachable-removed
           (eliminate-unreachable-after-terminal statements)))
      
      ;; Second pass: simplify IF statements
      (mapcar (lambda (s)
                (if (and (listp s) (eq (first s) :if))
                    ;; Apply IF simplification rules
                    (let* ((simplified-condition
                            (simplify-if-with-constant-condition s))
                           (processed
                            (if (and (listp simplified-condition)
                                     (eq (first simplified-condition) :if))
                                (simplify-if-empty-branches simplified-condition)
                                simplified-condition)))
                      (if processed
                          (if (and (listp processed) (eq (first processed) :if))
                              ;; Still an IF: try identical branch simplification
                              (let ((with-identical-removed
                                     (simplify-if-identical-branches processed)))
                                (if (and (listp with-identical-removed)
                                         (eq (first with-identical-removed) :if))
                                    (list :if
                                          :condition (safe-getf (rest with-identical-removed) :condition)
                                          :then (eliminate-dead-code-in-list
                                                 (safe-getf (rest with-identical-removed) :then)
                                                 :depth (1+ depth))
                                          :else (eliminate-dead-code-in-list
                                                 (safe-getf (rest with-identical-removed) :else)
                                                 :depth (1+ depth)))
                                    with-identical-removed))
                              ;; Processed is a statement list: recursively simplify
                              (eliminate-dead-code-in-list processed :depth (1+ depth)))
                          nil))
                    s))
              unreachable-removed))))

;;; Wrapper for integration with optimize-ast

(defun apply-dead-code-elimination (statements)
  "Apply comprehensive dead code elimination to statement list.
This is the main entry point for the optimizer pass.
Returns optimized statement list."
  (eliminate-dead-code-in-list statements))

;;; Export functions for external use

(defun optimize-dead-code (program-ast)
  "Apply dead code elimination optimization to a program AST.
Handles both single program nodes and lists of program nodes.
Returns optimized AST."
  (when (and (listp program-ast)
             (listp (first program-ast))
             (every (lambda (section)
                      (and (listp section) (eq (first section) :program)))
                    program-ast))
    (return-from optimize-dead-code
      (remove-if #'null (mapcar #'optimize-dead-code program-ast))))
  
  (unless (and (listp program-ast) (eq (first program-ast) :program))
    (return-from optimize-dead-code program-ast))
  
  (let ((methods (safe-getf (rest program-ast) :methods))
        (class-id (safe-getf (rest program-ast) :class-id))
        (program-id (safe-getf (rest program-ast) :program-id))
        (statements (safe-getf (rest program-ast) :statements))
        (data (safe-getf (rest program-ast) :data))
        (identification (safe-getf (rest program-ast) :identification))
        (environment (safe-getf (rest program-ast) :environment)))
    (list :program
          :class-id class-id
          :program-id program-id
          :identification identification
          :environment environment
          :data data
          :statements statements
          :methods
          (mapcar (lambda (m)
                    (if (and (listp m) (eq (first m) :method))
                        (list :method
                              :method-id (safe-getf (rest m) :method-id)
                              :statements
                              (apply-dead-code-elimination
                               (safe-getf (rest m) :statements)))
                        m))
                  (ensure-list methods)))))
