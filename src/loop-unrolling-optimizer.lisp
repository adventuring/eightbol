;; src/loop-unrolling-optimizer.lisp — Loop unrolling optimization for PERFORM statements
;;
;; Performs:
;; 1. Identification of unroll candidates: PERFORM with constant TIMES (2-8) or
;;    PERFORM VARYING with constant bounds yielding 2-8 iterations
;; 2. Body size heuristic: only unroll if body is < 40 complexity units
;; 3. Inlining: replace PERFORM with TIMES-COUNT copies of the body inline
;; 4. Variable substitution: in VARYING loops, replace loop counter with constants
;;
;; Rationale: Saves branch/counter overhead for small loops on limited CPUs

(in-package :eightbol)

(defun constant-expr-p (expr)
  "Check if EXPR is a constant literal (number or string)."
  (or (numberp expr) (stringp expr)))

(defun constant-expr-value (expr)
  "Get numeric value from constant expression; NIL if not numeric constant."
  (when (numberp expr)
    expr))

(defun statement-size (stmt)
  "Estimate statement complexity for unroll heuristics (0-100 scale).
   Simple statements: 1-5 units; complex: 10+; nested structures: 20+."
  (cond
    ((null stmt) 0)
    ((not (listp stmt)) 1)
    (t (let ((op (first stmt)))
         (case op
           ((:move :+ :- :set :compute) 2)
           ((:invoke :call :call-acc) 5)
           ((:string-blt) 8)
           ((:if) (let ((then-size (reduce #'+ (safe-getf (rest stmt) :then)
                                           :key #'statement-size :initial-value 0))
                        (else-size (reduce #'+ (safe-getf (rest stmt) :else)
                                          :key #'statement-size :initial-value 0)))
                    (+ 1 then-size else-size)))
           ((:perform :goto :debug-break :log-fault) 3)
           ((:exit :exit-method :exit-program :stop-run :goback) 1)
           (otherwise 1))))))

(defun loop-body-size (stmts)
  "Estimate total size of loop body."
  (reduce #'+ (ensure-list stmts) :key #'statement-size :initial-value 0))

(defun can-unroll-perform-p (perform-stmt)
  "Determine if :perform statement is unrollable. Returns iteration count (2-8) or NIL.
   
   Unrollable conditions:
   - :times with constant count (2-8) and no :until/:varying
   - :varying with constants yielding 2-8 iterations and simple UNTIL condition
   - Loop body smaller than 40 complexity units"
  (unless (and (listp perform-stmt) (eq (first perform-stmt) :perform))
    (return-from can-unroll-perform-p nil))
  (let ((times (safe-getf (rest perform-stmt) :times))
        (varying (safe-getf (rest perform-stmt) :varying))
        (until (safe-getf (rest perform-stmt) :until))
        (from (safe-getf (rest perform-stmt) :from))
        (by (safe-getf (rest perform-stmt) :by))
        (body (safe-getf (rest perform-stmt) :body)))
    
    ;; Check body size constraint first
    (when (> (loop-body-size body) 40)
      (return-from can-unroll-perform-p nil))
    
    ;; Case 1: PERFORM name TIMES count
    (when (and times (constant-expr-p times) (not varying) (not until))
      (let ((count (constant-expr-value times)))
        (when (and (numberp count) (>= count 2) (<= count 8))
          (return-from can-unroll-perform-p count))))
    
    ;; Case 2: PERFORM name VARYING id FROM start BY step UNTIL condition
    (when (and varying from by until (constant-expr-p from) (constant-expr-p by))
      (let ((start (constant-expr-value from))
            (step (constant-expr-value by))
            (cond-expr until))
        (when (and (numberp start) (numberp step) (> step 0))
          ;; Analyze UNTIL condition: (:op var limit) form
          (when (and (listp cond-expr) (>= (length cond-expr) 3))
            (let ((op (first cond-expr))
                  (lhs (second cond-expr))
                  (rhs (third cond-expr)))
              (when (and (eq lhs varying) (numberp rhs))
                (let ((count (case op
                               ((:>) (1+ (floor (- rhs start) step)))
                               ((:≥) (1+ (floor (- rhs 1 start) step)))
                               ((:< :≤) (1+ (floor (- rhs start) step)))
                               (otherwise nil))))
                  (when (and (numberp count) (>= count 2) (<= count 8))
                    (return-from can-unroll-perform-p count)))))))))
    nil))

(defun unroll-perform-times (perform-stmt times-count)
  "Unroll PERFORM TIMES: replicate body TIMES-COUNT times inline."
  (let ((body (safe-getf (rest perform-stmt) :body)))
    (loop for i from 1 to times-count
          append (ensure-list body))))

(defun substitute-varying-in-stmt (stmt var-name const-value)
  "Recursively substitute CONST-VALUE for VAR-NAME in STMT."
  (cond
    ((null stmt) nil)
    ((stringp stmt) (if (string-equal stmt var-name) const-value stmt))
    ((numberp stmt) stmt)
    ((listp stmt)
     (cons (first stmt)
           (loop for (key val) on (rest stmt) by #'cddr
                 append (list key
                             (if (listp val)
                                 (mapcar (lambda (v) (substitute-varying-in-stmt v var-name const-value))
                                        (ensure-list val))
                                 (substitute-varying-in-stmt val var-name const-value))))))
    (t stmt)))

(defun unroll-perform-varying (perform-stmt times-count)
  "Unroll PERFORM VARYING: generate TIMES-COUNT iterations with substituted values."
  (let ((varying (safe-getf (rest perform-stmt) :varying))
        (body (safe-getf (rest perform-stmt) :body))
        (start (constant-expr-value (safe-getf (rest perform-stmt) :from)))
        (step (constant-expr-value (safe-getf (rest perform-stmt) :by))))
    (loop for i from 0 below times-count
          for loop-val = (+ start (* i step))
          append (mapcar (lambda (stmt) (substitute-varying-in-stmt stmt varying loop-val))
                        (ensure-list body)))))

(defun unroll-performs-in-list (statements)
  "Unroll eligible PERFORM loops inline; recursively handle nested :if."
  (let ((result '()))
    (mapc (lambda (stmt)
            (if (and (listp stmt) (eq (first stmt) :perform))
                (let ((count (can-unroll-perform-p stmt)))
                  (if count
                      ;; Unroll the loop
                      (let ((unrolled (if (safe-getf (rest stmt) :varying)
                                        (unroll-perform-varying stmt count)
                                        (unroll-perform-times stmt count))))
                        (setf result (nconc result unrolled)))
                      ;; Keep original if not unrollable
                      (push stmt result)))
                ;; Recursively process nested :if
                (if (and (listp stmt) (eq (first stmt) :if))
                    (push (list :if
                               :condition (safe-getf (rest stmt) :condition)
                               :then (unroll-performs-in-list (safe-getf (rest stmt) :then))
                               :else (unroll-performs-in-list (safe-getf (rest stmt) :else)))
                          result)
                    (push stmt result))))
          (ensure-list statements))
    (nreverse result)))
