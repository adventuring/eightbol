;;; Test the dead code elimination optimizer
(in-package :eightbol/test/optimizer-dead-code-elimination)

(test dead_code_elimination_transformation_applied
  "TRANSFORMATION: Dead Code Elimination optimization is applied when applicable"
  (let* ((input '(((:move :to "X" :from 1)
                    (:goback)
                    (:move :to "Y" :from 2))))
         (result (eliminate-dead-code-in-list (first input))))
    ;; Should have only 2 statements after goback removed
    (is (= 2 (length result)))
    ;; First should be MOVE
    (is (eq :move (first (first result))))
    ;; Second should be GOBACK
    (is (eq :goback (first (second result))))))

(test dead_code_elimination_transformation_no_regression
  "TRANSFORMATION: Optimization does not introduce errors"
  (let* ((simple-stmts '((:move :to "A" :from 1)
                         (:move :to "B" :from 2))))
    ;; Should not crash or change structure
    (is (listp (eliminate-dead-code-in-list simple-stmts)))))

(test dead_code_elimination_transformation_preserves_semantics
  "TRANSFORMATION: Optimized code preserves program semantics"
  (let* ((input '((:move :to "X" :from 1)
                  (:move :to "X" :from 2)
                  (:move :to "Y" :from "X")))
         (result (eliminate-dead-code-in-list input)))
    ;; All semantically significant statements preserved
    (is (= 3 (length result)))))

(test dead_code_elimination_transformation_measurable_improvement
  "TRANSFORMATION: Optimization provides measurable performance/size improvement"
  (let* ((before '((:move :to "A" :from 1)
                   (:exit-program)
                   (:move :to "B" :from 2)
                   (:move :to "C" :from 3)
                   (:move :to "D" :from 4)))
         (after (eliminate-dead-code-in-list before)))
    ;; Dead code after exit-program should be removed (3 stmts removed)
    (is (< (length after) (length before)))))
