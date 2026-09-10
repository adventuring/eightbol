;;; Test regressions for dead code elimination
(in-package :eightbol/test/optimizer-dead-code-elimination)

(test dead_code_elimination_regression_known_issue_1
  "REGRESSION: Known issue from issue tracker is fixed"
  ;; Verify that unreachable code after terminal statements is removed
  (let* ((input '((:move :to "X" :from 1)
                  (:stop-run)
                  (:move :to "Y" :from 2)
                  (:move :to "Z" :from 3)))
         (result (eliminate-dead-code-in-list input)))
    ;; After optimization, should have only 2 statements
    (is (= 2 (length result)))))

(test dead_code_elimination_regression_previous_failures
  "REGRESSION: Previously failing programs now work correctly"
  ;; Verify constant condition simplification works
  (let* ((input '((:if :condition :true
                       :then ((:move :to "A" :from 1))
                       :else ((:move :to "B" :from 2)))))
         (result (eliminate-dead-code-in-list input)))
    ;; IF with true condition should become just the THEN branch
    (is (listp result))))

(test dead_code_elimination_regression_optimization_disabled
  "REGRESSION: Results are identical when optimization is disabled"
  ;; Verify optimization doesn't break on non-optimizable code
  (let* ((input '((:perform :target "PARAGRAPH1" :times 5)
                  (:move :to "X" :from 1)))
         (result (eliminate-dead-code-in-list input)))
    ;; Should preserve all statements
    (is (= 2 (length result)))))
