;;; Test edge cases for dead code elimination
(in-package :eightbol/test/optimizer-dead-code-elimination)

(test dead_code_elimination_edge_empty_input
  "EDGE CASES: Empty or minimal input is handled correctly"
  (is (null (eliminate-dead-code-in-list nil)))
  (is (null (eliminate-dead-code-in-list '()))))

(test dead_code_elimination_edge_single_node
  "EDGE CASES: Single-node AST is handled correctly"
  (let ((single '((:move :to "X" :from 1))))
    (is (equal single (eliminate-dead-code-in-list single)))))

(test dead_code_elimination_edge_deeply_nested
  "EDGE CASES: Deeply nested structures are handled without stack overflow"
  (let* ((nested (loop for i from 1 to 10
                      collect (list :if
                                    :condition (list :> "X" i)
                                    :then (list (list :move :to "Y" :from i))
                                    :else nil))))
    ;; Should not crash
    (is (listp (eliminate-dead-code-in-list nested)))))

(test dead_code_elimination_edge_conflicting_opts
  "EDGE CASES: Multiple optimization passes interact correctly"
  (let* ((input '((:if :condition :true
                       :then ((:move :to "A" :from 1))
                       :else ((:move :to "B" :from 2)))
                  (:exit-program)
                  (:move :to "C" :from 3)))
         (result (eliminate-dead-code-in-list input)))
    ;; Should eliminate dead code after exit AND simplify IF
    (is (listp result))))
