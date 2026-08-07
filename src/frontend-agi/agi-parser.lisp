;; src/frontend-agi/agi-parser.lisp — YACC grammar for AGI → EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token list for AGI YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun agi-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |,| |:| + - * / = <> < <= > >=
              if else then gosub goto newroom loadlogics return
              quit assign increment decrement set reset said
              test posn controller havekey and or not
              << >>
              number string ident))))

;;; Parser action functions for AGI
(defun agi-parse-program (statements)
  "Top-level program: list of statements → AST."
  (make-program-node 'agi-program
                     :data (list (cons 'main-block statements))))

(defun agi-parse-if (cond then-stmt)
  "IF cond THEN stmt"
  (make-if-node cond (list then-stmt) '()))

(defun agi-parse-if-else (cond then-stmt else-stmt)
  "IF cond THEN stmt ELSE stmt"
  (make-if-node cond (list then-stmt) (list else-stmt)))

(defun agi-parse-gosub (routine-name)
  "GOSUB routine-name"
  (make-call-node routine-name '()))

(defun agi-parse-goto (label)
  "GOTO label"
  (make-call-node label '()))

(defun agi-parse-return ()
  "RETURN"
  (make-goback-node))

(defun agi-parse-assignment (target value)
  "target = value"
  (make-move-node value target))

(defun agi-parse-increment (var)
  "INCREMENT var"
  (make-arithmetic-node :add (make-identifier var) (make-literal 1)))

(defun agi-parse-decrement (var)
  "DECREMENT var"
  (make-arithmetic-node :subtract (make-identifier var) (make-literal 1)))

(defun agi-parse-arithmetic (op left right)
  "Arithmetic operation."
  (make-arithmetic-node op left right))

(defun agi-parse-boolean (op operands)
  "Boolean operation."
  (make-boolean-node op operands))

(defun agi-parse-comparison (op left right)
  "Comparison operation."
  (make-conditional-node op left right))

;;; YACC parser definition
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *agi-parser*
      (:start-symbol program)
      (:terminals (,@(agi-token-list)
                   number string ident))
      (:precedence ((:left or)
                    (:left and)
                    (:left not)
                    (:left = <> <= < >= >)
                    (:left + -)
                    (:left * /)
                    (:left << >>)))

      ;; Program is a list of statements
      (program
       (statement-list
        (lambda (stmts) (agi-parse-program stmts))))

      (statement-list
       (statement
        (lambda (s) (list s)))
       (statement-list statement
        (lambda (list-stmts stmt) (append list-stmts (list stmt)))))

      ;; Statements
      (statement
       (if-statement)
       (gosub-statement)
       (goto-statement)
       (return-statement)
       (assign-statement)
       (increment-statement)
       (decrement-statement)
       (set-statement))

      (if-statement
       (:IF condition :THEN statement
        (lambda (c s) (agi-parse-if c s)))
       (:IF condition :THEN statement :ELSE statement
        (lambda (c s1 s2) (agi-parse-if-else c s1 s2))))

      (gosub-statement
       (:GOSUB ident
        (lambda (id) (agi-parse-gosub id))))

      (goto-statement
       (:GOTO ident
        (lambda (id) (agi-parse-goto id))))

      (return-statement
       (:RETURN
        (lambda () (agi-parse-return))))

      (assign-statement
       (ident :EQUAL expression
        (lambda (id expr) (agi-parse-assignment (make-identifier id) expr))))

      (increment-statement
       (:INCREMENT ident
        (lambda (id) (agi-parse-increment id))))

      (decrement-statement
       (:DECREMENT ident
        (lambda (id) (agi-parse-decrement id))))

      (set-statement
       (:SET ident
        (lambda (id) (agi-parse-assignment (make-identifier id) (make-literal 1))))
       (:RESET ident
        (lambda (id) (agi-parse-assignment (make-identifier id) (make-literal 0)))))

      ;; Conditions and expressions
      (condition
       (expression
        (lambda (e) e)))

      (expression
       (primary
        (lambda (p) p))
       (expression :+ expression
        (lambda (l r) (agi-parse-arithmetic :add l r)))
       (expression :- expression
        (lambda (l r) (agi-parse-arithmetic :subtract l r)))
       (expression :* expression
        (lambda (l r) (agi-parse-arithmetic :times l r)))
       (expression :/ expression
        (lambda (l r) (agi-parse-arithmetic :divide l r)))
       (expression :<< expression
        (lambda (l r) (agi-parse-arithmetic :lshift l r)))
       (expression :>> expression
        (lambda (l r) (agi-parse-arithmetic :rshift l r)))
       (expression := expression
        (lambda (l r) (agi-parse-comparison := l r)))
       (expression :<> expression
        (lambda (l r) (agi-parse-comparison :ne l r)))
       (expression :< expression
        (lambda (l r) (agi-parse-comparison :lt l r)))
       (expression :<= expression
        (lambda (l r) (agi-parse-comparison :le l r)))
       (expression :> expression
        (lambda (l r) (agi-parse-comparison :gt l r)))
       (expression :>= expression
        (lambda (l r) (agi-parse-comparison :ge l r)))
       (expression :and expression
        (lambda (l r) (agi-parse-boolean :and (list l r))))
       (expression :or expression
        (lambda (l r) (agi-parse-boolean :or (list l r))))
       (:NOT expression
        (lambda (e) (agi-parse-boolean :not (list e))))
       (|(| expression |)|
        (lambda (e) e)))

      (primary
       (ident
        (lambda (id) (make-identifier id)))
       (number
        (lambda (n) (make-literal (parse-integer n))))
       (string
        (lambda (s) (make-literal s :type :string)))))))

(defun agi-parse-from-tokens (tokens)
  "Parse token list into AGI AST."
  (yacc:parse-with-lexer
   (let ((token-index 0))
     (lambda ()
       (if (< token-index (length tokens))
           (let ((token (elt tokens token-index)))
             (incf token-index)
             (values (car token) (cdr token)))
           (values nil nil))))
   *agi-parser*))
