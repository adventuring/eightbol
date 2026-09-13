;; src/basic-parser.lisp — YACC grammar for Dartmouth BASIC → EIGHTBOL canonical AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token list for BASIC YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun basic-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |:| |,| + - * / = <> < <= > >=
              let for to step next if then else goto gosub return
              log fault stop method on class procedure end
              while until do library service
              and or not xor
              null self of
              print input dialogue $
              number string ident))))

;;; Parser action functions for BASIC

(defun basic-parse-program (statements)
  "Top-level program node from list of statement nodes."
  (let* ((class-id (or *current-basic-class* "Program"))
         (methods '())
         (procedures '())
         (main-statements '()))
    (dolist (item statements)
      (cond
        ((and (listp item) (eq (first item) :procedure))
         (push item procedures))
        ((and (listp item) (eq (first item) :line))
         (push (third item) main-statements))
        ((eq (first item) :error)
         (push item main-statements))
        (t (push item main-statements))))
    (make-program-node
     class-id
     :methods (append (nreverse methods) (nreverse procedures))
     :data (when main-statements
             (list (cons 'main-block (nreverse main-statements)))))))

(defun basic-parse-statement-line (lineno label stmt)
  "Parse a line with optional line number and label."
  (list :line lineno label stmt))

(defun basic-parse-let (target expr)
  "LET target = expr — emits :move"
  (make-move-node expr target))

(defun basic-parse-set (target value)
  "SET target TO value — emits :move (same as LET)"
  (make-move-node value target))

(defun basic-parse-if-then (cond then-stmt)
  "IF condition THEN statement"
  (make-if-node cond (list then-stmt) '()))

(defun basic-parse-if-then-else (cond then-stmt else-stmt)
  "IF condition THEN statement ELSE statement"
  (make-if-node cond (list then-stmt) (list else-stmt)))

(defun basic-parse-for (var start end step)
  "FOR var = start TO end [STEP step]"
  (make-perform-node
   (format nil "FOR-~A" var)
   :varying var
   :from start
   :by (or step 1)
   :until (list :gt (make-identifier var) end)))

(defun basic-parse-next (var)
  "NEXT [var] - loop variable is optional"
  (make-goback-node))

(defun basic-parse-while (cond stmt)
  "WHILE condition statement"
  (make-if-node cond (list stmt) '()))

(defun basic-parse-until (cond stmt)
  "UNTIL condition statement"
  (make-if-node (list :not cond) (list stmt) '()))

(defun basic-parse-log-fault (code)
  "LOG FAULT \"CODE\""
  (make-log-fault-node code))

(defun basic-parse-stop (code)
  "STOP \"CODE\""
  (make-stop-run-node code))

(defun basic-parse-goto (target)
  "GOTO target"
  (list :goto :target target))

(defun basic-parse-gosub (target)
  "GOSUB target"
  (list :perform :procedure target))

(defun basic-parse-return ()
  "RETURN — emits :goback"
  (make-goback-node))

(defun basic-parse-procedure (name)
  "PROCEDURE \"Name\""
  (make-procedure-node name))

(defun basic-parse-end ()
  "END — emits :goback"
  (make-goback-node))

(defun basic-parse-move (from to)
  "MOVE from TO to"
  (make-move-node from to))

(defun basic-parse-expression-or (e1 e2)
  (list :or e1 e2))

(defun basic-parse-expression-and (e1 e2)
  (list :and e1 e2))

(defun basic-parse-expression-not (expr)
  (list :not expr))

(defun basic-parse-expression-rel (e1 op e2)
  (ecase op
    (:equal (list :eq e1 e2))
    (:ne (list :ne e1 e2))
    (:lt (list :lt e1 e2))
    (:le (list :le e1 e2))
    (:gt (list :gt e1 e2))
    (:ge (list :ge e1 e2))))

(defun basic-parse-expression-add (e1 e2)
  (list :+ e1 e2))

(defun basic-parse-expression-sub (e1 e2)
  (list :- e1 e2))

(defun basic-parse-expression-mul (e1 e2)
  (list :× e1 e2))

(defun basic-parse-expression-div (e1 e2)
  (list :÷ e1 e2))

(defun basic-parse-expression-bit-and (e1 e2)
  (list :∧ e1 e2))

(defun basic-parse-expression-bit-or (e1 e2)
  (list :∨ e1 e2))

(defun basic-parse-expression-bit-xor (e1 e2)
  (list :⊻ e1 e2))

(defun basic-parse-expression-bit-not (e1)
  (list :¬ e1))

(defun basic-parse-subscript (array index)
  "Array subscript A(index) -> (:subscript A index)"
  (list :subscript array index))

(defun basic-parse-refmod (base start length)
  "String reference A$(start,length) -> (:refmod :base A$ :start start :length length)"
  (list :refmod :base base :start start :length length))

(defun basic-parse-of (slot object)
  "slot OF object -> (:of slot object)"
  (list :of slot object))

(defun basic-parse-address-of (var)
  "VARPTR(var) -> (:address-of var)"
  (list :address-of var))

(defun basic-parse-pcopy (src dest len)
  "PCOPY src, dest, len -> (:string-blt :from src :to dest :length len)"
  (list :string-blt :from src :to dest :length len))

(defun basic-parse-null-test (expr)
  "NULL(expr) -> (:null expr) test"
  (list :null expr))

(defun basic-parse-paren (expr)
  expr)

(defun basic-parse-identifier (name)
  (make-identifier name))

(defun basic-parse-number (n)
  (make-literal-number n))

(defun basic-parse-string (s)
  (make-literal-string s))

(defun basic-parse-self ()
  (make-self))

(defun basic-parse-null ()
  (make-null))

(defun basic-parse-print (expressions)
  "PRINT expr1, expr2, ..."
  (make-print-node expressions))

(defun basic-parse-input (variables)
  "INPUT var1, var2, ..."
  (make-input-node variables))

(defun basic-parse-dialogue (character text)
  "DIALOGUE $(Character)\"text\""
  (make-dialogue-node :speaker character :text text))

;;; YACC grammar definition for BASIC
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *basic-parser*
      (:start-symbol basic-program)
      (:terminals (,@(basic-token-list)
                   number string ident))
      (:precedence ((:left or)
                    (:left and)
                    (:left not)
                    (:left = <= < > >=)
                    (:left + -)
                    (:left * / xor)))
      (:muffle-conflicts :some)

      ;; Program is a list of lines/statements
      (basic-program
       (statement-list
        #'basic-parse-program))

      (statement-list
       (statement
        (lambda (s) (list s)))
       (statement-list statement
                       (lambda (list stmt) (append list (list stmt)))))

      ;; Each statement can have line number and label
      (statement
       (lineno-opt label-opt colon stmt
                   #'basic-parse-statement-line))

      (lineno-opt
       (number (lambda (n) n))
       (() (constantly nil)))

      (label-opt
       (string (lambda (s) s))
       (() (constantly nil)))

      (colon
       :COLON)

      (stmt
       (let-stmt)
       (if-stmt)
       (for-stmt)
       (next-stmt)
       (while-stmt)
       (until-stmt)
       (log-fault-stmt)
       (stop-stmt)
       (goto-stmt)
       (gosub-stmt)
       (return-stmt)
       (procedure-def)
       (end-stmt)
       (move-stmt)
       (set-stmt)
       (print-stmt)
       (input-stmt)
       (dialogue-stmt)
       (expression-stmt))

      ;; LET target = expression
      (let-stmt
       (:LET ident :EQUAL expression
             #'basic-parse-let))

      ;; IF condition THEN statement [ELSE statement]
      (if-stmt
       (:IF condition :THEN stmt
            #'basic-parse-if-then)
       (:IF condition :THEN stmt :ELSE stmt
            #'basic-parse-if-then-else))

      ;; FOR var = start TO end [STEP step]
      (for-stmt
       (:FOR ident :EQUAL expression :TO expression step-opt
             #'basic-parse-for))

      (step-opt
       (:STEP expression (lambda (e) e))
       (() (constantly nil)))

      ;; NEXT [var]
      (next-stmt
       (:NEXT ident-opt
              #'basic-parse-next))

      (ident-opt
       (ident (lambda (i) i))
       (() (constantly nil)))

      ;; WHILE condition statement
      (while-stmt
       (:WHILE condition stmt
               #'basic-parse-while))

      ;; UNTIL condition statement
      (until-stmt
       (:UNTIL condition stmt
               #'basic-parse-until))

      ;; LOG FAULT "code"
      (log-fault-stmt
       (:LOG :FAULT string
             #'basic-parse-log-fault))

      ;; STOP "code"
      (stop-stmt
       (:STOP string
              #'basic-parse-stop))

      ;; GOTO target (identifier or string label)
      (goto-stmt
       (:GOTO ident
              #'basic-parse-goto))

      ;; GOSUB target
      (gosub-stmt
       (:GOSUB ident
               #'basic-parse-gosub))

      ;; RETURN
      (return-stmt
       (:RETURN
        #'basic-parse-return))

      ;; PROCEDURE "Name"
      (procedure-def
       (:PROCEDURE string
                   #'basic-parse-procedure))

      ;; END
      (end-stmt
       (:END
        #'basic-parse-end))

      ;; MOVE from TO to
      (move-stmt
       (:MOVE expression :TO ident
              #'basic-parse-move))

      ;; SET target TO value
      (set-stmt
       (:SET ident :TO expression
             #'basic-parse-set))

      ;; PRINT expr1, expr2, ...
      (print-stmt
       (:PRINT expr-list
               #'basic-parse-print)
       (:PRINT
        (lambda () (basic-parse-print '()))))

      ;; INPUT var1, var2, ...
      (input-stmt
       (:INPUT ident-list
               #'basic-parse-input))

      ;; DIALOGUE $(Character)"text"
      (dialogue-stmt
       (:DIALOGUE :DOLLAR ident string
                  #'basic-parse-dialogue))

      ;; expression list for PRINT
      (expr-list
       (expression
        (lambda (e) (list e)))
       (expr-list :COMMA expression
                  (lambda (list _ e) (append list (list e)))))

      ;; identifier list for INPUT
      (ident-list
       (ident
        (lambda (i) (list i)))
       (ident-list :COMMA ident
                   (lambda (list _ i) (append list (list i)))))

      ;; expression statement (standalone expression)
      (expression-stmt
       (expression))

      ;; Conditions and expressions
      (condition
       (expression))

      (expression
       (or-expr))

      (or-expr
       (and-expr)
       (or-expr :OR and-expr
                #'basic-parse-expression-or))

      (and-expr
       (not-expr)
       (and-expr :AND not-expr
                 #'basic-parse-expression-and))

      (not-expr
       (rel-expr)
       (:NOT not-expr
             #'basic-parse-expression-not))

      (rel-expr
       (add-expr)
       (add-expr :EQUAL add-expr
                 (lambda (e1 _ e2) (basic-parse-expression-rel e1 :equal e2)))
       (add-expr :NE add-expr
                 (lambda (e1 _ e2) (basic-parse-expression-rel e1 :ne e2)))
       (add-expr :LT add-expr
                 (lambda (e1 _ e2) (basic-parse-expression-rel e1 :lt e2)))
       (add-expr :LE add-expr
                 (lambda (e1 _ e2) (basic-parse-expression-rel e1 :le e2)))
       (add-expr :GT add-expr
                 (lambda (e1 _ e2) (basic-parse-expression-rel e1 :gt e2)))
       (add-expr :GE add-expr
                 (lambda (e1 _ e2) (basic-parse-expression-rel e1 :ge e2))))

      (add-expr
       (mul-expr)
       (add-expr :PLUS mul-expr
                 #'basic-parse-expression-add)
       (add-expr :MINUS mul-expr
                 #'basic-parse-expression-sub))

      (mul-expr
       (xor-expr)
       (mul-expr :TIMES xor-expr
                 #'basic-parse-expression-mul)
       (mul-expr :DIVIDE xor-expr
                 #'basic-parse-expression-div))

      (xor-expr
       (primary)
       (xor-expr :XOR primary
                 #'basic-parse-expression-bit-xor))

      (primary
       :NUMBER #'basic-parse-number
       :STRING #'basic-parse-string
       :IDENT #'basic-parse-identifier
       (:SELF #'basic-parse-self)
       (:NULL #'basic-parse-null)
       (:LPAREN expression :RPAREN #'basic-parse-paren)))))

(defparameter *current-basic-class* "Program")

;;; Entry point functions
(defun basic-lex (source)
  "Lex BASIC source string into token list for YACC."
  (let ((tokens (basic-lex-source source)))
    (mapcar (lambda (tok)
              (list (first tok) (second tok)))
            tokens)))

(defun parse-basic (source)
  "Parse BASIC source string and return AST."
  (let ((*current-basic-class* "Program")
        (*yacc-debug* nil))
    (let ((tokens (basic-lex-source source)))
      (yacc:parse-with-lexer
       *basic-parser*
       (lambda ()
         (when tokens
           (let ((tok (pop tokens)))
             (when tok
               (values (first tok) (second tok))))))))))

(defun basic-make-parser ()
  "Return the BASIC YACC parser function."
  *basic-parser*)
