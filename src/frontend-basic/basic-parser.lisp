;; src/basic-parser.lisp — YACC grammar for Dartmouth BASIC → EIGHTBOL AST
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
              and or not
              print input dialogue $
              number string ident))))

;;; Parser action functions for BASIC

  (defun basic-parse-program (statements)
    "Top-level program node from list of statement nodes.
STATEMENTS is a list where each element is either:
  - (:line lineno label stmt)
  - (:error :code line-text)
The line number and label are used to create BASIC_Lnnnnnn labels."
    (let* ((class-id *current-class*)
           (methods '())
           (procedures '())
           (main-statements '()))
      (dolist (item statements)
        (cond
          ((eq (first item) :method)
           (push item methods))
          ((eq (first item) :procedure)
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
               (list (cons 'main-block main-statements))))))

  (defun basic-parse-statement-line (lineno label stmt)
    "Parse a line with optional line number and label."
    (list :line lineno label stmt))

  (defun basic-parse-error (line-text)
    "Create error node for invalid syntax."
    (make-error-node line-text))
  
  (defun basic-parse-let (target expr)
    "LET target = expr"
    (make-move-node expr target))

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
     :until (make-conditional-gt (make-identifier var) end)))

  (defun basic-parse-next (var)
    "NEXT [var] - loop variable is optional"
    (make-goback-node))  ; Simplified; NEXT just continues the loop

  (defun basic-parse-while (cond stmt)
    "WHILE condition statement"
    (make-if-node cond (list stmt) '()))

  (defun basic-parse-until (cond stmt)
    "UNTIL condition statement"
    (make-if-node (make-conditional-not cond) (list stmt) '()))

  (defun basic-parse-do (stmts while-cond)
    "DO : statements... : NEXT WHILE condition"
    (let ((label (format nil "DO-~A" (gensym))))
      (make-perform-node label
                         :until (make-conditional-not while-cond))))

  (defun basic-parse-do-until (stmts until-cond)
    "DO : statements... : NEXT UNTIL condition"
    (let ((label (format nil "DO-~A" (gensym))))
      (make-perform-node label
                         :until until-cond)))

  (defun basic-parse-log-fault (code)
    "LOG FAULT \"CODE\""
    (make-log-fault-node code))

  (defun basic-parse-stop (code)
    "STOP \"CODE\""
    (make-stop-run-node code))

  (defun basic-parse-goto (target)
    "GOTO target"
    (list :goto :target target))

  (defun basic-parse-goto-label (label)
    "GOTO \"label\" — local string label."
    (list :goto :target label))

  (defun basic-parse-gosub (target)
    "GOSUB target"
    (list :perform :procedure target))

  (defun basic-parse-gosub-local (label)
    "GOSUB \"label\" — local string label subroutine."
    (list :perform :procedure label))

  (defun basic-parse-gosub-library (name)
    "GOSUB \"name\" IN LIBRARY — resident library call (jsr Lib.Name)."
    (list :call :target name :bank nil :library t))

  (defun basic-parse-gosub-method (name object)
    "GOSUB \"method\" ON object — OOPS method invocation."
    (make-invoke-node object name))

  (defun basic-parse-gosub-service (name)
    "GOSUB \"name\" SERVICE — service-dispatch call, bank resolved at link time."
    (list :call :service name :bank nil))

  (defun basic-parse-return ()
    "RETURN"
    (make-goback-node))

  (defun basic-parse-method (name class)
    "METHOD \"Name\" ON \"Class\""
    (make-method-node name))

  (defun basic-parse-procedure (name)
    "PROCEDURE \"Name\""
    (make-procedure-node name))

  (defun basic-parse-class (name)
    "CLASS \"Name\""
    (setf *current-class* name)
    (make-program-node name))

  (defun basic-parse-end ()
    "END"
    (make-goback-node))

  (defun basic-parse-invoke (obj method)
    "INVOKE obj \"Method\""
    (make-invoke-node obj method))

  (defun basic-parse-call (target)
    "CALL target"
    (make-call-node target))

  (defun basic-parse-move (from to)
    "MOVE from TO to"
    (make-move-node from to))

  (defun basic-parse-set (target value)
    "SET target TO value"
    (make-set-node target value))

  (defun basic-parse-expression-or (e1 e2)
    (make-conditional-or e1 e2))

  (defun basic-parse-expression-and (e1 e2)
    (make-conditional-and e1 e2))

  (defun basic-parse-expression-not (expr)
    (make-conditional-not expr))

  (defun basic-parse-expression-rel (e1 op e2)
    (ecase op
      (:equal (make-conditional-eq e1 e2))
      (:ne (make-conditional-ne e1 e2))
      (:lt (make-conditional-lt e1 e2))
      (:le (make-conditional-le e1 e2))
      (:gt (make-conditional-gt e1 e2))
      (:ge (make-conditional-ge e1 e2))))

  (defun basic-parse-expression-add (e1 e2)
    (make-expression-add e1 e2))

  (defun basic-parse-expression-sub (e1 e2)
    (make-expression-subtract e1 e2))

  (defun basic-parse-expression-mul (e1 e2)
    (make-expression-multiply e1 e2))

  (defun basic-parse-expression-div (e1 e2)
    (make-expression-divide e1 e2))

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

  (defun basic-parse-assembly-entry (label)
    (make-assembly-entry-node label))

   (defun basic-parse-copy (name)
     (make-copy-node name))

   (defun basic-parse-print (expressions)
     "PRINT expr1, expr2, ..."
     (make-print-node expressions))

   (defun basic-parse-input (variables)
     "INPUT var1, var2, ..."
     (make-input-node variables))

   (defun basic-parse-dialogue (character text)
     "DIALOGUE $(Character)\"text\""
     (make-dialogue-node character text))

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
              (:left = <= < <= > >=)
              (:left + -)
              (:left * /)))
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
                   #'basic-parse-statement-line)
       (error-line
        #'basic-parse-error))

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
        (do-stmt)
        (log-fault-stmt)
        (stop-stmt)
        (goto-stmt)
        (gosub-stmt)
        (return-stmt)
        (method-def)
        (procedure-def)
        (class-def)
        (end-stmt)
        (invoke-stmt)
        (call-stmt)
        (move-stmt)
        (set-stmt)
        (print-stmt)
        (input-stmt)
        (dialogue-stmt)
        (assembly-entry-stmt)
        (copy-stmt)
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

      ;; DO : statements... : NEXT WHILE condition
      (do-stmt
          (:DO colon stmt-list :NEXT :WHILE condition
            #'basic-parse-do)
        (:DO colon stmt-list :NEXT :UNTIL condition
          #'basic-parse-do-until))

      (stmt-list
       (stmt
        (lambda (s) (list s)))
       (stmt-list colon stmt
                  (lambda (list s) (append list (list s)))))

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
       (:GOTO string
              #'basic-parse-goto)
       (:GOTO ident
              #'basic-parse-goto))

      ;; GOSUB target (local label or identifier)
      (gosub-stmt
       (:GOSUB string
               #'basic-parse-gosub-local)
       (:GOSUB ident
               #'basic-parse-gosub)
       (:GOSUB string :IN :LIBRARY
               #'basic-parse-gosub-library)
       (:GOSUB string :ON expression
               #'basic-parse-gosub-method)
       (:GOSUB string :SERVICE
               #'basic-parse-gosub-service))

      ;; RETURN
      (return-stmt
       (:RETURN
         #'basic-parse-return))

      ;; METHOD "Name" ON "Class"
      (method-def
       (:METHOD string :ON string
         #'basic-parse-method))

      ;; PROCEDURE "Name"
      (procedure-def
       (:PROCEDURE string
                   #'basic-parse-procedure))

      ;; CLASS "Name"
      (class-def
       (:CLASS string
               #'basic-parse-class))

      ;; END
      (end-stmt
       (:END
        #'basic-parse-end))

      ;; INVOKE obj "Method"
      (invoke-stmt
       (:INVOKE ident string
                #'basic-parse-invoke))

      ;; CALL target
      (call-stmt
       (:CALL ident
              #'basic-parse-call))

      ;; MOVE from TO to
      (move-stmt
       (:MOVE expression :TO ident
              #'basic-parse-move))

      ;; SET target TO value
      (set-stmt
       (:SET ident :TO expression
             #'basic-parse-set))

      ;; ASSEMBLY ENTRY "label"
      (assembly-entry-stmt
       (:ASSEMBLY :ENTRY string
                  #'basic-parse-assembly-entry))

       ;; COPY "name"
       (copy-stmt
        (:COPY string
               #'basic-parse-copy))

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
       (expression))  ; In BASIC, conditions are expressions

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
       (primary)
       (mul-expr :TIMES primary
                 #'basic-parse-expression-mul)
       (mul-expr :DIVIDE primary
                 #'basic-parse-expression-div))

      (primary
       :NUMBER #'basic-parse-number
       :STRING #'basic-parse-string
       :IDENT #'basic-parse-identifier
       (:SELF #'basic-parse-self)
       (:NULL #'basic-parse-null)
       (:LPAREN expression :RPAREN #'basic-parse-paren))

      ;; Error recovery: consume tokens until newline/colon
      (error-line
       (() (constantly ""))
       (error-line anything (lambda (_ a) a))))))

;;; Entry point functions
(defun basic-lex (source)
  "Lex BASIC source string into token list for YACC."
  (let ((tokens (basic-lex-source source)))
    (mapcar (lambda (tok)
              (list (first tok) (second tok)))
            tokens)))

(defun parse-basic (source)
  "Parse BASIC source string and return AST."
  (let ((*basic-current-class* "Program")
        (*yacc-debug* nil))
    (yacc:parse-with-lexer
     *basic-parser*
     (lambda ()
       (let ((tokens (basic-lex-source source)))
         (lambda ()
           (when tokens
             (pop tokens))))))))

(defun basic-make-parser ()
  "Return the BASIC YACC parser function."
  *basic-parser*)

