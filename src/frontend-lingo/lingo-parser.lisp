;; src/lingo-parser.lisp -- YACC parser for Lingo source
(in-package :eightbol)

;; Lingo YACC grammar with AST node construction

(eval
 `(yacc:define-parser *lingo-parser*
    (:muffle-conflicts :some)
    (:start-symbol program)

  ;; Program Root
  (program
    (module-definition+
      (lambda (modules)
        (make-program-node :class-id "Lingo Program" :methods (collect-methods modules)))))

  ;; Module/Function Definitions
  (module-definition+
    (module-definition)
    (module-definition+ module-definition))

  ;; Method Definitions
  (method-definition
    (method-signature method-body
      (lambda (name args body)
        (make-method-node name :statements body))))

  ;; Procedure Definitions
  (procedure-definition
    (procedure-signature procedure-body
      (lambda (name body)
        (make-procedure-node name :statements body))))

  ;; Statement List
  (statement-list
    ((constantly nil))
    (statement (lambda (s) (list s)))
    (statement-list statement
                   (lambda (prefix suffix) (append prefix (list suffix)))))

  ;; Statements
  (statement
    (assignment)
    (call-stmt)
    (loop-stmt)
    (if-stmt)
    (return-stmt)
    (goto-stmt)
    (expression))

  ;; Assignments
  (assignment
    (set-stmt)
    (move-stmt)
    (property-stmt))

  ;; Set Statement
  (set-stmt
    (SET ident :TO expression
      (lambda (target value)
        (make-move-node target value))))

  ;; Move Statement
  (move-stmt
    (MOVE expression :TO ident
      (lambda (value target)
        (make-move-node value target))))

  ;; Property Statement (Lingo specific)
  (property-stmt
    (PROPERTY identifiers
      (lambda (idents)
        (make-move-node (make-literal-string "properties") (make-qualified-identifier (first idents) "self")))))

  ;; Call Statement
  (call-stmt
    (CALL expression LPAREN arg-list? RPAREN
      (lambda (target args)
        (if (assoc "library" args)
            (make-call-node target :bank nil :library t)
            (make-call-node target :args (or args nil))))))

  ;; Loop Statements
  (loop-stmt
    (FOR ident :IN expression :TO expression :STEP expression DO statement-list END
      (lambda (var start end step stmts)
        (make-perform-node "LOOP-WITH"
                          :varying var :from start :to end :by step :then stmts)))
    (WHILE expression DO statement-list END
      (lambda (cond stmts)
        (make-perform-node "LOOP-WHILE"
                          :until (make-conditional-not cond) :then stmts))))

  ;; Conditional Statements
  (if-stmt
    (IF expression THEN statement-list ELSE statement-list? END IF
      (lambda (cond then-stmts else-stmts _end)
        (make-if-node cond then-stmts (or else-stmts '())))))

  ;; Return Statement
  (return-stmt
    (RETURN expression?
      (lambda (expr)
        (if expr (make-move-node expr "RESULT") (make-goback-node)))))

  ;; Goto Statement
  (goto-stmt
    (GOTO ident
      (lambda (label)
        (make-goto-node label))))

  ;; Expression Parsing
  (expression
    (primary))

  (primary
    (identifier COLON ident arg-list?
      (lambda (obj method args)
        (make-invoke-node obj method :args args)))
    (identifier DOT ident arg-list?
      (lambda (obj method args)
        (make-invoke-node obj method :args args))))

  (identifier
    (string (lambda (s) s)))

  (arg-list?
    ((constantly nil))
    (|,| arg-list (lambda (args) args)))))