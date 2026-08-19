;; src/frontend-smalltalk/smalltalk-parser.lisp -- YACC parser for SmallTalk source
(in-package :eightbol)

(defun smalltalk-to-camel-case (s)
  "Convert string S to camelCase (first letter lowercase, rest PascalCase).
Examples: 'myVariable' stays 'myVariable', 'MyVariable' becomes 'myVariable'."
  (if (zerop (length s))
      s
      (let ((first-lower (string-downcase (char s 0)))
            (rest (if (> (length s) 1)
                      (subseq s 1)
                      "")))
        (concatenate 'string first-lower rest))))

(defun normalize-identifier (ident)
  "Normalize IDENT to camelCase for AST node representation."
  (if (stringp ident)
      (smalltalk-to-camel-case ident)
      ident))

(defun collect-smalltalk-statements (stmts)
  "Flatten nested statement lists into a single list."
  (if (listp stmts)
      (mapcan (lambda (s)
                (if (and (listp s) (eq (first s) :statements))
                    (rest s)
                    (list s)))
              stmts)
      (list stmts)))

(defun make-move-node (target value)
  "Create an assignment AST node (MOVE in COBOL terminology)."
  (list :move
        :target (normalize-identifier target)
        :value value))

(defun make-invoke-node (receiver selector)
  "Create a message send (method invocation) AST node."
  (list :invoke
        :receiver receiver
        :selector (normalize-identifier selector)))

(defun make-literal-string (s)
  "Create a string literal AST node."
  (list :literal-string :value s))

(defun make-literal-number (n &key format)
  "Create a numeric literal AST node with optional format type.
FORMAT can be :DECIMAL, :HEX, :OCTAL, :BINARY, :DWORD, or NIL."
  (if format
      (list :literal-number :value n :format format)
      (list :literal-number :value n)))

(defun make-print-node (args)
  "Create a print/output statement AST node."
  (list :print :args args))

(defun make-input-node (&key var prompt)
  "Create an input/read statement AST node."
  (list :input :var var :prompt prompt))

(defun make-if-node (condition true-branch &key false-branch)
  "Create an if/conditional statement AST node."
  (list :if :condition condition :true-branch true-branch :false-branch false-branch))

(defun make-while-node (condition body)
  "Create a while loop AST node."
  (list :while :condition condition :body body))

(defun make-block-node (&key params expressions)
  "Create a code block AST node."
  (list :block :params params :expressions expressions))

(eval
 `(yacc:define-parser *smalltalk-parser*
    (:muffle-conflicts :some)
    (:start-symbol program)

    (program (stmt-list
              (lambda (stmts)
                (make-program-node "SmallTalk Program"
                                   :data (collect-smalltalk-statements stmts)))))

     (stmt-list
       (stmt)
       (stmt-list stmt
                  (lambda (prefix suffix) (append prefix (list suffix)))))

     (stmt (assignment))
     (stmt (print-stmt))
     (stmt (input-stmt))
     (stmt (dialogue-stmt))
     (stmt (if-stmt))
     (stmt (while-stmt))
     (stmt (copy-stmt))
     (stmt (message))
     (stmt (expression))

    ;; Assignment: variable := expression
    (assignment
      (variable :ASSIGN expression
        (lambda (target value)
          (make-move-node target value))))

    ;; Print statement: print arg1, arg2, ...
    (print-stmt
      (:PRINT expression-list
        (lambda (args) (make-print-node args)))
      (:PRINT expression
        (lambda (arg) (make-print-node (list arg))))
      (:DISPLAY expression-list
        (lambda (args) (make-print-node args)))
      (:DISPLAY expression
        (lambda (arg) (make-print-node (list arg)))))

    ;; Input statement: input var or ask var : 'prompt'
    (input-stmt
      (:INPUT variable
        (lambda (var) (make-input-node :var var)))
      (:READ variable
        (lambda (var) (make-input-node :var var)))
      (:ASK variable :COLON string
        (lambda (var prompt) (make-input-node :var var :prompt prompt))))

    ;; Dialogue statement: dialogue 'speaker' : 'text' or say 'text'
    (dialogue-stmt
      (:DIALOGUE string :COLON string
        (lambda (speaker text) (make-dialogue-node :speaker speaker :text text)))
      (:SAY string
        (lambda (text) (make-dialogue-node :speaker "Narrator" :text text))))

    ;; If statement: expression ifTrue: block ifFalse: block
    (if-stmt
      (expression :IF-TRUE block
        (lambda (cond true-block)
          (make-if-node cond true-block)))
      (expression :IF-TRUE block :IF-FALSE block
        (lambda (cond true-block false-block)
          (make-if-node cond true-block :false-branch false-block)))
      (expression :IF-TRUE-FALSE block block
        (lambda (cond true-block false-block)
          (make-if-node cond true-block :false-branch false-block))))

     ;; While statement: expression whileTrue: block
     (while-stmt
       (expression :WHILE-TRUE block
         (lambda (cond body)
           (make-while-node cond body))))

     ;; Copy statement: COPY "Filename":
     (copy-stmt
       (:COPY string :COLON
         (lambda (filename _colon)
           (declare (ignore _colon))
           (make-copy-node filename))))

     ;; Message: receiver.selector or receiver.selector(args)
     (message
       (expression :PERIOD expression
         (lambda (receiver selector)
           (:invoke :receiver receiver :selector selector)))
       (expression :PERIOD expression :LPAREN expression-list :RPAREN
         (lambda (receiver selector args)
           (declare (ignore args))
           (:invoke :receiver receiver :selector selector))))

    (expression
      (variable)
      (literal)
      (block)
      (:LPAREN expression :RPAREN
        (lambda (expr) expr)))

    (variable
      (ident (lambda (i) (normalize-identifier i))))

    (literal
      (number)
      (string (lambda (s) (make-literal-string s)))
      (symbol (lambda (sym) (list :symbol :value sym))))

    (ident
      (:IDENT (lambda (i) i)))

    (number
      (:NUMBER (lambda (n) (make-literal-number n))))

    (string
      (:STRING (lambda (s) s)))

    (symbol
      (:SYMBOL (lambda (sym) sym)))

    (block
      (block-literal))

    (block-literal
      (:LBRACKET expression-list :RBRACKET
        (lambda (exprlist)
          (make-block-node :expressions exprlist)))
      (:LBRACKET :VBAR param-list :VBAR expression-list :RBRACKET
        (lambda (params exprs)
          (make-block-node :params params :expressions exprs))))

    (param-list
      (ident)
      (param-list ident
                 (lambda (prefix ident) (append prefix (list ident)))))

    (expression-list
      (expression)
      (expression-list expression
                       (lambda (prefix expr) (append prefix (list expr)))))
    ))

(defun smalltalk-make-parser (parser-name)
  "Create a parser function for SmallTalk with YACC."
  (yacc:make-parser parser-name *smalltalk-parser*))
