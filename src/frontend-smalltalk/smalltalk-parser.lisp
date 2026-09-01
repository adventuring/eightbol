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

(defun smalltalk-normalize-identifier (ident)
  "Normalize IDENT to camelCase for Smalltalk AST node representation.
Renamed from normalize-identifier to avoid collision with COBOL lexer's normalize-identifier."
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

(defun smalltalk-make-move-node (target value)
  "Create an assignment AST node (MOVE in COBOL terminology)."
  (list :move
        :target (smalltalk-normalize-identifier target)
        :value value))

(defun smalltalk-make-invoke-node (receiver selector)
  "Create a message send (method invocation) AST node."
  (list :invoke
        :receiver receiver
        :selector (smalltalk-normalize-identifier selector)))

(defun smalltalk-make-literal-string (s)
  "Create a string literal AST node."
  (list :literal-string :value s))

(defun smalltalk-make-literal-number (n &key format)
  "Create a numeric literal AST node with optional format type.
FORMAT can be :DECIMAL, :HEX, :OCTAL, :BINARY, :DWORD, or NIL."
  (if format
      (list :literal-number :value n :format format)
      (list :literal-number :value n)))

(defun smalltalk-make-print-node (args)
  "Create a print/output statement AST node."
  (list :print :args args))

(defun smalltalk-make-input-node (&key var prompt)
  "Create an input/read statement AST node."
  (list :input :var var :prompt prompt))

(defun smalltalk-make-if-node (condition true-branch &key false-branch)
  "Create an if/conditional statement AST node."
  (list :if :condition condition :true-branch true-branch :false-branch false-branch))

(defun smalltalk-make-while-node (condition body)
  "Create a while loop AST node (converted to :perform)."
  (list :perform :body body :until (list :not condition)))

(defun smalltalk-make-block-node (&key params expressions)
  "Create a code block AST node."
  (list :block :params params :expressions expressions))

(defun smalltalk-make-call-node (target &key args)
  "Create a function call AST node."
  (list :call :target target :using args))

(defun smalltalk-make-perform-node (body &key until varying)
  "Create a loop/perform AST node."
  (let ((node (list :perform :body body)))
    (when until (setf node (append node (list :until until))))
    (when varying (setf node (append node (list :varying varying))))
    node))

(defun smalltalk-make-copy-node (filename)
  "Create a copy (include) AST node."
  (list :copy :name filename))

(defun smalltalk-make-dialogue-node (&key speaker text)
  "Create a dialogue AST node."
  (list :dialogue :speaker speaker :text text))

(defun smalltalk-make-comment-node (text)
  "Create a comment AST node."
  (list :comment :text text))

(defun smalltalk-make-break-node ()
  "Create a break AST node."
  (list :break))

(defun smalltalk-make-continue-node ()
  "Create a continue AST node."
  (list :continue))

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
          (smalltalk-make-move-node target value))))

    ;; Print statement: print arg1, arg2, ...
    (print-stmt
      (:PRINT expression-list
        (lambda (args) (smalltalk-make-print-node args)))
      (:PRINT expression
        (lambda (arg) (smalltalk-make-print-node (list arg))))
      (:DISPLAY expression-list
        (lambda (args) (smalltalk-make-print-node args)))
      (:DISPLAY expression
        (lambda (arg) (smalltalk-make-print-node (list arg)))))

    ;; Input statement: input var or ask var : 'prompt'
    (input-stmt
      (:INPUT variable
        (lambda (var) (smalltalk-make-input-node :var var)))
      (:READ variable
        (lambda (var) (smalltalk-make-input-node :var var)))
      (:ASK variable :COLON string
        (lambda (var prompt) (smalltalk-make-input-node :var var :prompt prompt))))

    ;; Dialogue statement: dialogue 'speaker' : 'text' or say 'text'
    (dialogue-stmt
      (:DIALOGUE string :COLON string
        (lambda (speaker text) (smalltalk-make-dialogue-node :speaker speaker :text text)))
      (:SAY string
        (lambda (text) (smalltalk-make-dialogue-node :speaker "Narrator" :text text))))

    ;; If statement: expression ifTrue: block ifFalse: block
    (if-stmt
      (expression :IF-TRUE block
        (lambda (cond true-block)
          (smalltalk-make-if-node cond true-block)))
      (expression :IF-TRUE block :IF-FALSE block
        (lambda (cond true-block false-block)
          (smalltalk-make-if-node cond true-block :false-branch false-block)))
      (expression :IF-TRUE-FALSE block block
        (lambda (cond true-block false-block)
          (smalltalk-make-if-node cond true-block :false-branch false-block))))

     ;; While statement: expression whileTrue: block
     (while-stmt
       (expression :WHILE-TRUE block
         (lambda (cond body)
           (smalltalk-make-while-node cond body))))

     ;; Copy statement: COPY "Filename":
     (copy-stmt
       (:COPY string :COLON
         (lambda (filename _colon)
           (declare (ignore _colon))
           (smalltalk-make-copy-node filename))))

      ;; Message: receiver.selector or receiver.selector(args)
      (message
       (expression :PERIOD expression
         (lambda (receiver selector)
           (list :invoke :receiver receiver :selector selector)))
       (expression :PERIOD expression :LPAREN expression-list :RPAREN
         (lambda (receiver selector args)
           (declare (ignore args))
           (list :invoke :receiver receiver :selector selector))))

    (expression
      (variable)
      (literal)
      (block)
      (:LPAREN expression :RPAREN
        (lambda (expr) expr)))

    (variable
      (ident (lambda (i) (smalltalk-normalize-identifier i))))

    (literal
      (number)
      (string (lambda (s) (smalltalk-make-literal-string s)))
      (symbol (lambda (sym) (list :symbol :value sym))))

    (ident
      (:IDENT (lambda (i) i)))

    (number
      (:NUMBER (lambda (n) (smalltalk-make-literal-number n))))

    (string
      (:STRING (lambda (s) s)))

    (symbol
      (:SYMBOL (lambda (sym) sym)))

    (block
      (block-literal))

    (block-literal
      (:LBRACKET expression-list :RBRACKET
        (lambda (exprlist)
          (smalltalk-make-block-node :expressions exprlist)))
      (:LBRACKET :VBAR param-list :VBAR expression-list :RBRACKET
        (lambda (params exprs)
          (smalltalk-make-block-node :params params :expressions exprs))))

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
