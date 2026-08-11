;; pascal-parser.lisp — CL-YACC grammar for Pascal → EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (declaim (ftype (function (list) cons) 
                  parse/pascal-program
                  parse/method-definition
                  parse/label-statement
                  parse/assignment
                  parse/calls
                  parse/conditional
                  parse/perform-loop
                  parse/exit
                  parse/error-statement
                  parse/copy-statement
                  parse/expression
                  parse/primary
                  parse/binary-op
                  parse/unary-op)))

   (defparameter *pascal-keyword-symbols*
   '((:procedure . |PROCEDURE|)
     (:var . |VAR|)
     (:begin . |BEGIN|)
     (:end . |END|)
     (:call . |CALL|)
     (:label . |LABEL|)
     (:goto . |GOTO|)
     (:go . |GO|)
     (:to . |TO|)
     (:method . |METHOD|)
     (:library . |LIBRARY|)
     (:in . |IN|)
     (:on . |ON|)
     (:protocol . |PROTOCOL|)
     (:and . |AND|)
     (:or . |OR|)
     (:not . |NOT|)
     (:if . |IF|)
     (:then . |THEN|)
     (:else . |ELSE|)
     (:while . |WHILE|)
     (:until . |UNTIL|)
     (:do . |DO|)
     (:next . |NEXT|)
     (:step . |STEP|)
     (:for . |FOR|)
     (:exit . |EXIT|)
     (:goback . |GOBACK|)
     (:log . |LOG|)
     (:fault . |FAULT|)
     (:stop . |STOP|)
     (:copy . |COPY|)
     (:as . |AS|)
     (:returning . |RETURNING|)
     (:print . |PRINT|)
     (:input . |INPUT|)
     (:read . |READ|)
     (:write . |WRITE|)
     (:dialogue . |DIALOGUE|)
     (:case . |CASE|)
     (:of . |OF|)
     (:endcase . |ENDCASE|))
   "Map keyword symbols to their yacc parser terminal forms.")

(defparameter *pascal-operators-alist*
  '(("+" . :plus)
    ("-" . :minus)
    ("*" . :times)
    ("/" . :divide)
    ("=" . :equals)
    ("<" . :lt)
    (">" . :gt)
    ("<=" . :le)
    (">=" . :ge)
    ("<>" . :ne)
    (":" . :colon)
    (";" . :semicolon)
    ("," . :comma)
    ("(" . :lparen)
    (")" . :rparen)
    ("." . :dot))
  "Association list of Pascal operators to token symbols.")

   (defparameter *pascal-keyword-alist*
   '(("PROCEDURE" . :procedure)
     ("VAR" . :var)
     ("BEGIN" . :begin)
     ("END" . :end)
     ("CALL" . :call)
     ("LABEL" . :label)
     ("GOTO" . :goto)
     ("GO" . :go)
     ("TO" . :to)
     ("METHOD" . :method)
     ("LIBRARY" . :library)
     ("IN" . :in)
     ("ON" . :on)
     ("PROTOCOL" . :protocol)
     ("AND" . :and)
     ("OR" . :or)
     ("NOT" . :not)
     ("IF" . :if)
     ("THEN" . :then)
     ("ELSE" . :else)
     ("WHILE" . :while)
     ("UNTIL" . :until)
     ("DO" . :do)
     ("NEXT" . :next)
     ("STEP" . :step)
     ("FOR" . :for)
     ("EXIT" . :exit)
     ("GOBACK" . :goback)
     ("LOG" . :log)
     ("FAULT" . :fault)
     ("STOP" . :stop)
     ("COPY" . :copy)
     ("AS" . :as)
     ("RETURNING" . :returning)
     ("PRINT" . :print)
     ("INPUT" . :input)
     ("READ" . :read)
     ("WRITE" . :write)
     ("DIALOGUE" . :dialogue)
     ("CASE" . :case)
     ("OF" . :of)
     ("ENDCASE" . :endcase))
   "Association list of Pascal keywords to token symbols.")

(defun pascal-tokenize (input)
  "Tokenize Pascal INPUT string into a stream of (token-type . value) pairs for YACC.
Keywords are mapped to their symbol forms matching terminal declarations."
  (let ((raw-tokens (pascal-lex-source input))
        (result '()))
    (dolist (tok raw-tokens)
      (let ((type (first tok))
            (value (second tok)))
        (cond
          ;; Keywords: return as keywords using symbol lookup
          ((eq type :keyword)
           (let ((upper-kw (string-upcase value))
                 (kw-entry (find-if (lambda (entry)
                                      (string-equal upper-kw (symbol-name (car entry))))
                                    *pascal-keyword-alist*)))
             (if kw-entry
                 (push (cons (cdr kw-entry) value) result)
                 ;; Fallback: create symbol from upcase
                 (push (cons (intern upper-kw :keyword) value) result))))
          ;; Operators: map to corresponding operators
          ((eq type :op)
           (let ((op-entry (assoc value *pascal-operators-alist* :test #'string-equal)))
             (if op-entry
                 (push (cons (cdr op-entry) value) result)
                 (push (cons :op value) result))))
          ;; Numbers: convert to symbol number
          ((eq type :number)
           (push (cons 'number value) result))
          ;; Strings: convert to symbol string
          ((eq type :string)
           (push (cons 'string value) result))
          ;; Identifiers: convert to symbol ident
          ((eq type :ident)
           (push (cons 'ident value) result))
          ;; Punctuation: return as lowercase symbols
          ((eq type :colon)
           (push (cons :colon nil) result))
          ((eq type :semicolon)
           (push (cons :semicolon nil) result))
          ((eq type :comma)
           (push (cons :comma nil) result))
          ((eq type :lparen)
           (push (cons :lparen nil) result))
          ((eq type :rparen)
           (push (cons :rparen nil) result))
          ((eq type :dot)
           (push (cons :dot nil) result))
          ;; Labels: special handling
          ((eq type :label)
           (push (cons :label value) result))
          (t
           (push (cons :unknown value) result)))))
    (nreverse result)))

(defun pascal-parse (tokens)
  "Parse TOKENS into an EIGHTBOL AST program node."
  (funcall *pascal-parser* tokens))

  (yacc:define-parser *pascal-parser*
   (:start-symbol pascal-program)
   (:terminals
    (:number :string :ident
             :procedure :var :begin :end :call :label :method :on :protocol
             :library :in :and :or :not
             :if :then :else :while :until :do :next :step :for
             :exit :goback :log :fault :stop :copy :as :returning
             :print :input :read :write :dialogue :case :of :endcase
             :colon :semicolon :comma :lparen :rparen :dot :lbracket :rbracket
             :plus :minus :times :divide :equals :lt :gt :le :ge :ne))

  (:precedence ((:left :plus :minus)
                (:left :times :divide)
                (:left :equals :lt :gt :le :ge :ne)
                (:left :or)
                (:left :and)
                (:right :not :unary-minus)))
  
  (pascal-program
   (method-list
    (lambda (methods) (make-program-node "PascalProgram" :methods methods))))

  (method-list
   (method
    (lambda (m) (list m)))
   (method-list method
                (lambda (ms m) (append ms (list m)))))

  (method
   (procedure-identifier method-body
                         (lambda (proc-name class-name body)
                           (make-method-node (format nil "~A#~A" class-name proc-name) :statements body)))
   (procedure-identifier
    (lambda (proc-name)
      (make-method-node proc-name :statements '()))))

  (procedure-identifier
   (PROCEDURE IDENTIFIER ON IDENTIFIER
              (lambda (p1 p2 proc-name p3 class-name)
                (declare (ignore p1 p2 p3))
                (make-method-node (format nil "~A#~A" class-name proc-name) :statements '())))
   (PROCEDURE IDENTIFIER
              (lambda (p1 p2 proc-name)
                (declare (ignore p1 p2))
                (make-method-node proc-name :statements '()))))
  
  (method-body
   (BEGIN statement-list END
          (lambda (ignore1 stmts ignore2)
            (declare (ignore ignore1 ignore2))
            stmts)))
  
  (statement-list
   ()
   (statement
    (lambda (s) (list s)))
   (statement-list statement
                   (lambda (ss s) (append ss (list s)))))
  
    (statement
     label-statement
     assignment
     calls
     conditional
     case-statement
     perform-loop
     exit
     error-statement
     copy-statement
     print-statement
     input-statement
     dialogue-statement)
  
  (label-statement
   (NUMBER COLON
           (lambda (ignore1 num)
             (declare (ignore ignore1))
             (make-assembly-entry-node num)))
   (IDENTIFIER COLON
               (lambda (ignore1 label)
                 (declare (ignore ignore1))
                 (make-assembly-entry-node label))))
  
  (assignment
   (IDENTIFIER EQUALS expression
               (lambda (target _ expr)
                 (declare (ignore _))
                 (make-move-node expr target)))
   (MOVE expression IDENTIFIER
         (lambda (_ expr target)
           (declare (ignore _))
           (make-move-node expr target))))
  
  (calls
   (CALL IDENTIFIER
         (lambda (ignored1 target)
           (declare (ignore ignored1))
           (make-call-node target)))
   (CALL IDENTIFIER ON IDENTIFIER
         (lambda (ignored1 method-name ignored2 object-name)
           (declare (ignore ignored1 ignored2))
           (make-invoke-node object-name method-name)))
    (CALL IDENTIFIER IN NUMBER
          (lambda (_call routine-name _in bank-name)
            (declare (ignore _call _in))
            (make-call-node routine-name :bank bank-name)))
    (CALL IDENTIFIER IN LIBRARY
          (lambda (_call routine-name _in _library)
            (declare (ignore _call _in _library))
            (list :call :target routine-name :bank nil :library t))))
  
   (conditional
    (IF expression THEN statement-list ELSE statement-list
        (lambda (ignore1 cond ignore2 then-stmts ignore3 else-stmts)
          (declare (ignore ignore1 ignore2 ignore3))
          (make-if-node cond then-stmts else-stmts)))
    (IF expression THEN statement-list
        (lambda (ignore1 cond ignore2 stmts)
          (declare (ignore ignore1 ignore2))
          (make-if-node cond stmts))))
   
   (case-statement
    (CASE expression OF case-clauses ENDCASE
     (lambda (_case subject _of clauses _end)
       (declare (ignore _case _of _end))
       (list :evaluate :subject subject :when-clauses clauses))))
   
   (case-clauses
    (case-clause
     (lambda (clause) (list clause)))
    (case-clauses case-clause
     (lambda (clauses clause) (append clauses (list clause)))))
   
   (case-clause
    (NUMBER COLON statement-list SEMICOLON
     (lambda (value _colon stmts _semi)
       (declare (ignore _colon _semi))
       (list :when (list :equals value) stmts)))
    (NUMBER COLON statement-list
     (lambda (value _colon stmts)
       (declare (ignore _colon))
       (list :when (list :equals value) stmts)))
    (ELSE COLON statement-list SEMICOLON
     (lambda (_else _colon stmts _semi)
       (declare (ignore _else _colon _semi))
       (list :when-other stmts)))
    (ELSE COLON statement-list
     (lambda (_else _colon stmts)
       (declare (ignore _else _colon))
       (list :when-other stmts))))
  
  (perform-loop
   (WHILE expression NEXT statement-list
     (lambda (ignore-while cond ignore-next stmts)
       (declare (ignore ignore-while ignore-next stmts))
       (make-perform-node "WHILE" :until cond)))
   (UNTIL expression NEXT statement-list
     (lambda (ignore-until cond ignore-next stmts)
       (declare (ignore ignore-until ignore-next stmts))
       (make-perform-node "UNTIL" :until cond)))
   (DO NEXT WHILE expression statement-list
     (lambda (ignore1 ignore2 ignore3 cond stmts)
       (declare (ignore ignore1 ignore2 ignore3 stmts))
       (make-perform-node "WHILE" :until cond)))
   (DO NEXT UNTIL expression statement-list
     (lambda (ignore1 ignore2 ignore3 cond stmts)
       (declare (ignore ignore1 ignore2 ignore3 stmts))
       (make-perform-node "UNTIL" :until cond)))
   (FOR IDENTIFIER FROM expression TO expression STEP expression NEXT
     (lambda (ignore1 var ignore2 from ignore3 to ignore4 step ignore5)
       (declare (ignore ignore1 ignore2 ignore3 ignore4 ignore5))
       (make-perform-node "PERFORM-VARYING"
                          :varying var :from from :by step
                          :until (make-conditional-gt (make-identifier var) to)))))
  
  (exit
   (EXIT
    (lambda () (make-exit-method-node)))
   (GOBACK
    (lambda () (make-goback-node))))
  
  (error-statement
   (LOG FAULT expression
        (lambda (ignore1 ignore2 code)
          (declare (ignore ignore1 ignore2))
          (make-log-fault-node code)))
   (STOP expression
         (lambda (_ code)
           (declare (ignore _))
           (make-debug-break-node code))))
  
    (copy-statement
     (COPY STRING SEMICOLON
           (lambda (_ name _semi)
             (declare (ignore _ _semi))
             (make-copy-node name)))
     (COPY STRING
           (lambda (_ name)
             (declare (ignore _))
             (make-copy-node name))))

   (print-statement
    (PRINT expression
           (lambda (_ expr)
             (declare (ignore _))
             (pascal-make-print-node expr)))
    (PRINT STRING
           (lambda (_ str)
             (declare (ignore _))
             (pascal-make-print-node str)))
    (WRITE expression
           (lambda (_ expr)
             (declare (ignore _))
             (pascal-make-print-node expr)))
    (WRITE STRING
           (lambda (_ str)
             (declare (ignore _))
             (pascal-make-print-node str))))

   (input-statement
    (INPUT IDENTIFIER
           (lambda (_ var)
             (declare (ignore _))
             (pascal-make-input-node var)))
    (READ IDENTIFIER
          (lambda (_ var)
            (declare (ignore _))
            (pascal-make-input-node var))))

   (dialogue-statement
    (DIALOGUE STRING
              (lambda (_ text)
                (declare (ignore _))
                (pascal-make-dialogue-node text))))
   
   (expression
    (primary)
    (expression binary-op expression
                (lambda (left op right)
                  (pascal-binary-operator op left right)))
    (unary-op expression
              (lambda (op expr)
                (pascal-unary-operator op expr))))
   
   (primary
    (IDENT (lambda (id) (cadr id)))
    (NUMBER (lambda (num) num))
    (STRING (lambda (str) str))
    (LPAREN expression RPAREN (lambda (lpar expr rpar)
                                (declare (ignore lpar rpar))
                                expr))
    (primary DOT IDENT
             (lambda (base _dot slot)
               (declare (ignore _dot))
               (list :of "slot" base (cadr slot))))
    (primary LBRACKET expression RBRACKET
             (lambda (base _lbracket idx _rbracket)
               (declare (ignore _lbracket _rbracket))
               (list :subscript base idx))))
  
  (binary-op
   (PLUS
    (constantly :plus))
   (MINUS
    (constantly :minus))
   (TIMES
    (constantly :times))
   (DIVIDE
    (constantly :divide))
   (EQUALS
    (constantly :equals))
   (LT
    (constantly :lt))
   (GT
    (constantly :gt))
   (LE
    (constantly :le))
   (GE
    (constantly :ge))
   (NE
    (constantly :ne))
   (AND
    (constantly :and))
   (OR
    (constantly :or)))
  
  (unary-op
   (NOT
    (constantly :not))
   (MINUS
    (constantly :unary-minus))))

(defun pascal-binary-operator (operator left right)
  "Combine LEFT OPERATOR RIGHT into a standard EIGHTBOL expression node."
  (ecase operator
    (:plus (make-expression-add left right))
    (:minus (make-expression-subtract left right))
    (:times (make-expression-multiply left right))
    (:divide (make-expression-divide left right))
    (:equals (make-conditional-eq left right))
    (:ne (make-conditional-ne left right))
    (:lt (make-conditional-lt left right))
    (:le (make-conditional-le left right))
    (:gt (make-conditional-gt left right))
    (:ge (make-conditional-ge left right))
    (:and (make-conditional-and left right))
    (:or (make-conditional-or left right))))

(defun pascal-unary-operator (operator operand)
  "Combine OPERATOR OPERAND into a standard EIGHTBOL expression node."
  (ecase operator
    (:not (make-conditional-not operand))
    (:unary-minus (make-expression-subtract 0 operand))))

(defun make-debug-break-node (code)
  "Create a :debug-break AST node."
  (list :debug-break :code code))

(defun pascal-make-print-node (expr)
  "Create a :print AST node for output statements (PRINT or WRITE).
Calls the canonical make-print-node from grammar-build.lisp."
  (make-print-node expr))

(defun pascal-make-input-node (var &optional prompt)
  "Create an :input AST node for input statements (INPUT or READ).
Calls the canonical make-input-node from grammar-build.lisp."
  (make-input-node var :prompt prompt))

(defun pascal-make-dialogue-node (name)
  "Create a :dialogue AST node for dialogue/narrative text.
Calls the canonical make-dialogue-node from grammar-build.lisp."
  (make-dialogue-node name :statements '() :goals '()))

(defun make-assembly-entry-node (label)
  "Create an :assembly-entry AST node for label definitions."
  (list :assembly-entry :label label))
