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

(defparameter *pascal-keyword-alist*
  '(("PROCEDURE" . :procedure)
    ("VAR" . :var)
    ("BEGIN" . :begin)
    ("END" . :end)
    ("CALL" . :call)
    ("LABEL" . :label)
    ("METHOD" . :method)
    ("LIBRARY" . :library)
    ("ON" . :on)
    ("PROTOCOL" . :protocol)
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
    ("RETURNING" . :returning))
  "Association list of Pascal keywords to token symbols.")

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

(defun pascal-tokenize (input)
  "Tokenize Pascal INPUT string into a stream of tokens."
  (pascal-lex-source input))

(defun pascal-parse (tokens)
  "Parse TOKENS into an EIGHTBOL AST program node."
  (funcall *pascal-parser* tokens))

(yacc:define-parser *pascal-parser*
  (:start-symbol pascal-program)
  (:terminals
   (:number :string :ident
            :procedure :var :begin :end :call :label :method :on :protocol
    :if :then :else :while :until :do :next :step :for
            :exit :goback :log :fault :stop :copy :as :returning
            :colon :semicolon :comma :lparen :rparen :dot
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
   perform-loop
   exit
   error-statement
   copy-statement)
  
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
           (error "unimplemented: far call")))
   (CALL IDENTIFIER IN LIBRARY
         (lambda (_call routine-name _in _library)
           (declare (ignore _call _in _library))
           (error "unimplemented: library call"))))
  
  (conditional
   (IF expression THEN statement-list ELSE statement-list
       (lambda (ignore1 cond ignore2 then-stmts ignore3 else-stmts)
         (declare (ignore ignore1 ignore2 ignore3))
         (make-if-node cond then-stmts else-stmts)))
   (IF expression THEN statement-list
       (lambda (ignore1 cond ignore2 stmts)
         (declare (ignore ignore1 ignore2))
         (make-if-node cond stmts))))
  
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
   (COPY STRING
         (lambda (_ name)
           (declare (ignore _))
           (make-copy-node name))))
  
  (expression
   (primary)
   (expression binary-op expression
               (lambda (left op right)
                 (make-binary-op-node :operator op :left left :right right)))
   (unary-op expression
             (lambda (op expr)
               (make-unary-op-node :operator op :operand expr))))
  
  (primary
   (IDENT (lambda (id) (cadr id)))
   (NUMBER (lambda (num) num))
   (STRING (lambda (str) str))
   (LPAREN expression RPAREN (lambda (lpar expr rpar)
                               (declare (ignore lpar rpar))
                               expr)))
  
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
    (constantly :ne)))
  
  (unary-op
   (NOT
    (constantly :not))
   (MINUS
    (constantly :unary-minus))))

(defun make-debug-break-node (code)
  "Create a :debug-break AST node."
  (list :debug-break :code code))

(defun make-binary-op-node (&key operator left right)
  "Create a binary operation AST node."
  (list :binary-op :operator operator :left left :right right))

(defun make-unary-op-node (&key operator operand)
  "Create a unary operation AST node."
  (list :unary-op :operator operator :operand operand))
