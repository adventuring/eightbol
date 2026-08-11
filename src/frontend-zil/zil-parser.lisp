;; src/frontend-zil/zil-parser.lisp — YACC grammar for ZIL → EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token list for ZIL YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun zil-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |,| |'|
              + - * / = <> < > <= >=
              and or not
              :|<IF>| :|<ELSE>| :|<THEN>| :|<WHILE>| :|<FOR>|
              :|<BREAK>| :|<CONTINUE>| :|<RETURN>|
              :|<DEFINE>| :|<SET>| :|<SETG>| :|<GET>| :|<PUT>| :|<CALL>|
              :|<INVOKE>| :|<GO>| :|<EXIT>| :|<LABEL>| :|<ASSERT>| :|<WAIT>|
              :|<STOP>| :|<START>| :|<FADE>| :|<RESTORE>| :|<SAVE>| :|<RESTART>|
              :|<VERSION>| :|<OBJECT>| :|<PROPERTY>| :|<GLOBAL>| :|<LOCAL>|
              :|<P>| :|<PRINT>| :|<INPUT>| :|<DIALOGUE>|
              number string atom))))

;;; Parser action functions for ZIL

(defun zil-parse-program (forms)
  "Top-level program node."
  (make-program-node "ZIL" :data (when forms (list (cons 'main-block forms)))))

(defun zil-parse-if (cond then-form &optional else-form)
  "(<IF> cond then-form [else-form]) – conditional statement"
  (make-if-node cond (list then-form) (if else-form (list else-form) '())))

(defun zil-parse-while (cond body)
  "(<WHILE> cond body) – while loop"
  (make-perform-node (list body) :until (make-conditional-not cond)))

(defun zil-parse-for (var start end body)
  "(<FOR> var start end body) – for loop"
  (make-perform-node (list body) :varying var :from start :by 1
                                 :until (make-conditional-gt (make-identifier var) end)))

(defun zil-parse-define (name params body)
  "(<DEFINE> name params body) – function/routine definition"
  (make-method-node name :statements (list body)))

(defun zil-parse-set (var expr)
  "(<SET> var expr) – local variable assignment"
  (make-move-node expr (make-identifier var)))

(defun zil-parse-setg (var expr)
  "(<SETG> var expr) – global variable assignment"
  (make-move-node expr (make-identifier var)))

(defun zil-parse-call (func args)
  "(<CALL> func args) – subroutine call"
  (make-call-node func))

(defun zil-parse-invoke (obj method)
  "(<INVOKE> obj method) – method/object invocation"
  (make-invoke-node obj method))

(defun zil-parse-return (&optional val)
  "(<RETURN> [val]) – return from subroutine"
  (make-goback-node))

(defun zil-parse-break ()
  "(<BREAK>) – break statement"
  (make-break-node))

(defun zil-parse-continue ()
  "(<CONTINUE>) – continue statement"
  (make-continue-node))

(defun zil-parse-print (&rest args)
  "(<PRINT> arg1 arg2 ...) – output to console"
  (make-print-node args))

(defun zil-parse-input (var)
  "(<INPUT> var) – read input into variable"
  (make-input-node var))

(defun zil-parse-get (obj prop)
  "(<GET> obj prop) – get object property"
  (make-get-node obj prop))

(defun zil-parse-put (obj prop val)
  "(<PUT> obj prop val) – set object property"
  (make-put-node obj prop val))

(defun zil-parse-object (name &rest props)
  "(<OBJECT> name prop1 val1 ...) – create object"
  (make-object-node name props))

(defun zil-parse-label (name)
  "(<LABEL> name) – define label"
  (make-label-node name))

(defun zil-parse-go (label)
  "(<GO> label) – goto statement"
  (make-goto-node label))

(defun zil-parse-exit (&optional code)
  "(<EXIT> [code]) – exit program"
  (make-exit-node (or code 0)))

(defun zil-parse-wait (&optional time)
  "(<WAIT> [time]) – wait/delay"
  (make-wait-node (or time 0)))

(defun zil-parse-stop ()
  "(<STOP>) – stop execution"
  (make-stop-run-node 0))

(defun zil-parse-assertion (cond msg)
  "(<ASSERT> cond msg) – assertion"
  (make-assert-node cond msg))

(defun zil-parse-dialogue (text)
  "(<DIALOGUE> text) – dialogue/narrative text (print variant)"
  (make-print-node (list text)))

(defun make-break-node ()
  "Create a BREAK AST node."
  '(:break))

(defun make-continue-node ()
  "Create a CONTINUE AST node."
  '(:continue))

(defun make-print-node (args)
  "Create a PRINT AST node from list of arguments."
  (cons :print args))

(defun make-input-node (var)
  "Create an INPUT AST node for reading into variable."
  (list :input var))

(defun make-get-node (obj prop)
  "Create a GET (property access) AST node."
  (list :get obj prop))

(defun make-put-node (obj prop val)
  "Create a PUT (property set) AST node."
  (list :put obj prop val))

(defun make-object-node (name props)
  "Create an OBJECT definition AST node."
  (cons :object (cons name props)))

(defun make-label-node (name)
  "Create a LABEL definition AST node."
  (list :label name))

(defun make-exit-node (code)
  "Create an EXIT AST node."
  (list :exit code))

(defun make-wait-node (time)
  "Create a WAIT/DELAY AST node."
  (list :wait time))

(defun make-assert-node (cond msg)
  "Create an ASSERT AST node."
  (list :assert cond msg))

;;; Create the YACC parser
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *zil-parser*
      (:start-symbol program)
      (:terminals (lparen rparen comma quote
                          plus minus times divide equal ne
                          lt gt le ge
                          and or not
                          :|<IF>| :|<ELSE>| :|<THEN>| :|<WHILE>| :|<FOR>|
                          :|<BREAK>| :|<CONTINUE>| :|<RETURN>|
                          :|<DEFINE>| :|<SET>| :|<SETG>| :|<GET>| :|<PUT>| :|<CALL>|
                          :|<INVOKE>| :|<GO>| :|<EXIT>| :|<LABEL>| :|<ASSERT>| :|<WAIT>|
                          :|<STOP>| :|<START>| :|<FADE>| :|<RESTORE>| :|<SAVE>| :|<RESTART>|
                          :|<VERSION>| :|<OBJECT>| :|<PROPERTY>| :|<GLOBAL>| :|<LOCAL>|
                          :|<P>| :|<PRINT>| :|<INPUT>| :|<DIALOGUE>|
                          number string atom))
      
      (program
       (form
        (lambda (f) (zil-parse-program (list f))))
       (program form
                (lambda (prog f) (nconc prog (list f)))))
      
      (form
       (atom
        (lambda (a) (make-identifier a)))
       (number
        (lambda (n) n))
       (string
        (lambda (s) s))
       (sexp
        (lambda (s) s)))
      
      (sexp
       ;; Control flow
       (lparen :|<IF>| expression form form rparen
               (lambda (condition then else) (zil-parse-if condition then else)))
       (lparen :|<IF>| expression form rparen
               (lambda (condition then) (zil-parse-if condition then)))
       (lparen :|<WHILE>| expression form rparen
               (lambda (c b) (zil-parse-while c b)))
       (lparen :|<FOR>| atom expression expression form rparen
               (lambda (v s e b) (zil-parse-for v s e b)))
       (lparen :|<BREAK>| rparen
               (lambda () (zil-parse-break)))
       (lparen :|<CONTINUE>| rparen
               (lambda () (zil-parse-continue)))
       (lparen :|<RETURN>| rparen
               (lambda () (zil-parse-return)))
       (lparen :|<RETURN>| expression rparen
               (lambda (v) (zil-parse-return v)))
       
       ;; Variable assignment
       (lparen :|<SET>| atom expression rparen
               (lambda (v e) (zil-parse-set v e)))
       (lparen :|<SETG>| atom expression rparen
               (lambda (v e) (zil-parse-setg v e)))
       
       ;; Function definition and calling
       (lparen :|<DEFINE>| atom form-list form rparen
               (lambda (n p b) (zil-parse-define n p b)))
       (lparen :|<CALL>| atom rparen
               (lambda (f) (zil-parse-call f nil)))
       (lparen :|<CALL>| atom form-list rparen
               (lambda (f a) (zil-parse-call f a)))
       (lparen :|<INVOKE>| atom atom rparen
               (lambda (o m) (zil-parse-invoke o m)))
       
       ;; Object and property access
       (lparen :|<OBJECT>| atom form-list rparen
               (lambda (n p) (zil-parse-object n p)))
       (lparen :|<GET>| atom atom rparen
               (lambda (o p) (zil-parse-get o p)))
       (lparen :|<PUT>| atom atom expression rparen
               (lambda (o p v) (zil-parse-put o p v)))
       
       ;; I/O and dialogue
       (lparen :|<PRINT>| form-list rparen
               (lambda (forms) (apply #'zil-parse-print forms)))
       (lparen :|<INPUT>| atom rparen
               (lambda (input) (zil-parse-input input)))
       (lparen :|<DIALOGUE>| string rparen
               (lambda (text) (zil-parse-dialogue text)))
       
       ;; Labels and jumps
       (lparen :|<LABEL>| atom rparen
               (lambda (n) (zil-parse-label n)))
       (lparen :|<GO>| atom rparen
               (lambda (l) (zil-parse-go l)))
       
       ;; Misc statements
       (lparen :|<EXIT>| rparen
               (lambda () (zil-parse-exit)))
       (lparen :|<EXIT>| expression rparen
               (lambda (c) (zil-parse-exit c)))
       (lparen :|<WAIT>| rparen
               (lambda () (zil-parse-wait)))
       (lparen :|<WAIT>| expression rparen
               (lambda (time) (zil-parse-wait time)))
       (lparen :|<STOP>| rparen
               (lambda () (zil-parse-stop)))
       (lparen :|<ASSERT>| expression string rparen
               (lambda (c m) (zil-parse-assertion c m)))
       
       ;; Atomic sexp
       (lparen atom rparen
               (lambda (a) a))
       (lparen atom form-list rparen
               (lambda (f args) (list f args))))
      
      (expression
       (atom
        (lambda (a) a))
       (number
        (lambda (n) n))
       (string
        (lambda (s) s))
       (expression plus expression
                   (lambda (l r) (make-expression-add l r)))
       (expression minus expression
                   (lambda (l r) (make-expression-subtract l r)))
       (expression times expression
                   (lambda (l r) (make-expression-multiply l r)))
       (expression divide expression
                   (lambda (l r) (make-expression-divide l r)))
       (expression equal expression
                   (lambda (l r) (make-conditional-eq l r)))
       (expression ne expression
                   (lambda (l r) (make-conditional-ne l r)))
       (expression lt expression
                   (lambda (l r) (make-conditional-lt l r)))
       (expression gt expression
                   (lambda (l r) (make-conditional-gt l r)))
       (expression le expression
                   (lambda (l r) (make-conditional-le l r)))
       (expression ge expression
                   (lambda (l r) (make-conditional-ge l r)))
       (and form-list
            (lambda (forms) (if (cdr forms) (make-conditional-and (car forms) (cadr forms)) (car forms))))
       (or form-list
           (lambda (forms) (if (cdr forms) (make-conditional-or (car forms) (cadr forms)) (car forms))))
       (not expression
            (lambda (f) (make-conditional-not f))))
      
      (form-list
       (form
        (lambda (f) (list f)))
       (form-list form
                  (lambda (fl f) (nconc fl (list f))))))))

(defun zil-parser ()
  "Return the ZIL YACC parser function."
  *zil-parser*)


