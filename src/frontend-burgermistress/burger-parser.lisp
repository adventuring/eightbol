;; src/frontend-burgermistress/burger-parser.lisp — YACC grammar for Burgermistress → EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token list for Burgermistress YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun burgermistress-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |:| |,| |[| |]|
              + - * / = <> < <= > >=
              << >>
              if else then endif while wend for to step next do loop until exit
              sub end function call return gosub
              and or not
              dim as integer string boolean double
              number string ident))))

;;; Parser action functions for Burgermistress

(defun burgermistress-parse-program (statements)
  "Top-level program node from list of statement nodes."
  (let ((main-statements '()))
    (dolist (stmt statements)
      (push stmt main-statements))
    (make-program-node "Main"
                       :data (when main-statements
                               (list (cons 'main-block (nreverse main-statements)))))))

(defun burgermistress-parse-if-then (cond then-stmt)
  "IF condition THEN statement"
  (make-if-node cond (list then-stmt) '()))

(defun burgermistress-parse-if-then-else (cond then-stmt else-stmt)
  "IF condition THEN statement ELSE statement"
  (make-if-node cond (list then-stmt) (list else-stmt)))

(defun burgermistress-parse-assignment (target expr)
  "target = expr"
  (make-move-node expr target))

(defun burgermistress-parse-for (var start end &optional step)
   "FOR var = start TO end [STEP step]"
   (make-perform-node
    (format nil "FOR-~A" var)
    :varying var
    :from start
    :by (or step 1)
    :until (make-conditional-gt (make-identifier var) end)))

(defun burgermistress-parse-while (cond stmts)
   "WHILE condition : statements"
   (make-perform-node "WHILE"
                      :until (make-conditional-not cond)))

(defun burgermistress-parse-do-loop (stmts until-cond)
   "DO : statements : LOOP UNTIL condition"
   (make-perform-node "DO-LOOP"
                      :until until-cond))

(defun burgermistress-parse-sub (name stmts)
  "SUB name : statements : END SUB"
  (make-method-node name :statements stmts))

(defun burgermistress-parse-function (name stmts)
  "FUNCTION name : statements : END FUNCTION"
  (make-method-node name :statements stmts))

(defun burgermistress-parse-call (name)
   "CALL name"
   (make-call-node name))

(defun burgermistress-parse-return ()
  "RETURN"
  (make-goback-node))

(defun burgermistress-parse-gosub (name)
   "GOSUB name"
   (make-call-node name))

(defun burgermistress-parse-exit ()
   "EXIT"
   (make-exit-method-node))

;;; Create the YACC parser
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *burgermistress-parser*
      (:start-symbol program)
      (:terminals (lparen rparen colon comma lbracket rbracket
                   plus minus times divide equal
                   lt gt le ge ne lshift rshift
                   if else then endif while wend for to step next
                   do loop until exit
                   sub end function call return gosub
                   and or not
                   dim as integer string boolean double
                   number string ident))
      
      (program
       (statement
        (lambda (s) (burgermistress-parse-program (list s))))
       (program statement
        (lambda (prog stmt) (burgermistress-parse-program (nconc prog (list stmt))))))
      
       (statement
        (if-statement)
        (assignment)
        (for-loop)
        (while-loop)
        (do-loop)
        (sub-def)
        (function-def)
        (call-stmt)
        (return-stmt)
        (gosub-stmt)
        (exit-stmt))
      
      (if-statement
       (if expression then statement
        (lambda (cond stmt) (burgermistress-parse-if-then cond stmt)))
       (if expression then statement else statement endif
        (lambda (cond then-s else-s) (burgermistress-parse-if-then-else cond then-s else-s))))
      
      (assignment
       (ident equal expression
        (lambda (id expr) (burgermistress-parse-assignment (make-identifier id) expr))))
      
      (for-loop
       (for ident equal expression to expression statement next
        (lambda (var start end stmt) (burgermistress-parse-for var start end))))
      
      (while-loop
       (while expression colon statement wend
        (lambda (cond stmt) (burgermistress-parse-while cond (list stmt)))))
      
      (do-loop
       (do colon statement loop until expression
        (lambda (stmt cond) (burgermistress-parse-do-loop (list stmt) cond))))
      
      (sub-def
       (sub ident colon statement end sub
        (lambda (name stmt) (burgermistress-parse-sub name (list stmt)))))
      
      (function-def
       (function ident colon statement end function
        (lambda (name stmt) (burgermistress-parse-function name (list stmt)))))
      
      (call-stmt
       (call ident
        (lambda (name) (burgermistress-parse-call name))))
      
      (return-stmt
       (return
        (lambda () (burgermistress-parse-return))))
      
      (gosub-stmt
       (gosub ident
        (lambda (name) (burgermistress-parse-gosub name))))
      
      (exit-stmt
       (exit
        (lambda () (burgermistress-parse-exit))))
      
      (expression
       (ident
        (lambda (id) (make-identifier id)))
        (number
         (lambda (n) (parse-integer n)))
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
        (expression and expression
         (lambda (l r) (make-conditional-and l r)))
        (expression or expression
         (lambda (l r) (make-conditional-or l r)))
        (not expression
         (lambda (e) (make-conditional-not e)))
        (lparen expression rparen
         (lambda (e) e))))))

(defun burgermistress-parser ()
  "Return the Burgermistress YACC parser function."
  *burgermistress-parser*)


