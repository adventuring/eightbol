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
              :|<GO>| :|<EXIT>| :|<LABEL>| :|<ASSERT>| :|<WAIT>|
              :|<STOP>| :|<START>| :|<FADE>| :|<RESTORE>| :|<SAVE>| :|<RESTART>|
              :|<VERSION>| :|<OBJECT>| :|<PROPERTY>| :|<GLOBAL>| :|<LOCAL>|
              :|<P>|
              number string atom))))

;;; Parser action functions for ZIL

(defun zil-parse-program (forms)
  "Top-level program node."
  (make-program-node "ZIL" :data (when forms (list (cons 'main-block forms)))))

(defun zil-parse-if (cond then-form &optional else-form)
  "(<IF> cond then-form [else-form])"
  (make-if-node cond (list then-form) (if else-form (list else-form) '())))

(defun zil-parse-while (cond body)
   "(<WHILE> cond body)"
   (make-perform-node "WHILE" :until (make-conditional-not cond)))

(defun zil-parse-for (var start end body)
   "(<FOR> var start end body)"
   (make-perform-node "FOR" :varying var :from start :by 1
                      :until (make-conditional-gt (make-identifier var) end)))

(defun zil-parse-define (name params body)
  "(<DEFINE> name params body)"
  (make-method-node name :statements (list body)))

(defun zil-parse-set (var expr)
  "(<SET> var expr)"
  (make-move-node expr (make-identifier var)))

(defun zil-parse-setg (var expr)
  "(<SETG> var expr)"
  (make-move-node expr (make-identifier var)))

(defun zil-parse-call (func args)
   "(<CALL> func args)"
   (make-call-node func))

(defun zil-parse-return (&optional val)
  "(<RETURN> [val])"
  (make-goback-node))

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
                   :|<GO>| :|<EXIT>| :|<LABEL>| :|<ASSERT>| :|<WAIT>|
                   :|<STOP>| :|<START>| :|<FADE>| :|<RESTORE>| :|<SAVE>| :|<RESTART>|
                   :|<VERSION>| :|<OBJECT>| :|<PROPERTY>| :|<GLOBAL>| :|<LOCAL>|
                   :|<P>|
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
         (lambda (n) (parse-integer n)))
        (string
         (lambda (s) s))
       (sexp
        (lambda (s) s)))
      
      (sexp
       (lparen atom rparen
        (lambda (a) a))
       (lparen atom form-list rparen
        (lambda (f args) (list f args))))
      
      (expression
       (atom
        (lambda (a) a))
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


