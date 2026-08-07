;; src/frontend-scumm/scumm-parser.lisp — YACC grammar for SCUMM → EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token list for SCUMM YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun scumm-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |[| |]| |;| |,|
              + - * / % = == != < > <= >=
              && || !
              if else endif while endwhile for endfor
              break continue return
              define set get put call exit goto label
              assert wait stop start fade restore save restart
              version to end
              number string ident))))

;;; Parser action functions for SCUMM

(defun scumm-parse-program (statements)
  "Top-level program node."
  (make-program-node "SCUMM" :data (when statements (list (cons 'main-block statements)))))

(defun scumm-parse-if (cond then-stmt &optional else-stmt)
  "if (cond) then-stmt [else else-stmt]"
  (make-if-node cond (list then-stmt) (if else-stmt (list else-stmt) '())))

(defun scumm-parse-while (cond body)
   "while (cond) body"
   (make-perform-node "WHILE" :until (make-conditional-not cond)))

(defun scumm-parse-for (var start end body)
   "for var = start to end body"
   (make-perform-node "FOR" :varying var :from start :by 1
                      :until (make-conditional-gt (make-identifier var) end)))

(defun scumm-parse-set (var expr)
  "set var = expr"
  (make-move-node expr (make-identifier var)))

(defun scumm-parse-define (name body)
  "define name to body end"
  (make-method-node name :statements (list body)))

(defun scumm-parse-call (name args)
   "call name(args)"
   (make-call-node name))

(defun scumm-parse-return (&optional val)
  "return [val]"
  (make-goback-node))

(defun scumm-parse-goto (label)
  "goto label"
  (make-goto-node label))

;;; Create the YACC parser
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *scumm-parser*
      (:start-symbol program)
      (:terminals (lparen rparen lbracket rbracket semicolon comma
                   plus minus times divide mod assign
                   equal ne lt gt le ge
                   and or not
                   if else endif while endwhile for endfor
                   break continue return
                   define set get put call exit goto label
                   assert wait stop start fade restore save restart
                   version to end
                   number string ident))
      
      (program
       (statement
        (lambda (s) (scumm-parse-program (list s))))
       (program statement
        (lambda (prog s) (nconc prog (list s)))))
      
      (statement
       (if-stmt
        (lambda (s) s))
       (while-stmt
        (lambda (s) s))
       (for-stmt
        (lambda (s) s))
       (define-stmt
        (lambda (s) s))
       (assignment
        (lambda (a) a))
       (call-stmt
        (lambda (c) c))
       (return-stmt
        (lambda (r) r))
       (goto-stmt
        (lambda (g) g))
       (expr-stmt
        (lambda (e) e)))
      
      (if-stmt
       (if lparen expression rparen statement
        (lambda (cond stmt) (scumm-parse-if cond stmt)))
       (if lparen expression rparen statement else statement endif
        (lambda (cond s1 s2) (scumm-parse-if cond s1 s2))))
      
      (while-stmt
       (while lparen expression rparen statement endwhile
        (lambda (cond stmt) (scumm-parse-while cond stmt))))
      
      (for-stmt
       (for ident assign expression to expression statement endfor
        (lambda (v s e stmt) (scumm-parse-for v s e stmt))))
      
      (define-stmt
       (define ident to statement end
        (lambda (name stmt) (scumm-parse-define name stmt))))
      
      (assignment
       (set ident assign expression semicolon
        (lambda (v e) (scumm-parse-set v e))))
      
      (call-stmt
       (call ident lparen rparen semicolon
        (lambda (name) (scumm-parse-call name '())))
       (call ident lparen expr-list rparen semicolon
        (lambda (name args) (scumm-parse-call name args))))
      
      (return-stmt
       (return semicolon
        (lambda () (scumm-parse-return)))
       (return expression semicolon
        (lambda (e) (scumm-parse-return e))))
      
      (goto-stmt
       (goto label semicolon
        (lambda (lbl) (scumm-parse-goto lbl))))
      
      (expr-stmt
       (expression semicolon
        (lambda (e) e)))
      
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
        (expression mod expression
         (lambda (l r) (list :mod l r)))
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
        (lambda (e) e))
       (ident lbracket expression rbracket
        (lambda (id e) (list :subscript id e))))
      
       (expr-list
        (expression
         (lambda (e) (list e)))
        (expr-list comma expression
         (lambda (el e) (nconc el (list e))))))))

(defun scumm-parser ()
  "Return the SCUMM YACC parser function."
  *scumm-parser*)

