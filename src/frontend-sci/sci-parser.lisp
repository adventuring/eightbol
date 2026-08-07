;; src/frontend-sci/sci-parser.lisp — YACC grammar for SCI → EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token list for SCI YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun sci-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |,| |'|
              + - * / = <> < > <= >=
              and or not
              :if :else :then :while :for :foreach
              :break :continue :return
              :define :setq :set :get :put :call
              :proc :method :class :instance
              :send :super :self
              :print :format
              :strcat :strlen :substr :upcase :downcase
              :numtostr :strtonum
              number string ident))))

;;; Parser action functions for SCI

(defun sci-parse-program (forms)
  "Top-level program node from list of forms."
  (make-program-node "SCI" :data (when forms (list (cons 'main-block forms)))))

(defun sci-parse-if (cond then-form &optional else-form)
  "if condition then-form [else else-form]"
  (make-if-node cond (list then-form) (if else-form (list else-form) '())))

(defun sci-parse-while (cond body)
  "while condition body"
  (make-perform-node "WHILE" :until (make-conditional-not cond)))

(defun sci-parse-for (var start end body)
  "for var start end body"
  (make-perform-node "FOR" :varying var :from start :by 1
                     :until (make-conditional-gt (make-identifier var) end)))

(defun sci-parse-setq (var expr)
  "setq var expr"
  (make-move-node expr (make-identifier var)))

(defun sci-parse-define (name params body)
  "(define name params body)"
  (make-method-node name :statements (list body)))

(defun sci-parse-call (func args)
  "(call func args)"
  (make-call-node func))

(defun sci-parse-send (obj method args)
  "(send obj method args)"
  (make-invoke-node (make-identifier obj) method :returning (when args (car args))))

(defun sci-parse-return (&optional val)
  "return [val]"
  (make-goback-node))

;;; Create the YACC parser
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *sci-parser*
      (:start-symbol program)
      (:terminals (lparen rparen comma quote
                   plus minus times divide equal ne
                   lt gt le ge
                   and or not
                   :if :else :then :while :for :foreach
                   :break :continue :return
                   :define :setq :set :get :put :call
                   :proc :method :class :instance
                   :send :super :self
                   :print :format
                   :strcat :strlen :substr :upcase :downcase
                   :numtostr :strtonum
                   number string ident))

      (program
       (form
        (lambda (f) (sci-parse-program (list f))))
       (program form
        (lambda (prog f) (nconc prog (list f)))))

      (form
       (atom
        (lambda (a) a))
       (sexp
        (lambda (s) s)))

      (atom
       (ident
        (lambda (id) (make-identifier id)))
       (number
        (lambda (n) (parse-integer n)))
       (string
        (lambda (s) s)))

      (sexp
       (lparen atom rparen
        (lambda (a) a))
       (lparen atom form-list rparen
        (lambda (f args) (list f args))))

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
        (lambda (f) (make-conditional-not f))))

      (form-list
       (form
        (lambda (f) (list f)))
       (form-list form
        (lambda (fl f) (nconc fl (list f))))))))

(defun sci-parser ()
  "Return the SCI YACC parser function."
  *sci-parser*)
