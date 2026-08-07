;; src/frontend-muddle/muddle-parser.lisp — YACC grammar for Muddle → EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token list for Muddle YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun muddle-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |,|
              + - * / ^ = /= < > <= >=
              :.if :.else :.then :.endif
              :.while :.endwhile :.for :.endfor
              :.exit :.return :.define :.set :.get :.put
              :.call :.go :.throw :.catch :.finally
              and or not
              number string ident))))

;;; Parser action functions for Muddle

(defun muddle-parse-program (forms)
  "Top-level program node from list of forms."
  (make-program-node "MDL" :data (when forms (list (cons 'main-block forms)))))

(defun muddle-parse-if (cond then-form &optional else-form)
  "(.IF cond then-form else-form)"
  (make-if-node cond (list then-form) (if else-form (list else-form) '())))

(defun muddle-parse-while (cond body)
   "(.WHILE cond body)"
   (make-perform-node "WHILE" :until (make-conditional-not cond)))

(defun muddle-parse-for (var start end body)
   "(.FOR var start end body)"
   (make-perform-node "FOR" :varying var :from start :by 1
                      :until (make-conditional-gt (make-identifier var) end)))

(defun muddle-parse-define (name params body)
  "(.DEFINE name params body)"
  (make-method-node name :statements (list body)))

(defun muddle-parse-set (var expr)
  "(.SET var expr)"
  (make-move-node expr (make-identifier var)))

(defun muddle-parse-get (obj prop)
  "(.GET obj prop)"
  (make-identifier (format nil "~A.~A" obj prop)))

(defun muddle-parse-call (func &rest args)
   "(.CALL func args...)"
   (make-call-node func))

(defun muddle-parse-return (&optional val)
  "(.RETURN val)"
  (make-goback-node))

;;; Create the YACC parser
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *muddle-parser*
      (:start-symbol program)
      (:terminals (lparen rparen comma
                   plus minus times divide power equal ne
                   lt gt le ge
                   :.if :.else :.then :.endif
                   :.while :.endwhile :.for :.endfor
                   :.exit :.return :.define :.set :.get :.put
                   :.call :.go :.throw :.catch :.finally
                   and or not
                   number string ident))
      
      (program
       (form
        (lambda (f) (muddle-parse-program (list f))))
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
        (expression power expression
         (lambda (l r) (make-expression-bit-xor l r)))
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

(defun muddle-parser ()
   "Return the Muddle YACC parser function."
   *muddle-parser*)

