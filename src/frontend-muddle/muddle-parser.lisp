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
              :.print :.display :.input :.accept :.dialogue
              and or not
              number string ident))))

;;; Parser action functions for Muddle

(defun muddle-normalize-identifier-to-kebab (s)
  "Convert identifier S to kebab-case form (e.g., MyVariable -> my-variable).
   Identifier is assumed to already be normalized from the lexer."
  (if (stringp s) s (format nil "~A" s)))

(defun muddle-make-number-literal (text)
  "Parse number literal TEXT using the lexer's parser."
  (muddle-parse-number text))

(defun muddle-parse-program (forms)
  "Top-level program node from list of forms."
  (make-program-node "MDL" :data (when forms (list (cons 'main-block forms)))))

(defun muddle-parse-if (cond then-form &optional else-form)
  "(.IF cond then-form else-form) - Conditional branching."
  (make-if-node cond (list then-form) (if else-form (list else-form) '())))

(defun muddle-parse-while (cond body)
  "(.WHILE cond body) - Looping until condition becomes false."
  (make-perform-node (if (listp body) body (list body))
                     :until (make-conditional-not cond)))

(defun muddle-parse-for (var start end body)
  "(.FOR var start end body) - Counted loop from start to end."
  (make-perform-node (if (listp body) body (list body))
                     :varying var
                     :from start
                     :by 1
                     :until (make-conditional-gt (make-identifier var) end)))

(defun muddle-parse-define (name params body)
  "(.DEFINE name params body) - Function/method definition."
  (make-method-node name :statements (if (listp body) body (list body))))

(defun muddle-parse-set (var expr)
"(.SET var expr) - Assignment."
(make-move-node expr (make-identifier var)))

(defun muddle-parse-get (obj prop)
  "(.GET obj prop) - Property access."
  (list :of prop obj))

(defun muddle-parse-put (obj prop value)
  "(.PUT obj prop value) - Property assignment."
  (make-move-node value (make-identifier (format nil "~A.~A" obj prop))))

(defun muddle-parse-call (func &rest args)
  "(.CALL func args...) - Function invocation."
  (let ((call-node (make-call-node func)))
    ;; If there are arguments, wrap them somehow
    ;; This depends on how the AST structure handles arguments
    call-node))

(defun muddle-parse-return (&optional val)
  "(.RETURN val) - Return from function."
  (make-goback-node))

(defun muddle-parse-print (&rest args)
  "(.PRINT arg1 arg2...) or (.DISPLAY arg1 arg2...) - Output text/values."
  ;; Create an assembly entry node or use error node for now
  ;; In real implementation, this would emit print/display code
  (make-move-node (car args) (make-identifier "OUTPUT")))

(defun muddle-parse-input (&rest vars)
  "(.INPUT var1 var2...) or (.ACCEPT var1 var2...) - Read input values."
  ;; In real implementation, this would emit input/accept code
  (make-move-node (make-identifier "INPUT") (make-identifier (car vars))))

(defun muddle-parse-dialogue (text)
  "(.DIALOGUE \"text\") - Display dialogue text.
   
   This creates a display instruction for dialogue."
  (make-move-node text (make-identifier "DIALOGUE")))

(defun muddle-parse-exit (&optional code)
  "(.EXIT code) - Exit program with optional status code."
  (make-stop-run-node (or code 0)))

(defun muddle-parse-throw (code)
  "(.THROW code) - Throw a fault with code."
  (make-log-fault-node code))

(defun muddle-parse-go (label)
  "(.GO label) - Unconditional jump (legacy)."
  (make-goto-node label))

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
                          :.print :.display :.input :.accept :.dialogue
                          and or not
                          number string ident))
      
      (program
       (empty
        (lambda () (muddle-parse-program '())))
       (form
        (lambda (f) (muddle-parse-program (list f))))
       (program form
                (lambda (prog f) (append prog (list f)))))
      
      (empty)
      
      (form
       (atom
        (lambda (a) a))
       (sexp
        (lambda (s) s)))
      
      (atom
       (ident
        (lambda (id) (make-identifier id)))
       (number
        (lambda (n) (muddle-make-number-literal n)))
       (string
        (lambda (s) s)))
      
      (sexp
       (lparen atom rparen
               (lambda (a) a))
       (lparen atom form-list rparen
               (lambda (f args) (list f args)))
       ;; .IF cond then-form [else-form]
       (lparen :.if expression form form rparen
               (lambda (condition then else) (muddle-parse-if condition then else)))
       (lparen :.if expression form rparen
               (lambda (condition then) (muddle-parse-if condition then)))
       ;; .WHILE cond body
       (lparen :.while expression form rparen
               (lambda (c b) (muddle-parse-while c b)))
       ;; .FOR var start end body
       (lparen :.for ident atom atom form rparen
               (lambda (v s e b) (muddle-parse-for v s e b)))
       ;; .DEFINE name params body
       (lparen :.define ident form form rparen
               (lambda (n p b) (muddle-parse-define n p b)))
       ;; .SET var expr
       (lparen :.set ident expression rparen
               (lambda (v e) (muddle-parse-set v e)))
       ;; .GET obj prop
       (lparen :.get ident ident rparen
               (lambda (o p) (muddle-parse-get o p)))
       ;; .PUT obj prop value
       (lparen :.put ident ident expression rparen
               (lambda (o p v) (muddle-parse-put o p v)))
       ;; .CALL func [args...]
       (lparen :.call ident rparen
               (lambda (f) (muddle-parse-call f)))
       (lparen :.call ident form-list rparen
               (lambda (f args) (apply #'muddle-parse-call f args)))
       ;; .RETURN [value]
       (lparen :.return rparen
               (lambda () (muddle-parse-return)))
       (lparen :.return expression rparen
               (lambda (v) (muddle-parse-return v)))
       ;; .PRINT [args...]
       (lparen :.print rparen
               (lambda () (muddle-parse-print)))
       (lparen :.print form-list rparen
               (lambda (args) (apply #'muddle-parse-print args)))
       ;; .DISPLAY [args...]
       (lparen :.display rparen
               (lambda () (muddle-parse-print)))
       (lparen :.display form-list rparen
               (lambda (args) (apply #'muddle-parse-print args)))
       ;; .INPUT [vars...]
       (lparen :.input rparen
               (lambda () (muddle-parse-input)))
       (lparen :.input form-list rparen
               (lambda (vars) (apply #'muddle-parse-input vars)))
       ;; .ACCEPT [vars...]
       (lparen :.accept rparen
               (lambda () (muddle-parse-input)))
       (lparen :.accept form-list rparen
               (lambda (vars) (apply #'muddle-parse-input vars)))
       ;; .DIALOGUE "text"
       (lparen :.dialogue string rparen
               (lambda (s) (muddle-parse-dialogue s)))
       ;; .EXIT [code]
       (lparen :.exit rparen
               (lambda () (muddle-parse-exit)))
       (lparen :.exit expression rparen
               (lambda (c) (muddle-parse-exit c)))
       ;; .THROW code
       (lparen :.throw expression rparen
               (lambda (c) (muddle-parse-throw c)))
       ;; .GO label
       (lparen :.go ident rparen
               (lambda (l) (muddle-parse-go l))))
      
      (expression
       (atom
        (lambda (a) a))
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
       (and expression expression
            (lambda (l r) (make-conditional-and l r)))
       (and form-list
            (lambda (forms) 
              (reduce (lambda (acc f) (make-conditional-and acc f)) (cdr forms) :initial-value (car forms))))
       (or expression expression
           (lambda (l r) (make-conditional-or l r)))
       (or form-list
           (lambda (forms)
             (reduce (lambda (acc f) (make-conditional-or acc f)) (cdr forms) :initial-value (car forms))))
       (not expression
            (lambda (f) (make-conditional-not f)))
       (lparen expression rparen
               (lambda (e) e)))
      
      (form-list
       (form
        (lambda (f) (list f)))
       (form-list form
                  (lambda (fl f) (append fl (list f))))))))

(defun muddle-parser ()
  "Return the Muddle YACC parser function."
  *muddle-parser*)
