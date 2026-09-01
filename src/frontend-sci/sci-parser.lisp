(in-package :eightbol)

(defun sci-parse-number (n)
  "Parse number N which may be decimal, hex (#x or 0x), octal (#o or 0o), 
  or binary (#b or 0b) format. Returns an integer."
  (cond
    ;; Hex: #xHEX or 0xHEX
    ((or (string-starts-with "#x" n :test #'string-equal)
         (string-starts-with "0x" n :test #'string-equal))
     (let ((hex-str (if (string-starts-with "#x" n :test #'string-equal)
                        (subseq n 2)
                        (subseq n 2))))
       (parse-integer hex-str :radix 16)))
    
    ;; Octal: #oOCT or 0oOCT
    ((or (string-starts-with "#o" n :test #'string-equal)
         (string-starts-with "0o" n :test #'string-equal))
     (let ((oct-str (if (string-starts-with "#o" n :test #'string-equal)
                        (subseq n 2)
                        (subseq n 2))))
       (parse-integer oct-str :radix 8)))
    
    ;; Binary: #bBIN or 0bBIN
    ((or (string-starts-with "#b" n :test #'string-equal)
         (string-starts-with "0b" n :test #'string-equal))
     (let ((bin-str (if (string-starts-with "#b" n :test #'string-equal)
                        (subseq n 2)
                        (subseq n 2))))
       (parse-integer bin-str :radix 2)))
    
    ;; Default: decimal
    (t (parse-integer n))))

(defun string-starts-with (prefix s &key (test #'equal))
  "Check if string S starts with PREFIX using TEST function."
  (and (>= (length s) (length prefix))
       (funcall test (subseq s 0 (length prefix)) prefix)))

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
  (make-perform-node "WHILE" :until (make-conditional-not cond) :body (ensure-list body)))

(defun sci-parse-for (var start end body)
  "for var start end body"
  (make-perform-node "FOR" :varying var :from start :by 1
                     :until (make-conditional-gt (make-identifier var) end) :body (ensure-list body)))

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

(defun sci-parse-print (args)
  "print args or (print arg1 arg2 ...)"
  (make-print-node args))

(defun sci-parse-dialogue (character text)
  "dialogue character text"
  (make-dialogue-node :speaker character :text text))

(defun sci-parse-say (character &rest args)
  "say character text [response1 response2 ...]"
  ;; Create dialogue node (say is alias for dialogue)
  (make-dialogue-node :speaker character :text (car args)))

(defun sci-parse-input (prompt &optional variable)
  "input prompt [variable]"
  ;; Create input node for user interaction
  (make-input-node (if variable (list variable) '())))

;;; Helper functions for standard EIGHTBOL node production

(defun sci-make-program-node (name &key data methods)
  "Create a program AST node."
  (list :program :name name :data data :methods (or methods nil)))

(defun sci-make-method-node (name &key statements)
  "Create a method/procedure AST node."
  (list :method :name name :statements (or statements nil)))

(defun sci-make-if-node (cond then-stmts &optional else-stmts)
  "Create an if-conditional AST node."
  (let ((node (list :if :condition cond :then then-stmts)))
    (when else-stmts (setf node (append node (list :else else-stmts))))
    node))

(defun sci-make-perform-node (name &key until varying from by body)
  "Create a perform/loop AST node."
  (let ((node (list :perform :name name)))
    (when varying (setf node (append node (list :varying varying))))
    (when from (setf node (append node (list :from from))))
    (when by (setf node (append node (list :by by))))
    (when until (setf node (append node (list :until until))))
    (when body (setf node (append node (list :body body))))
    node))

(defun sci-make-move-node (expr target)
  "Create a move/assignment AST node."
  (list :move :from expr :to target))

(defun sci-make-identifier (name)
  "Create an identifier AST node."
  (list :identifier :name name))

(defun sci-make-call-node (func)
  "Create a function call AST node."
  (list :call :target func))

(defun sci-make-invoke-node (object method &key returning args)
  "Create an invocation (method call) AST node."
  (declare (ignore args))
  (let ((node (list :invoke :object object :method method)))
    (when returning (setf node (append node (list :returning returning))))
    node))

(defun sci-make-goback-node ()
  "Create a return/goback AST node."
  (list :goback))

(defun sci-make-print-node (args)
  "Create a print AST node."
  (list :print :args args))

(defun sci-make-dialogue-node (&key speaker text responses)
  "Create a dialogue AST node."
  (let ((node (list :dialogue :speaker speaker :text text)))
    (when responses (setf node (append node (list :responses responses))))
    node))

(defun sci-make-input-node (&optional vars)
  "Create an input AST node."
  (list :input :variables (or vars nil)))

(defun sci-make-conditional-not (expr)
  "Create a NOT conditional node."
  (list :not expr))

(defun sci-make-conditional-gt (left right)
  "Create a > comparison node."
  (list :gt left right))

(defun sci-make-conditional-and (left right)
  "Create an AND node."
  (list :and left right))

(defun sci-make-conditional-or (left right)
  "Create an OR node."
  (list :or left right))

(defun sci-make-conditional-eq (left right)
  "Create an = comparison node."
  (list :eq left right))

(defun sci-make-conditional-ne (left right)
  "Create a <> (not equal) node."
  (list :neq left right))

(defun sci-make-conditional-lt (left right)
  "Create a < comparison node."
  (list :lt left right))

(defun sci-make-conditional-le (left right)
  "Create a <= comparison node."
  (list :le left right))

(defun sci-make-conditional-ge (left right)
  "Create a >= comparison node."
  (list :ge left right))

(defun make-expression-add (left right)
  "Create an ADD expression node."
  (list :add :from left :to right))

(defun make-expression-subtract (left right)
  "Create a SUBTRACT expression node."
  (list :subtract :from left :from-target right))

(defun make-expression-multiply (left right)
  "Create a MULTIPLY expression node."
  (list :compute :target 'result :expression (list :* left right)))

(defun make-expression-divide (left right)
  "Create a DIVIDE expression node."
  (list :compute :target 'result :expression (list :/ left right)))

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
                   :print :dialogue :dialog :say :input :ask :format
                   :strcat :strlen :substr :upcase :downcase
                   :numtostr :strtonum
                   number hex octal binary dword string ident))

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
        (lambda (n) (sci-parse-number n)))
       (hex
        (lambda (h) (sci-parse-number (concatenate 'string "#x" h))))
       (octal
        (lambda (o) (sci-parse-number (concatenate 'string "#o" o))))
       (binary
        (lambda (b) (sci-parse-number (concatenate 'string "#b" b))))
       (dword
        (lambda (d) d))
       (string
        (lambda (s) s)))

      (sexp
       (lparen atom rparen
        (lambda (a) a))
       (lparen atom form-list rparen
        (lambda (f args) (list f args)))
       (lparen :print form-list rparen
        (lambda (args) (sci-parse-print args)))
       (lparen :dialogue string string rparen
        (lambda (char text) (sci-parse-dialogue char text)))
       (lparen :dialogue string string form-list rparen
        (lambda (char text resps) (list :dialogue :character char :text text :responses resps)))
       (lparen :say string string rparen
        (lambda (char text) (sci-parse-say char text)))
       (lparen :input string rparen
         (lambda (prompt) (sci-parse-input prompt)))
        (lparen :input string ident rparen
         (lambda (prompt var) (sci-parse-input prompt var)))
        (lparen :while expression form rparen
         (lambda (_while cond body _rparen) (declare (ignore _while _rparen)) (sci-parse-while cond body)))
        (lparen :while expression form-list rparen
         (lambda (_while cond body _rparen) (declare (ignore _while _rparen)) (sci-parse-while cond (ensure-list body))))
        (lparen :for lparen ident expression expression rparen form rparen
         (lambda (_for _lparen var start end _rparen body _rparen2) (declare (ignore _for _lparen _rparen _rparen2)) (sci-parse-for var start end body)))
        (lparen :for lparen ident expression expression rparen form-list rparen
         (lambda (_for _lparen var start end _rparen body _rparen2) (declare (ignore _for _lparen _rparen _rparen2)) (sci-parse-for var start end (ensure-list body)))))

      (expression
       (ident
        (lambda (id) (make-identifier id)))
       (number
        (lambda (n) (sci-parse-number n)))
       (hex
        (lambda (h) (sci-parse-number (concatenate 'string "#x" h))))
       (octal
        (lambda (o) (sci-parse-number (concatenate 'string "#o" o))))
       (binary
        (lambda (b) (sci-parse-number (concatenate 'string "#b" b))))
       (dword
        (lambda (d) d))
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
