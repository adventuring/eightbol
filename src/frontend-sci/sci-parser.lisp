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
  (list :if :condition cond :then (list then-form) :else (if else-form (list else-form) '())))

(defun sci-parse-while (cond body)
  "while condition body"
  (list :perform :procedure "WHILE" :until (list :not cond) :body (ensure-list body)))

(defun sci-parse-for (var start end body)
  "for var start end body"
  (list :perform :procedure "FOR" :varying var :from start :by 1
        :until (list :< var end) :body (ensure-list body)))

(defun sci-parse-setq (var expr)
  "setq var expr"
  (list :move :from expr :to var))

(defun sci-parse-define (name params body)
  "(define name params body)"
  (make-method-node name :statements (list body)))

(defun sci-parse-call (func args)
  "(call func args)"
  (list :call :target func))

(defun sci-parse-send (obj method args)
  "(send obj method args)"
  (list :invoke :object obj :method method))

(defun sci-parse-return (&optional val)
  "return [val]"
  (if val
      (list :goback :value val)
      (list :goback)))

(defun sci-parse-print (args)
  "print args or (print arg1 arg2 ...)"
  (list :print :expressions (if (listp args) args (list args))))

(defun sci-parse-dialogue (character text)
  "dialogue character text"
  (list :dialogue :speaker character :text text))

(defun sci-parse-say (character &rest args)
  "say character text [response1 response2 ...]"
  (list :dialogue :speaker character :text (car args)))

(defun sci-parse-input (prompt &optional variable)
  "input prompt [variable]"
  (list :input :variables (if variable (list variable) '()) :prompt prompt))

;;; Helper functions for standard EIGHTBOL node production

(defun sci-make-program-node (name &key data methods)
  "Create a program AST node."
  (list :program :name name :data data :methods (or methods nil)))

(defun sci-make-method-node (name &key statements)
  "Create a method/procedure AST node."
  (list :method :name name :statements (or statements nil)))

(defun sci-make-if-node (cond then-stmts &optional else-stmts)
  "Create an if-conditional AST node."
  (list :if :condition cond :then then-stmts :else (or else-stmts '())))

(defun sci-make-perform-node (name &key until varying from by body)
  "Create a perform/loop AST node."
  (list* :perform :procedure name
         (append (when varying `(:varying ,varying))
                 (when from `(:from ,from))
                 (when by `(:by ,by))
                 (when until `(:until ,until))
                 (when body `(:body ,body)))))

(defun sci-make-move-node (expr target)
  "Create a move/assignment AST node."
  (list :move :from expr :to target))

(defun sci-make-identifier (name)
  "Create an identifier AST node."
  (string name))

(defun sci-make-call-node (func)
  "Create a function call AST node."
  (list :call :target func))

(defun sci-make-invoke-node (class method &key args)
  "Create an invocation (method call) AST node."
  (declare (ignore args))
  (list :invoke :object class :method method))

(defun sci-make-goback-node ()
  "Create a return/goback AST node."
  (list :goback))

(defun sci-make-print-node (args)
  "Create a print AST node."
  (list :print :expressions (ensure-list args)))

(defun sci-make-dialogue-node (&key speaker text responses)
  "Create a dialogue AST node."
  (list* :dialogue :speaker speaker :text text
         (when responses `(:responses ,responses))))

(defun sci-make-input-node (&optional vars)
  "Create an input AST node."
  (list :input :variables (or vars nil)))

(defun sci-make-conditional-not (expr)
  "Create a NOT conditional node."
  (list :not expr))

(defun sci-make-conditional-gt (left right)
  "Create a > comparison node."
  (list :> left right))

(defun sci-make-conditional-and (left right)
  "Create an AND node."
  (list :and left right))

(defun sci-make-conditional-or (left right)
  "Create an OR node."
  (list :or left right))

(defun sci-make-conditional-eq (left right)
  "Create an = comparison node."
  (list := left right))

(defun sci-make-conditional-ne (left right)
  "Create a <> (not equal) node."
  (list :≠ left right))

(defun sci-make-conditional-lt (left right)
  "Create a < comparison node."
  (list :< left right))

(defun sci-make-conditional-le (left right)
  "Create a <= comparison node."
  (list :≤ left right))

(defun sci-make-conditional-ge (left right)
  "Create a >= comparison node."
  (list :≥ left right))

(defun sci-make-expression-add (left right)
  "Create an ADD expression node."
  (list :+ left right))

(defun sci-make-expression-subtract (left right)
  "Create a SUBTRACT expression node."
  (list :- left right))

(defun sci-make-expression-multiply (left right)
  "Create a MULTIPLY expression node."
  (list :× left right))

(defun sci-make-expression-divide (left right)
  "Create a DIVIDE expression node."
  (list :÷ left right))

;;; Additional statement node constructors for full 56-node coverage

(defun sci-make-exit-method-node (&optional value)
  "Create an EXIT METHOD node."
  (if value
      (list :goback :value value)
      (list :goback)))

(defun sci-make-exit-program-node ()
  "Create an EXIT PROGRAM node."
  (list :goback))

(defun sci-make-exit-node ()
  "Create an EXIT node."
  (list :goback))

(defun sci-make-stop-run-node ()
  "Create a STOP RUN node."
  (list :goback))

(defun sci-make-set-node (target value)
  "Create a SET identifier TO value node."
  (list :move :from value :to target))

(defun sci-make-compute-node (target expression)
  "Create a COMPUTE target FROM expression node."
  (list :move :from expression :to target))

(defun sci-make-call-acc-node (func value)
  "Create a CALL with :using clause node."
  (list :call-acc :target func :using value))

(defun sci-make-log-fault-node (code)
  "Create a LOG FAULT code node."
  (list :log-fault :code code))

(defun sci-make-debug-break-node (code)
  "Create a DEBUG BREAK code node."
  (list :debug-break :code code))

(defun sci-make-copy-node (name)
  "Create a COPY name node."
  (list :copy :name name))

(defun sci-make-string-blt-node (source dest &key length)
  "Create a STRING BLT (string move) node."
  (list* :string-blt :source source :dest dest
         (when length `(:length ,length))))

(defun sci-make-subscript-node (base index)
  "Create a subscripted access node."
  (list :subscript :name base :index index))

(defun sci-make-of-node (slot object)
  "Create a qualified identifier (slot OF object) node."
  (list :of :slot slot :object object))

(defun sci-make-self-node ()
  "Create a SELF reference node."
  :self)

(defun sci-make-null-node ()
  "Create a NULL value node."
  :null)

(defun sci-make-address-of-node (identifier)
  "Create an ADDRESS OF identifier node."
  (list :address-of :name identifier))

(defun sci-make-refmod-node (base start length)
  "Create a reference modification base(start:length) node."
  (list :refmod :base base :start start :length length))

;;; Bitwise and shift operators

(defun sci-make-bitwise-not (value)
  "Create a bitwise NOT expression."
  (list :¬ value))

(defun sci-make-bitwise-and (left right)
  "Create a bitwise AND expression."
  (list :∧ left right))

(defun sci-make-bitwise-or (left right)
  "Create a bitwise OR expression."
  (list :∨ left right))

(defun sci-make-bitwise-xor (left right)
  "Create a bitwise XOR expression."
  (list :⊻ left right))

(defun sci-make-bitwise-nand (left right)
  "Create a bitwise NAND expression."
  (list :¬ (list :∧ left right)))

(defun sci-make-bitwise-nor (left right)
  "Create a bitwise NOR expression."
  (list :¬ (list :∨ left right)))

(defun sci-make-shift-arithmetic (value amount)
  "Create an arithmetic shift expression."
  (list :ash value amount))

(defun sci-make-shift-left (value amount)
  "Create a shift left expression."
  (list :ash value amount))

(defun sci-make-shift-right (value amount)
  "Create a shift right expression."
  (list :ash value (- amount)))

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
        (lambda (id) (string id)))
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
        (lambda (id) (string id)))
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
        (lambda (l r) (sci-make-expression-add l r)))
       (expression minus expression
        (lambda (l r) (sci-make-expression-subtract l r)))
       (expression times expression
        (lambda (l r) (sci-make-expression-multiply l r)))
       (expression divide expression
        (lambda (l r) (sci-make-expression-divide l r)))
       (expression equal expression
        (lambda (l r) (sci-make-conditional-eq l r)))
       (expression ne expression
        (lambda (l r) (sci-make-conditional-ne l r)))
       (expression lt expression
        (lambda (l r) (sci-make-conditional-lt l r)))
       (expression gt expression
        (lambda (l r) (sci-make-conditional-gt l r)))
       (expression le expression
        (lambda (l r) (sci-make-conditional-le l r)))
       (expression ge expression
        (lambda (l r) (sci-make-conditional-ge l r)))
       (expression and expression
        (lambda (l r) (sci-make-conditional-and l r)))
       (expression or expression
        (lambda (l r) (sci-make-conditional-or l r)))
       (not expression
        (lambda (f) (sci-make-conditional-not f))))

      (form-list
       (form
        (lambda (f) (list f)))
       (form-list form
        (lambda (fl f) (nconc fl (list f))))))))

(defun sci-parser ()
  "Return the SCI YACC parser function."
  *sci-parser*)
