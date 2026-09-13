;;; lua-parser.lisp
;;; YACC parser for Lua subset
;;; Maps Lua tokens to EightBol AST nodes
;;; Supports: dialogue, print, input, if/while/for, method calls, assignments

(in-package :eightbol)

(defun lua-token-list ()
  "Define Lua token types compatible with EightBol lexer."
  (mapcar (compose #'intern #'string)
          '(|(| |)| |{| |}| |[| |]| |\:| :comma + - * / = == ~= < <= > >= 
            keyword symbol number string comment)))

;;; Parser definition using YACC
(yacc:define-parser *lua-parser*
      (:start-symbol program)
      (:terminals (number string ident :comma |=| |+| |-| |*| |/| |\|\|| |&| |~| |\^| |<<| |>>| 
                          |<| |>| |<=| |>=| |==| |~=| |(| |)| |{| |}| |[| |]| |\:|
                          |\;| keyword |break| |continue| |comment| |move| |to|
                          |if| |then| |else| |elseif| |end| |while| |do| |for|
                          |local| |return| |dialogue| |dialog| |print| |input| |copy|
                          |true| |false| |nil| |and| |or| |not| |function|))
                          (:precedence ((:left :|or|) (:left :|and|)
                                                       (:left :|==| :|~=| :|<| :|>| :|<=| :|>=|)
                               (:left :|+| :|-| )
                               (:left :|*| :|/| )
                               (:left :|<<| :|>>|)
                               (:left :|&|)
                               (:left :|^|)
                               (:right :|not| :|~|)))

    (:muffle-conflicts t)

   ;; Top-level program: list of statements
   (program
    (statement-list
     (lambda (stmts) (list :program :statements stmts)))
    (nil
     (lambda () (list :program :statements nil))))

   ;; Statement list
   (statement-list
    (statement
     (lambda (stmt) (remove nil (list stmt))))
    (statement-list statement
                    (lambda (stmts stmt)
                      (append stmts (remove nil (list stmt))))))

     ;; Statements
     (statement
      (nil)
      (local-declaration)
      (function-declaration)
      (if-statement)
      (while-statement)
      (for-statement)
      (return-statement)
      (expression-statement)
      (dialogue-statement)
      (print-statement)
      (input-statement)
        (copy-statement)
        (break-statement)
        (continue-statement)
        (move-statement))

    ;; Local variable declaration
    (local-declaration
     (|local| ident |=| expression
              (lambda (_local var _eq expr)
                (declare (ignore _local _eq))
                (list :move :from expr :to var))))

    ;; Function declaration
    (function-declaration
     (|function| ident |(| |)| statement-list |end|
                 (lambda (_func name _lp _rp body _end)
                   (declare (ignore _func _lp _rp _end))
                   (list :procedure name :body body))))


   ;; If statement
   (if-statement
    (|if| expression |then| statement-list |end|
          (lambda (_if cond _then body _end)
            (declare (ignore _if _then _end))
            (list :if :condition cond :then body)))
    (|if| expression |then| statement-list |else| statement-list |end|
          (lambda (_if cond _then then-body _else else-body _end)
            (declare (ignore _if _then _else _end))
            (list :if :condition cond :then then-body :else else-body)))
    (|if| expression |then| statement-list |elseif| expression |then| statement-list |end|
          (lambda (_if cond1 _then body1 _elseif cond2 _then2 body2 _end)
            (declare (ignore _if _then _elseif _then2 _end))
            (list :if :condition cond1 :then body1
                      :else (list (list :if :condition cond2 :then body2))))))

   ;; While statement
   (while-statement
    (|while| expression |do| statement-list |end|
             (lambda (_while cond _do body _end)
               (declare (ignore _while _do _end))
               (list :perform :body body :until (list :not cond)))))

   ;; For statement (simple numeric range)
   (for-statement
    (|for| ident |=| expression :comma expression |do| statement-list |end|
           (lambda (_for var _eq from _comma to _do body _end)
             (declare (ignore _for _eq _comma _do _end))
             (list :perform :body body :varying var :from from :to to)))
    (|for| ident |=| expression :comma expression :comma expression |do| statement-list |end|
           (lambda (_for var _eq from _comma1 to _comma2 by _do body _end)
             (declare (ignore _for _eq _comma1 _comma2 _do _end))
             (list :perform :body body :varying var :from from :to to :by by))))

   ;; Return statement — emit canonical :goback
   (return-statement
    (|return|
     (lambda (_ret)
       (declare (ignore _ret))
       (list :goback)))
    (|return| expression
              (lambda (_ret expr)
                (declare (ignore _ret))
                (list :goback :value expr))))

   ;; Expression statement (e.g., function call, method call, assignment)
   (expression-statement
    (expression))

   ;; Dialogue statement
   (dialogue-statement
    (|dialogue| string
                (lambda (_dial msg)
                  (declare (ignore _dial))
                  (list :dialogue :text msg)))
    (|dialog| string
              (lambda (_dial msg)
                (declare (ignore _dial))
                (list :dialogue :text msg))))

   ;; Print statement — emit canonical :print node
   (print-statement
    (|print| expression
             (lambda (_print expr)
               (declare (ignore _print))
               (make-print-node (list expr))))
    (|print| |\|\|| expression
             (lambda (_print _pipe expr)
               (declare (ignore _print _pipe))
               (make-print-node (list expr))))
    (|print| expression print-args
             (lambda (_print expr rest)
               (declare (ignore _print))
               (make-print-node (cons expr rest)))))

      ;; Print arguments
      
      (print-args
       (:comma expression
               (lambda (_comma expr)
                 (declare (ignore _comma))
                 (list expr)))
       (print-args :comma expression
                  (lambda (prev _comma expr)
                    (declare (ignore _comma))
                     (append prev (list expr)))))

      ;; Input statement — emit canonical :input node with address-of wrapped vars
      (input-statement
       (|input| ident
                (lambda (_inp var)
                  (declare (ignore _inp))
                  (make-input-node (list (list :address-of var)))))
       (|input| ident input-args
                (lambda (_inp var rest)
                  (declare (ignore _inp))
                  (make-input-node (cons (list :address-of var) rest)))))

      ;; Input arguments
      
      (input-args
       (:comma ident
               (lambda (_comma var)
                 (declare (ignore _comma))
                 (list (list :address-of var))))
       (input-args :comma ident
                   (lambda (prev _comma var)
                     (declare (ignore _comma))
                     (append prev (list (list :address-of var))))))

        ;; Copy statement
        
        (copy-statement
         (|copy| |(| string |)| |;|
                 (lambda (_copy _lp filename _rp _semi)
                   (declare (ignore _copy _lp _rp _semi))
                   (list :copy :name filename)))
         (|copy| |(| string |)|
                 (lambda (_copy _lp filename _rp)
                   (declare (ignore _copy _lp _rp))
                   (list :copy :name filename))))

        ;; Break statement (for loops/perform)
       
       (break-statement
        (|break|
         (lambda (_break)
           (declare (ignore _break))
           (list :break))))

       ;; Continue statement (for loops/perform)
       
       (continue-statement
        (|continue|
         (lambda (_continue)
           (declare (ignore _continue))
           (list :continue))))

       ;; Move statement (direct assignment/copy to another variable)
       
       (move-statement
        (|move| expression |to| ident
                (lambda (_move expr _to target)
                  (declare (ignore _move _to))
                  (list :move :from expr :to target))))

       ;; Expressions
      
      (expression
       (number)
       (string)
       (ident)
       (|true|
        (lambda (_true)
          (declare (ignore _true))
          1))
       (|false|
        (lambda (_false)
          (declare (ignore _false))
          0))
       (|nil|
        (lambda (_nil)
          (declare (ignore _nil))
          nil))
        (|(| expression |)|
             (lambda (_lp expr _rp)
               (declare (ignore _lp _rp))
               expr))
         (expression |+| expression
                     (lambda (left _op right)
                       (declare (ignore _op))
                       (list :+ left right)))
         (expression |-| expression
                     (lambda (left _op right)
                       (declare (ignore _op))
                       (list :- left right)))
         (expression |*| expression
                     (lambda (left _op right)
                       (declare (ignore _op))
                       (list :× left right)))
         (expression |/| expression
                     (lambda (left _op right)
                       (declare (ignore _op))
                       (list :÷ left right)))
          (expression |==| expression
                      (lambda (left _op right)
                        (declare (ignore _op))
                        (list := left right)))
         (expression |~=| expression
                     (lambda (left _op right)
                       (declare (ignore _op))
                       (list :≠ left right)))
         (expression |<| expression
                     (lambda (left _op right)
                       (declare (ignore _op))
                       (list :< left right)))
         (expression |>| expression
                     (lambda (left _op right)
                       (declare (ignore _op))
                       (list :> left right)))
         (expression |<=| expression
                     (lambda (left _op right)
                       (declare (ignore _op))
                       (list :≤ left right)))
           (expression |>=| expression
                       (lambda (left _op right)
                         (declare (ignore _op))
                         (list :≥ left right)))
         (expression |and| expression
                     (lambda (left _and right)
                       (declare (ignore _and))
                       (list :and left right)))
          (expression |or| expression
                      (lambda (left _or right)
                        (declare (ignore _or))
                        (list :or left right)))
          (expression |&| expression
                      (lambda (left _op right)
                        (declare (ignore _op))
                        (list :∧ left right)))
          (expression |\|| expression
                      (lambda (left _op right)
                        (declare (ignore _op))
                        (list :∨ left right)))
          (expression |\^| expression
                      (lambda (left _op right)
                        (declare (ignore _op))
                        (list :⊻ left right)))
          (expression |<<| expression
                      (lambda (left _op right)
                        (declare (ignore _op))
                        (list :ash left right)))
          (expression |>>| expression
                      (lambda (left _op right)
                        (declare (ignore _op))
                        (list :ash left (list :- 0 right))))
          (|~| expression
               (lambda (_op expr)
                 (declare (ignore _op))
                 (list :¬ expr)))
          (|not| expression
                 (lambda (_not expr)
                   (declare (ignore _not))
                   (list :not expr)))
         (ident |=| expression
               (lambda (var _eq expr)
                 (declare (ignore _eq))
                 (list :move :from expr :to var)))
       (ident |:| ident function-args
              (lambda (obj _colon method args)
                (declare (ignore _colon args))
                (list :invoke obj method)))
       (ident function-args
              (lambda (fn args)
                (list :call :target fn :args args))))

      ;; Function arguments
      
      (function-args
       (|(| (lambda (_lp)
              (declare (ignore _lp))
              nil))
       (|(| |)|
           (lambda (_lp _rp)
             (declare (ignore _lp _rp))
             nil))
       (|(| arg-list |)|
            (lambda (_lp args _rp)
              (declare (ignore _lp _rp))
              args)))

      ;; Argument list
      
      (arg-list
       (expression
        (lambda (expr) (list expr)))
       (arg-list :comma expression
                 (lambda (prev _comma expr)
                   (declare (ignore _comma))
                   (append prev (list expr))))))

;;; Helper functions to handle statement conversion

(defun parse/lua-if (cond then &optional else-part)
  "Convert Lua if to EightBol :if node."
  (let ((node (list :if :condition cond :then then)))
    (when else-part
      (setf node (append node (list :else else-part))))
    node))

(defun parse/lua-while (cond body)
  "Convert Lua while to EightBol :perform node."
  (list :perform :body body :until (list :not cond)))

(defun parse/lua-for (var from to &optional by body)
  "Convert Lua for to EightBol :perform node."
  (let ((stmt (list :perform :body body :varying var :from from :to to)))
    (when by (setf stmt (append stmt (list :by by))))
    stmt))

(defun parse/lua-call (fn &optional args)
  "Convert Lua function call to EightBol :call node."
  (list :call :target fn :args (or args nil)))

(defun parse/lua-method (obj method &optional args)
  "Convert Lua method call to EightBol :invoke node."
  (declare (ignore args))
  (list :invoke obj method))

(defun parse/lua-set (target value)
  "Convert Lua assignment to EightBol :move node."
  (list :move :from value :to target))

(defun parse/lua-return (&optional value)
  "Convert Lua return to EightBol :goback node."
  (list :goback :value value))

(defun parse/lua-dialogue (text)
  "Create a dialogue AST node."
  (list :dialogue :text text))

(defun parse/lua-print (&rest args)
  "Create a print call AST node."
  (make-print-node args))

(defun parse/lua-input (&rest vars)
  "Create an input AST node with address-of wrapped vars."
  (make-input-node (mapcar (lambda (v) (list :address-of v)) vars)))

(defun parse/lua-copy (filename)
  "Create a copy AST node."
  (list :copy :name filename))

(defun parse/lua-break ()
  "Create a break AST node."
  (list :break))

(defun parse/lua-continue ()
  "Create a continue AST node."
  (list :continue))

(defun parse/lua-move (from to)
  "Create a move AST node (copy value from one place to another)."
  (list :move :from from :to to))

(defun parse/lua-procedure (name &optional body)
  "Create a procedure (function) AST node."
  (list :procedure name :body (or body '())))

(defun lua-transform-special-calls (ast)
  "Transform special library function calls to canonical AST nodes.
   - string_sub(s, start, end) -> :refmod
   - blt(src, dest, len) -> :string-blt
   - log_fault(code) -> :log-fault
   - debug_break(code) -> :debug-break
   - nil?(var) -> :nil?
   - bit_and(a,b), bit_or(a,b), bit_xor(a,b), bit_not(v) -> bitwise ops
   - lshift(v,n), rshift(v,n) -> arithmetic shift"
  (cond
    ((null ast) nil)
    ((not (listp ast)) ast)
    ((eq (car ast) :program)
     (list* :program (rest (mapcar #'lua-transform-special-calls ast))))
    ((eq (car ast) :call)
     ;; Check if this is a special function call
     (let ((target (getf ast :target)))
       (cond
         ;; string_sub(s, start, end) -> (:refmod :base s :start start :length end)
         ((string-equal target "string_sub")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 3)
              (list :refmod :base (first args) :start (second args) :length (third args)))))
         ;; blt(src, dest, len) -> (:string-blt :source src :dest dest :length len)
         ((string-equal target "blt")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 3)
              (list :string-blt :source (first args) :dest (second args) :length (third args)))))
         ;; log_fault(code) -> (:log-fault :code code)
         ((string-equal target "log_fault")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 1)
              (list :log-fault :code (first args)))))
         ;; debug_break(code) -> (:debug-break :code code)
         ((string-equal target "debug_break")
          (destructuring-bind (&key args) (cdr ast)
            (let ((code (if args (first args) 0)))
              (list :debug-break :code code))))
         ;; nil?(var) -> test for null
         ((string-equal target "nil?")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 1)
              (list :nil? (first args)))))
         ;; Bitwise operators
         ((string-equal target "bit_and")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 2)
              (list :∧ (first args) (second args)))))
         ((string-equal target "bit_or")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 2)
              (list :∨ (first args) (second args)))))
         ((string-equal target "bit_xor")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 2)
              (list :⊻ (first args) (second args)))))
         ((string-equal target "bit_not")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 1)
              (list :¬ (first args)))))
         ;; Shift operators
         ((string-equal target "lshift")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 2)
              (list :ash (first args) (second args)))))
         ((string-equal target "rshift")
          (destructuring-bind (&key args) (cdr ast)
            (when (= (length args) 2)
              ;; Right shift is negative left shift
              (list :ash (first args) (list :- 0 (second args))))))
         ;; Otherwise, return transformed call
         (t (list* :call (rest (mapcar #'lua-transform-special-calls ast)))))))
    ;; Recursively transform all nested structures
    (t (mapcar #'lua-transform-special-calls ast))))

;;; Main parser entry points

(defun lua-lex (source)
  "Tokenize Lua SOURCE string using tokenize-lua.
Return a list of token plists."
  (tokenize-lua source))

(defun parse/lua-program (source)
  "Parse Lua SOURCE to EightBol AST.
Returns a :program node with :statements containing all parsed statements."
  (let ((tokens (lua-lex source)))
    (when tokens
      (let ((parsed (yacc:parse-with-lexer
                      (lambda ()
                        (if tokens
                            (let ((tok (pop tokens)))
                              (let ((tok-type (getf tok :type))
                                    (tok-value (getf tok :value)))
                                ;; Convert token type to symbol for YACC
                                (cond
                                  ((eq tok-type :keyword)
                                   ;; For keywords, use the keyword name as terminal
                                   (list (intern tok-value :eightbol) tok-value))
                                  ((eq tok-type :symbol)
                                   ;; For symbols, keep the original behavior
                                   (list (intern (string-upcase tok-value) :eightbol) tok-value))
                                  ((eq tok-type :number)
                                   (list 'number tok-value))
                                  ((eq tok-type :string)
                                   (list 'string tok-value))
                                  (t
                                   (list tok-type tok-value)))))
                            (list nil nil)))
                      *lua-parser*)))
        (lua-transform-special-calls parsed)))))

;;; Export for use by other modules
(export '(parse/lua-program
          lua-lex
          parse/lua-if
          parse/lua-while
          parse/lua-for
          parse/lua-call
          parse/lua-method
          parse/lua-set
          parse/lua-return
          parse/lua-dialogue
          parse/lua-print
          parse/lua-input
          parse/lua-copy
          parse/lua-break
          parse/lua-continue
          parse/lua-move
          parse/lua-procedure))
