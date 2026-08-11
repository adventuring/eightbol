;; src/frontend-fortran/fortran-parser.lisp — YACC grammar for FORTRAN → EIGHTBOL AST
(in-package :eightbol)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fortran-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |:| |,| + - * / ** = < <= > >= <>
              program subroutine function end return stop call
              goto if then else do read write print open close
              real integer logical character complex type data
              parameter dimension common equivalence include copy
              while until and or not true false
              class method instance new delete invoke super self procedure
              number string ident))))

(defvar *fortran-token-location* nil
  "plist (:source-file :source-line :source-sequence) of current token.")

(defmacro fortran-unsupported (feature)
  `(error 'source-error
          :message (format nil "FORTRAN ~a is not supported." ,feature)
          :source-line (getf *fortran-token-location* :source-line)))

(defun fortran-parse-program (name statements)
  (make-program-node (string-upcase name)
                     :methods (list (make-method-node "MAIN" :statements statements))))

(defun fortran-parse-subroutine (name args statements)
  (declare (ignore args))
  (make-method-node (string-upcase name) :statements statements))

(defun fortran-parse-function (name args return-type statements)
  (declare (ignore args return-type))
  (make-method-node (string-upcase name) :statements statements))

(defun fortran-parse-assignment (target expr)
  (make-move-node expr target))

(defun fortran-parse-call (target args)
  (declare (ignore args))
  (make-call-node target))

(defun fortran-parse-instantiation (class-name init-args)
  (make-invoke :object class-name :method "New" :using init-args))

(defun fortran-parse-invoke (obj method args)
   (declare (ignore args))
   (list :invoke :object obj :method method))

(defun fortran-parse-create-method (name parameters body)
  (declare (ignore parameters body))
  (make-method-node name :statements '()))

(defun fortran-parse-super-call (method)
  (list :sup-call :method method))

(defun fortran-parse-goto (label)
  (list :goto :target label))

(defun fortran-parse-return ()
   (make-goback-node))

(defun fortran-parse-copy (filename)
   (list :copy :name filename))

(defun fortran-parse-stop ()
  (make-stop-run-node))

(defun fortran-parse-print (arguments &optional format)
   "Parse PRINT statement. Emit a CALL to PrintRoutine."
   (declare (ignore format))
   (make-call-node "PrintRoutine"))

(defun fortran-parse-read (variables &optional format unit)
   "Parse READ statement. Emit a CALL to ReadRoutine."
   (declare (ignore variables format unit))
   (make-call-node "ReadRoutine"))

(defun fortran-parse-write (unit arguments &optional format)
   "Parse WRITE statement. Emit a CALL to WriteRoutine."
   (declare (ignore arguments format unit))
   (make-call-node "WriteRoutine"))

(defun fortran-parse-input (arguments)
   "Parse INPUT statement (dialogue). Emit a CALL to InputRoutine."
   (declare (ignore arguments))
   (make-call-node "InputRoutine"))

(defun fortran-parse-if-then (condition then-stmts)
  (make-if-node condition then-stmts '()))

(defun fortran-parse-if-then-else (condition then-stmts else-stmts)
  (make-if-node condition then-stmts else-stmts))

(defun fortran-parse-do-loop (var from to by stmts)
   (make-perform-node (format nil "DO-~A" var)
                      :varying var
                      :from from
                      :by (or by 1)
                      :until (list '> var to)
                      :body stmts))

(defun fortran-build-literal (value) value)
(defun fortran-build-string (value) value)
(defun fortran-build-identifier (name)
  "Normalize identifier NAME to PascalCase."
  (fortran-normalize-identifier name))

(defun fortran-build-arithmetic (op left right)
   (ecase op
     (:plus (list :add :from left :to right :giving nil))
     (:minus (list :subtract :subtrahend right :from left :giving nil))
     (:times (list :multiply :by left :multiplier right :giving nil))
     (:divide (list :divide :numerator left :denominator right :giving nil))
     (:power (fortran-unsupported "power operator (**)"))))

(defun fortran-build-relational (op left right)
  (ecase op
    (:equal (list '= left right))
    (:not-equal (list '/= left right))
    (:less-than (list '< left right))
    (:greater-than (list '> left right))
    (:less-equal (list '<= left right))
    (:greater-equal (list '>= left right))))

(defun fortran-build-logical (op left &optional right)
  (ecase op
    (:and (list :and left right))
    (:or (list :or left right))
    (:not (list :not left))))

(defun fortran-build-parenthesized (expr) expr)

(eval
 `(yacc:define-parser *fortran-parser*
    (:start-symbol fortran-program)
    (:terminals `(,@(fortran-token-list) number string ident))
    (:precedence ((:left and :or) (:right not)
                  (:left equal not-equal lt le gt ge)
                  (:left plus minus)
                  (:left times divide)
                  (:right power)))
    (:muffle-conflicts :some)

    (fortran-program
      (program ident stmt-list end
        (lambda (_ name stmts __ end-name)
          (declare (ignore _ __ end-name))
          (fortran-parse-program name stmts))))

    (stmt-list
      ()
      (stmt-list stmt (lambda (s st) (append s (list st)))))

    (stmt
      (subroutine ident arg-list stmt-list end ident
        (lambda (_ name args stmts __ end-name)
          (declare (ignore _ __ end-name))
          (fortran-parse-subroutine name args stmts)))

      (function ident arg-list type-spec stmt-list end ident
        (lambda (_ name args rtype stmts __ end-name)
          (declare (ignore _ __ end-name))
          (fortran-parse-function name args rtype stmts)))

      (ident :EQUAL expression
        (lambda (target _ e)
          (declare (ignore _))
          (fortran-parse-assignment target e)))

      (call ident arg-list
        (lambda (_ target args)
          (declare (ignore _))
          (fortran-parse-call target args)))

      (goto ident
        (lambda (_ l)
          (declare (ignore _))
          (fortran-parse-goto l)))

      (return
        (lambda (_)
          (declare (ignore _))
          (fortran-parse-return)))

      (stop
        (lambda (_)
          (declare (ignore _))
          (fortran-parse-stop)))

      (if lpar condition rpar then stmt-list
        (lambda (_ open-paren cond close-paren then-keyword then-stmts)
          (declare (ignore _ open-paren close-paren then-keyword))
          (fortran-parse-if-then cond then-stmts)))

      (if lpar condition rpar then stmt-list else stmt-list
        (lambda (_ open-paren cond close-paren then-keyword then-stmts else-keyword else-stmts)
          (declare (ignore _ open-paren close-paren then-keyword else-keyword))
          (fortran-parse-if-then-else cond then-stmts else-stmts)))

      (do ident :EQUAL expression :COMMA expression step-opt stmt-list enddo
        (lambda (_ var eq-keyword from comma-keyword to opt-step stmts enddo-keyword)
          (declare (ignore _ eq-keyword comma-keyword enddo-keyword))
          (fortran-parse-do-loop var from to opt-step stmts)))

       (read ident arg-list
         (lambda (_ u args)
           (declare (ignore _))
           (fortran-parse-read args nil u)))

       (write ident format arg-list
         (lambda (_ u f args)
           (declare (ignore _))
           (fortran-parse-write u args f)))

       (print arg-list
         (lambda (_ args)
           (declare (ignore _))
           (fortran-parse-print args)))

      (data ident :EQUAL expression
        (lambda (_ n eq-keyword e)
          (declare (ignore _ eq-keyword))
          (fortran-unsupported "DATA")))

      (parameter ident :EQUAL expression
        (lambda (_ n eq-keyword e)
          (declare (ignore _ eq-keyword))
          (fortran-unsupported "PARAMETER")))

      (type-spec ident
        (lambda (_ type-name v)
          (declare (ignore _))
          (fortran-unsupported "type declaration")))

      (common ident variable-list
        (lambda (_ b v)
          (declare (ignore _))
          (fortran-unsupported "COMMON")))

      (dimension ident lpar variable-list rpar
        (lambda (_ n open-paren d close-paren)
          (declare (ignore _ open-paren close-paren))
          (fortran-unsupported "DIMENSION")))

       (include string
         (lambda (_ f)
           (declare (ignore _))
           (fortran-unsupported "INCLUDE")))

       (copy string
         (lambda (_ f)
           (declare (ignore _))
           (fortran-parse-copy f)))

       (class ident end ident
           (lambda (_ name _2 body _3 end-name)
             (declare (ignore _ _2 _3 end-name))
             (make-method-node name :statements body)))

      (class method ident colon ident parameter-list stmt-list endmethod
        (lambda (_1 _2 mname _3 mname2 params body _4)
          (declare (ignore _1 _2 _3 _4))
          (fortran-parse-create-method mname params body)))

      (instance ident :EQUAL new ident lpar arg-list rpar
        (lambda (_1 var _2 _3 _4 cname _5 args _6)
          (declare (ignore _1 _2 _3 _4 _5 _6))
          (fortran-parse-instantiation cname args)))

      (invoke ident dot ident lpar arg-list rpar
        (lambda (_1 obj _2 _3 method _4 args _5)
          (declare (ignore _1 _2 _3 _4 _5))
          (fortran-parse-invoke obj method args)))

      (invoke super dot ident lpar arg-list rpar
        (lambda (_1 _2 _3 method _4 args _5)
          (declare (ignore _1 _2 _3 _4 _5))
          (fortran-parse-super-call method))))

    (arg-list () (variable-list))

    (variable-list
      (ident (lambda (i) (list i)))
      (variable-list :COMMA ident
        (lambda (vars _ comma v)
          (declare (ignore _ comma))
          (append vars (list v)))))

    (step-opt
      (step ident expression
        (lambda (_ step-ident e)
          (declare (ignore _ step-ident))
          e))
      ())

    (expression
      (literal-expression)
      (string-expression)
      (identifier-expression)
      (parenthesized-expression)
      (arithmetic-expression)
      (relational-expression)
      (expression and expression
        (lambda (left _op right)
          (declare (ignore _op))
          (fortran-build-logical :and left right)))
      (expression or expression
        (lambda (left _op right)
          (declare (ignore _op))
          (fortran-build-logical :or left right)))
      (not expression
        (lambda (_op expr)
          (declare (ignore _op))
          (fortran-build-logical :not expr))))

    (literal-expression (number #'fortran-build-literal))
    (string-expression (string #'fortran-build-string))
    (identifier-expression (ident #'fortran-build-identifier))

    (parenthesized-expression
      (lpar expression rpar #'fortran-build-parenthesized))

    (arithmetic-expression
      (expression plus expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-arithmetic :plus l r)))
      (expression minus expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-arithmetic :minus l r)))
      (expression times expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-arithmetic :times l r)))
      (expression divide expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-arithmetic :divide l r)))
      (expression power expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-arithmetic :power l r))))

    (relational-expression
      (expression equal expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-relational :equal l r)))
      (expression not-equal expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-relational :not-equal l r)))
      (expression lt expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-relational :less-than l r)))
      (expression gt expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-relational :greater-than l r)))
      (expression le expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-relational :less-equal l r)))
      (expression ge expression
        (lambda (l _ r) (declare (ignore _)) (fortran-build-relational :greater-equal l r))))

     (condition (expression))

     (format
       (string (lambda (s) s))
       (number (lambda (n) n))
       ())

     (keyword
      (program (constantly :PROGRAM))
      (subroutine (constantly :SUBROUTINE))
      (function (constantly :FUNCTION))
      (end (constantly :END))
      (return (constantly :RETURN))
      (stop (constantly :STOP))
      (call (constantly :CALL))
      (goto (constantly :GOTO))
      (if (constantly :IF))
      (then (constantly :THEN))
      (else (constantly :ELSE))
      (do (constantly :DO))
      (enddo (constantly :ENDDO))
      (read (constantly :READ))
      (write (constantly :WRITE))
      (print (constantly :PRINT))
      (open (constantly :OPEN))
      (close (constantly :CLOSE))
      (data (constantly :DATA))
      (parameter (constantly :PARAMETER))
      (dimension (constantly :DIMENSION))
      (common (constantly :COMMON))
       (equivalence (constantly :EQUIVALENCE))
       (include (constantly :INCLUDE))
       (copy (constantly :COPY))
       (while (constantly :WHILE))
      (until (constantly :UNTIL))
      (and (constantly :AND))
      (or (constantly :OR))
      (not (constantly :NOT))
      (true (constantly :TRUE))
      (false (constantly :FALSE))
      (class (constantly :CLASS))
      (method (constantly :METHOD))
      (instance (constantly :INSTANCE))
      (new (constantly :NEW))
      (delete (constantly :DELETE))
      (invoke (constantly :INVOKE))
      (super (constantly :SUPER))
      (self (constantly :SELF))
      (procedure (constantly :PROCEDURE)))

    (lpar (|(| (constantly :LPAR)))
    (rpar (|)| (constantly :RPAR)))
    (comma (|,|))
    (colon (|:|))
    (equal (|=| (constantly :EQUAL)))
    (not-equal (|<>| (constantly :NOT-EQUAL)))
    (lt (|<| (constantly :LT)))
    (gt (|>| (constantly :GT)))
    (le (|<=| (constantly :LE)))
    (ge (|>=| (constantly :GE)))
    (plus (|+| (constantly :PLUS)))
    (minus (|-| (constantly :MINUS)))
    (times (|*| (constantly :TIMES)))
    (divide (|/| (constantly :DIVIDE)))
    (power (|**| (constantly :POWER)))
    (step (step (constantly :STEP)))

    (type-spec
      (real (constantly :REAL))
      (integer (constantly :INTEGER))
      (logical (constantly :LOGICAL))
      (character (constantly :CHARACTER))
      (complex (constantly :COMPLEX)))))

(defun fortran-stream-code (tokens)
  (let ((*fortran-token-location*
          (or *fortran-token-location*
              (list :source-file "<fortran>"))))
    (lambda ()
      (loop
        (unless tokens (return (values nil nil)))
        (let ((token (pop tokens)))
          (when token
            (return (values (first token) (second token)))))))))

(defun parse-fortran (source)
  (let ((*fortran-token-location* nil))
    (handler-case
        (yacc:parse-with-lexer
         (fortran-stream-code (fortran-lex source))
         *fortran-parser*)
      (yacc:yacc-parse-error (c)
        (error 'source-error
               :message (format nil "FORTRAN parse error: unexpected token ~s~@[ (~s)~]. Expected one of: ~{~s~^, ~}"
                                (yacc:yacc-parse-error-terminal c)
                                (yacc:yacc-parse-error-value c)
                                (yacc:yacc-parse-error-expected-terminals c))
               :source-line (getf *fortran-token-location* :source-line))))))

(defvar *fortran-type-table* (make-hash-table :test 'equalp)
  "FORTRAN variable type information from COPY books.")

(defun load-fortran-copybook (pathname)
  (declare (ignore pathname))
  nil)