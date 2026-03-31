;; src/conditions.lisp — EIGHTBOL condition hierarchy
;; Unique, informational condition classes for different error types.
;; Use eightbol: package prefix (e.g. eightbol:copybook-not-found).
;; Callers can HANDLER-CASE on specific conditions for tailored handling.
(in-package :eightbol)

;;; Base condition

(define-condition compiler-error (error)
  ((message :initarg :message :reader eightbol-error-message))
  (:report (lambda (c s) (format s "~a" (eightbol-error-message c)))))

;;; Source-level errors (parser, unsupported statements)

(define-condition source-error (compiler-error)
  ((source-file     :initarg :source-file     :reader eightbol-error-file
    :initform nil)
   (source-line     :initarg :source-line     :reader eightbol-error-line
    :initform nil)
   (source-sequence :initarg :source-sequence :reader eightbol-error-sequence
    :initform nil)
   (terminal        :initarg :terminal        :reader eightbol-error-terminal
    :initform nil)
   (token-value     :initarg :token-value     :reader eightbol-error-value
    :initform nil)
   (expected        :initarg :expected        :reader eightbol-error-expected
    :initform nil))
  (:report
   (lambda (c s)
     (let ((file (eightbol-error-file c))
           (line (eightbol-error-line c))
           (seq  (eightbol-error-sequence c)))
       (format s "~@[~a:~]~@[line ~a~@[ (seq ~a)~]: ~]~a"
               file line seq (eightbol-error-message c))))))

;;; Lexer errors

(define-condition lexer-error (compiler-error)
  ((source-file :initarg :source-file :reader eightbol-error-file :initform nil)
   (source-line :initarg :source-line :reader eightbol-error-line :initform nil)
   (form        :initarg :form        :reader eightbol-error-form  :initform nil))
  (:report
   (lambda (c s)
     (format s "~@[~a:~]~@[line ~a: ~]~a~@[ (form: ~s)~]"
             (eightbol-error-file c) (eightbol-error-line c)
             (eightbol-error-message c) (eightbol-error-form c)))))

;;; Copybook errors

(define-condition copybook-error (compiler-error)
  ((copybook-name :initarg :copybook-name :reader eightbol-copybook-name
    :initform nil)
   (library       :initarg :library       :reader eightbol-copybook-library
    :initform nil))
  (:report
   (lambda (c s)
     (format s "~a~@[ (copybook: ~s)~]~@[ (library: ~s)~]"
             (eightbol-error-message c)
             (eightbol-copybook-name c) (eightbol-copybook-library c)))))

(define-condition copybook-not-found (copybook-error)
  ()
  (:documentation "Signalled when a COPY references a copybook that cannot be found."))

(define-condition copybook-invalid-name (copybook-error)
  ((kind :initarg :kind :reader eightbol-copybook-error-kind
    :initform :copybook
    :type (member :copybook :library)))
  (:report
   (lambda (c s)
     (format s "Invalid COPY ~a ~s: must not contain /, \\, or start with ."
             (ecase (eightbol-copybook-error-kind c)
               (:copybook "name")
               (:library "library"))
             (or (eightbol-copybook-name c) (eightbol-copybook-library c))))))

(define-condition copybook-read-error (copybook-error)
  ((path             :initarg :path             :reader eightbol-copybook-path
    :initform nil)
   (underlying-error :initarg :underlying-error :reader eightbol-copybook-underlying
    :initform nil))
  (:report
   (lambda (c s)
     (format s "Could not read copybook ~a: ~a"
             (eightbol-copybook-path c)
             (or (eightbol-copybook-underlying c) (eightbol-error-message c))))))

;;; Backend / code generation errors

(define-condition backend-error (compiler-error)
  ((cpu    :initarg :cpu    :reader eightbol-backend-cpu    :initform nil)
   (detail :initarg :detail :reader eightbol-backend-detail :initform nil))
  (:report
   (lambda (c s)
     (format s "~@[~a: ~]~a~@[ (~a)~]"
             (eightbol-backend-cpu c) (eightbol-error-message c)
             (eightbol-backend-detail c)))))

(define-condition backend-ast-error (backend-error)
  ((expected :initarg :expected :reader eightbol-backend-expected :initform nil)
   (actual   :initarg :actual   :reader eightbol-backend-actual   :initform nil))
  (:report
   (lambda (c s)
     (format s "~@[~a: ~]expected ~s, got ~s"
             (eightbol-backend-cpu c)
             (eightbol-backend-expected c) (eightbol-backend-actual c)))))

(define-condition backend-condition-not-implemented (backend-error)
  ((condition :initarg :condition :reader eightbol-backend-condition
    :initform nil))
  (:report
   (lambda (c s)
     (format s "~@[~a: ~]condition ~s not implemented"
             (eightbol-backend-cpu c) (eightbol-backend-condition c)))))

(define-condition backend-string-blt-error (backend-error)
  ((reason :initarg :reason :reader eightbol-backend-string-blt-reason
    :initform :length-required
    :type (member :length-required :invalid-length)))
  (:report
   (lambda (c s)
     (format s "~@[~a: ~]STRING DELIMITED BY SIZE ~a"
             (eightbol-backend-cpu c)
             (ecase (eightbol-backend-string-blt-reason c)
               (:length-required
                "requires LENGTH clause when source/dest have no reference modification")
               (:invalid-length
                "length must be a positive integer constant"))))))

(define-condition backend-call-service-error (backend-error)
  ((service :initarg :service :reader eightbol-backend-call-service
    :initform nil))
  (:report
   (lambda (c s)
     (format s "~@[~a: ~]CALL ~a IN SERVICE requires bank (not in service-bank table)"
             (eightbol-backend-cpu c) (eightbol-backend-call-service c)))))

(define-condition backend-copy-not-expanded (backend-error)
  ((copy-name :initarg :copy-name :reader eightbol-backend-copy-name
    :initform nil))
  (:report
   (lambda (c s)
     (format s "~@[~a: ~]COPY ~s should have been expanded at lex time"
             (eightbol-backend-cpu c) (eightbol-backend-copy-name c)))))

;;; Compilation pipeline errors

(define-condition compile-error (compiler-error)
  ((stage   :initarg :stage   :reader eightbol-compile-stage   :initform nil)
   (input   :initarg :input   :reader eightbol-compile-input   :initform nil))
  (:report
   (lambda (c s)
     (format s "~@[~a: ~]~a~@[ (input: ~a)~]"
             (eightbol-compile-stage c) (eightbol-error-message c)
             (eightbol-compile-input c)))))

;;; CLI / usage errors

(define-condition usage-error (compiler-error)
  ((option    :initarg :option    :reader eightbol-usage-option    :initform nil)
   (argument  :initarg :argument  :reader eightbol-usage-argument  :initform nil))
  (:report
   (lambda (c s)
     (format s "~@[~a: ~]~a"
             (eightbol-usage-option c) (eightbol-error-message c)))))

(define-condition unknown-option-error (usage-error)
  ()
  (:report
   (lambda (c s)
     (format s "Unknown option: ~a" (eightbol-usage-option c)))))

(define-condition dangling-option-error (usage-error)
  ()
  (:report
   (lambda (c s)
     (format s "Looks like a dangling option ends arguments: missing ~a"
             (eightbol-usage-option c)))))

(define-condition unknown-cpu-error (usage-error)
  ((cpu       :initarg :cpu       :reader eightbol-unknown-cpu       :initform nil)
   (known-cpus :initarg :known-cpus :reader eightbol-known-cpus :initform nil))
  (:report
   (lambda (c s)
     (format s "Unknown CPU: ~a. Known: ~a"
             (eightbol-unknown-cpu c) (eightbol-known-cpus c)))))

;;; Input / file errors

(define-condition input-file-not-found (compile-error)
  ((path :initarg :path :reader eightbol-input-file-path :initform nil))
  (:report
   (lambda (c s)
     (format s "Can't find input file ~a" (eightbol-input-file-path c)))))

;;; Parse / AST errors

(define-condition parse-failed-error (compile-error)
  ((input-file :initarg :input-file :reader eightbol-parse-input-file
    :initform nil)
   (actual     :initarg :actual     :reader eightbol-parse-actual
    :initform nil))
  (:report
   (lambda (c s)
     (format s "Parse of ~a did not yield a :program node~@[ (got ~s)~]"
             (eightbol-parse-input-file c) (eightbol-parse-actual c)))))

(define-condition invalid-ast-error (compile-error)
  ((expected :initarg :expected :reader eightbol-invalid-ast-expected
    :initform :program)
   (actual   :initarg :actual   :reader eightbol-invalid-ast-actual
    :initform nil))
  (:report
   (lambda (c s)
     (format s "Not a :program AST node (expected ~s~@[, got ~s~])"
             (eightbol-invalid-ast-expected c) (eightbol-invalid-ast-actual c)))))

;;; AST validation (class references, procedure termination)

(define-condition undefined-class-reference (compile-error)
  ((class-name :initarg :class-name :reader eightbol-undefined-class-name
    :initform nil)
   (defined-set :initarg :defined-set :reader eightbol-undefined-class-defined-set
    :initform nil))
  (:report
   (lambda (c s)
     (format s "OBJECT REFERENCE class ~s is not in the defined-class set ~s"
             (eightbol-undefined-class-name c)
             (eightbol-undefined-class-defined-set c)))))

(define-condition routine-not-terminated (compile-error)
  ((method-id :initarg :method-id :reader eightbol-routine-not-terminated-method-id
    :initform nil))
  (:report
   (lambda (c s)
     (format s "Method ~s is not terminated by GOBACK, EXIT, STOP RUN, unconditional ~
GO TO, or tail INVOKE/CALL (or IF with ELSE whose branches all terminate)"
             (eightbol-routine-not-terminated-method-id c)))))
