;; src/ast.lisp — Abstract Syntax Tree definitions for EIGHTBOL
;;
;; All AST nodes are standard CL plists so they round-trip through
;; print/read with no special serialiser needed.
;;
;; Node shapes:
;;   Program:   (:program  :class-id "Character" :data (...) :methods (...))
;;   Method:    (:method   :method-id "Think" :statements (...))
;;
;; Statement nodes:
;;   (:move       :from expr :to identifier)
;;   (:invoke     :object expr :method "Kill" [:returning identifier])
;;   (:call       :target name :bank bank-or-nil)
;;   (:if         :condition cond :then stmts :else stmts)
;;   (:goback)
;;   (:exit-method)
;;   (:exit-program)
;;   (:exit)
;;   (:stop-run)
;;   (:add        :from expr :to identifier)
;;   (:add        :from expr :to expr :giving identifier)
;;   (:subtract   :from expr :from-target identifier)
;;   (:subtract   :from expr :from-target expr :giving identifier)
;;   (:compute    :target identifier :expression expr)
;;   (:perform    :procedure name [:times expr] [:until cond] [:varying ...])
;;   (:set        :target identifier :value expr)
;;   (:log-fault  :code dword-expr)
;;   (:debug-break :code expr)
;;   (:copy       :name "CopybookName")   ; residual after failed expansion
;;   (:string-blt :source operand :dest operand [:length expr])  ; STRING DELIMITED BY SIZE
;;
;; Expression/operand values:
;;   literal number or string
;;   symbol (identifier reference)
;;   (:of slot obj)       — qualified identifier (slot OF obj)
;;   (:address-of id)     — ADDRESS OF id
;;   (:refmod :base name :start expr :length expr)  — reference modification name(start:length)
;;   (:subscript name index)  — subscripted identifier name(index)
;;   :self / :null        — SELF / NULL

(in-package :eightbol)

;;; Constructors

(defun make-program-node (class-id &key data methods identification environment)
  "Build a :program AST node."
  (list :program
        :class-id      class-id
        :identification identification
        :environment   environment
        :data          (or data '())
        :methods       (or methods '())))

(defun make-method-node (method-id &key statements)
  "Build a :method AST node."
  (list :method
        :method-id  method-id
        :statements (or statements '())))

;;; Accessors

(defun ast-node-type (node)
  "Return the node type keyword (:program, :method, :move, …)."
  (and (listp node) (first node)))

(defun ast-class-id (program-node)
  (getf (rest program-node) :class-id))

(defun ast-methods (program-node)
  (getf (rest program-node) :methods))

(defun ast-data (program-node)
  (getf (rest program-node) :data))

(defun ast-method-name (method-node)
  (getf (rest method-node) :method-id))

(defun ast-method-statements (method-node)
  (getf (rest method-node) :statements))

;;; S-expression I/O

(defun write-ast (ast output-stream)
  "Write AST to OUTPUT-STREAM as a readable S-expression."
  (let ((*print-pretty*   t)
        (*print-readably* t)
        (*print-length*   nil)
        (*print-level*    nil))
    (write ast :stream output-stream)
    (terpri output-stream)))

(defun read-ast (input-stream)
  "Read and return an AST S-expression from INPUT-STREAM."
  (read input-stream))

