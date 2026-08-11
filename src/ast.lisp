;; src/ast.lisp — Abstract Syntax Tree definitions for EIGHTBOL
;;
;; All AST nodes are standard CL plists so they round-trip through
;; print/read with no special serialiser needed.
;;
;; Node shapes:
;;   Program:   (:program  :class-id "Character" :data (...) :methods (...))
;;   Method:    (:method   :method-id "Think" :statements (...))
;;              Optional first statement:
;;   (:assembly-entry :label "MyRoutine") — names the assembly entry symbol
;;              for this method body (see Programmers Reference).
;;
;; Statement nodes:
;;   (:move       :from expr :to identifier)
;;   (:invoke     :object expr :method "Kill" [:returning identifier] [:using expr])
;;   (:call-acc :target name :bank bank-or-nil)
;;   (:if         :condition cond :then stmts :else stmts)
;;   (:goto       :target identifier)          ; GOTO/GO TO; GOBACK equivalent
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
;;   (:perform    :procedure name [:times expr] [:until cond] [:varying ...] [:body stmts])
;;   (:set        :target identifier :value expr)
;;   (:log-fault  :code dword-expr)
;;   (:debug-break :code expr)
;;   (:copy       :name "CopybookName")   ; residual after failed expansion
;;   (:string-blt :source operand :dest operand [:length expr])  ; STRING DELIMITED BY SIZE
;;   (:assembly-entry :label "Symbol")  ; must be first executable-area statement
;;
;; Expression/operand values:
;;   literal number or string
;;   symbol (identifier reference)
;;   (:of slot obj)       — qualified identifier (slot OF obj)
;;   (:address-of id)     — ADDRESS OF id
;;   (:refmod :base name :start expr :length expr)  — reference modification name(start:length)
;;   (:subscript name index)  — subscripted identifier name(index)
;;   :self / :null        — SELF / NULL
;; ;;
;; FORTRAN-inspired node shapes (explicit typing, no implicit numeric):
;;   (:fortran-move       :from expr :to identifier :target-type type-keyword)
;;   (:fortran-compute    :target identifier :expression expr :result-type type-keyword)
;;   (:fortran-do         :var identifier :from expr :to expr [:by expr] :stmts stmts)
;;   (:fortran-if         :condition expr :then stmts :else stmts)
;;   (:fortran-logical-op :op keyword :left expr :right expr)
;;   (:fortran-arith-op   :op keyword :left expr :right expr :result-type type-keyword)
;;   (:fortran-print      :arguments exprs :format format-spec :unit unit-expr)
;;   (:fortran-read       :variables vars :format format-spec :unit unit-expr)
;;   (:fortran-write      :arguments exprs :format format-spec :unit unit-expr)
;;   (:fortran-dialogue   :type keyword :arguments exprs :format format-spec)

(in-package :eightbol)

;;; Constructors

(defun make-goto-node (target)
  "Build a :goto AST node for GOTO/GO TO statements."
  (list :goto :target target))

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

;;; FORTRAN I/O and dialogue node constructors

(defun make-fortran-print (arguments &key format unit)
  "Build a :fortran-print AST node for PRINT statements.
   ARGUMENTS — list of expressions to output
   FORMAT — optional format specification
   UNIT — optional output unit (default: standard output)"
  (list :fortran-print :arguments arguments :format format :unit unit))

(defun make-fortran-read (variables &key format unit)
  "Build a :fortran-read AST node for READ statements.
   VARIABLES — list of variables to read into
   FORMAT — optional format specification
   UNIT — optional input unit (default: standard input)"
  (list :fortran-read :variables variables :format format :unit unit))

(defun make-fortran-write (arguments &key format unit)
  "Build a :fortran-write AST node for WRITE statements.
   ARGUMENTS — list of expressions to write
   FORMAT — optional format specification
   UNIT — output unit (required)"
  (list :fortran-write :arguments arguments :format format :unit unit))

(defun make-fortran-dialogue (dialogue-type arguments &key format)
  "Build a :fortran-dialogue AST node for dialogue/input/output operations.
   DIALOGUE-TYPE — keyword like :PRINT, :READ, :INPUT, :OUTPUT
   ARGUMENTS — list of expressions or variables
   FORMAT — optional format specification"
  (list :fortran-dialogue :type dialogue-type :arguments arguments :format format))

(defun make-fortran-move (from to &optional target-type)
  "Build a :fortran-move AST node with explicit typing."
  (list :fortran-move :from from :to to :target-type target-type))

(defun make-fortran-compute (target expression &optional result-type)
  "Build a :fortran-compute AST node with explicit result typing."
  (list :fortran-compute :target target :expression expression :result-type result-type))

(defun make-fortran-declare (name &key type init)
  "Build a :fortran-declare AST node for variable declarations."
  (list :fortran-declare :name name :type type :init init))

(defun make-fortran-do (var from to &key by stmts)
  "Build a :fortran-do AST node for DO loops."
  (list :fortran-do :var var :from from :to to :by by :stmts stmts))

(defun make-fortran-if (condition then else)
  "Build a :fortran-if AST node."
  (list :fortran-if :condition condition :then then :else else))

(defun make-fortran-arithmetic (op left right &optional result-type)
  "Build a :fortran-arithmetic AST node with type info."
  (list :fortran-arithmetic :op op :left left :right right :result-type result-type))

(defun make-fortran-type (name type &key fields)
  "Build a :fortran-type AST node for structured types."
  (list :fortran-type :name name :type type :fields fields))

(defun make-print-node (expressions)
  "Build a :print AST node for PRINT statements.
EXPRESSIONS is a list of values/strings to print."
  (list :print :expressions (or expressions '())))

(defun make-input-node (variables)
  "Build an :input AST node for INPUT statements.
VARIABLES is a list of identifiers to read into."
  (list :input :variables (or variables '())))

(defun make-dialogue-node (character text)
  "Build a :dialogue AST node for dialogue statements.
CHARACTER is the character name, TEXT is the dialogue string."
  (list :dialogue :character character :text text))

;;; Accessors

(defun ast-node-type (node)
  "Return the node type keyword (:program, :method, :move, …)."
  (and (listp node) (first node)))

(defun ast-class-id (program-node)
  (safe-getf (rest (lastcar (remove-if #'null program-node))) :class-id))

(defun ast-program-id (program-node)
  (safe-getf (rest (lastcar (remove-if #'null program-node))) :program-id))

(defun ast-methods (program-node)
  (if (and (listp program-node) (eq (first program-node) :program))
      (safe-getf (rest program-node) :methods)
      (loop for node in program-node
            append (safe-getf (rest node) :methods))))

(defun ast-data (program-node)
  (safe-getf (rest program-node) :data))

(defun ast-method-name (method-node)
  (safe-getf (rest method-node) :method-id))

(defun ast-method-statements (method-node)
  (safe-getf (rest method-node) :statements))

(defun split-method-leading-assembly-entry (statements)
  "Return (values entry-label-string-or-nil body-statements).

If the first non-NIL item in STATEMENTS is @code{(:assembly-entry :label …)},
return that label (as a string) and the remaining statements (NIL entries
removed from the head only). Otherwise return @code{(values nil cleaned)} where
@code{cleaned} is @code{(remove nil (ensure-list statements))}.

@table @asis
@item STATEMENTS
Raw @code{:statements} list from a @code{:method} node (may contain NIL).
@end table"
  (let ((lst (remove nil (ensure-list statements))))
    (if (and lst (listp (car lst)) (eq :assembly-entry (first (car lst))))
        (let ((lab (getf (rest (car lst)) :label)))
          (values (when lab (string lab)) (cdr lst)))
        (values nil lst))))

(defun ast-method-statements-for-validation (method-node)
  "Return METHOD's statements with an optional leading @code{:assembly-entry} removed.

Used by termination validation: @code{:assembly-entry} is not executable code."
  (multiple-value-bind (_ body) (split-method-leading-assembly-entry
                                  (ast-method-statements method-node))
    (declare (ignore _))
    body))

;;; S-expression I/O

(defun write-ast (ast output-stream)
  "Write AST to OUTPUT-STREAM as a readable S-expression."
  (let ((*print-pretty* t)
        (*print-readably* t)
        (*print-length* nil)
        (*print-level* nil))
    (write ast :stream output-stream)
    (terpri output-stream)))

(defun read-ast (input-stream)
  "Read and return an AST S-expression from INPUT-STREAM."
  (read input-stream))

;;; Case-preserving display labels for UI (help text, error messages).
;;; Input matching is case-insensitive (string-equal); display preserves
;;; intended case.
;;;
;;; After changing +cpu-display-names+ or +supported-cpus+, rebuild
;;; bin/skyline-tool (make bin/skyline-tool) so the buildapp image matches;
;;; otherwise alexandria:define-constant may signal when sources are
;;; recompiled against an older embedded value.
(define-constant +cpu-display-names+
    '((:6502 . "6502")
      (:65c02 . "65c02")
      (:65c816 . "65c816")
      (:cp1610 . "cp1610")
      (:huc6280 . "HuC6280")
      (:rp2a03 . "RP2A03")
      (:z80 . "Z80")
      (:sm83 . "SM83")
      (:m6800 . "m6800")
      (:m68k . "m68k")
      (:i286 . "i286")
      (:arm7 . "ARM7")
      (:f8 . "F8"))
  :test 'equalp
  :documentation
  "Alist of CPU keyword to case-preserving directory / UI label string.")

;;; All supported CPU keywords, in canonical order (see +cpu-display-names+).
(define-constant +supported-cpus+
    (mapcar #'first +cpu-display-names+)
  :test 'equal
  :documentation
  "List of supported CPU keywords in canonical order; derived from +cpu-display-names+.")

(defvar *cpu* nil
  "Current target CPU keyword; bound during compile-to-assembly.")

(defun cpu-display-name (&optional (cpu *cpu*))
  "Return the case-preserving UI label for CPU."
  (rest (assoc cpu +cpu-display-names+)))

(defun cpu-directory-name (&optional (cpu *cpu*))
  "Return the canonical output directory component for a CPU keyword.
Uses display names: cp1610, 65c02, RP2A03, m68k, i286, Z80, etc."
  (cpu-display-name cpu))

;;; Validation

(defun validate-ast-no-data-definitions (ast frontend-name)
  "Validate that AST does not contain data definition nodes for non-COBOL frontends.
Only COBOL frontend should emit :dd or :fortran-* data definition nodes.
Returns NIL if valid, or error message if invalid."
  (when (and frontend-name (not (string-equal frontend-name "COBOL")))
    (labels
        ((has-data-definitions (node)
           (when (listp node)
             (or (eq (first node) :dd)
                 (eq (first node) :fortran-declare)
                 (eq (first node) :fortran-class)
                 (eq (first node) :fortran-method)
                 (eq (first node) :fortran-new)
                 (some #'has-data-definitions (rest node))))))
      (when (has-data-definitions ast)
        (format nil "Frontend ~A must not emit data definition nodes (~A)" frontend-name
                (let ((node (find-if #'has-data-definitions ast)))
                  (when node (first node))))))))

;;; End of file