;; src/frontend-fountain/fountain-transpile.lisp — Fountain screenplay compilation
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defun compile-fountain-from-path (input-file &key cpus output-file)
  "Compile Fountain screenplay file to assembly for specified CPUs.
  
Fountain screenplay format (.fountain files) is used for game dialogue and cutscenes.
This function:
1. Lexes the Fountain source file
2. Parses to intermediate AST (screenplay-specific nodes)
3. Converts to canonical EIGHTBOL AST nodes
4. Compiles to target CPU assembly

INPUT-FILE: Path to .fountain screenplay file
CPUS: List of target CPU keywords (e.g., :6502, :z80, :arm7)
OUTPUT-FILE: Optional output assembly file path"
  (declare (ignore output-file))
  
  ;; Load and parse the Fountain file
  (let* ((source (uiop:read-file-string input-file))
         (tokens (fountain-frontend:lex-fountain-source source))
         (ast (fountain-frontend:parse-fountain-tokens tokens)))
    
    ;; Convert Fountain AST to canonical EIGHTBOL AST
    (let ((eightbol-ast (fountain-ast-to-eightbol ast)))
      ;; Compile using standard EIGHTBOL pipeline
      (compile-ast-program eightbol-ast cpus))))

(defun fountain-ast-to-eightbol (fountain-ast &optional script-name)
   "Convert Fountain-specific AST nodes to canonical EIGHTBOL nodes.
    
Fountain uses: :scene, :dialogue, :action, :transition, :character-entry, :variable-assignment, :conditional
Tier 1 additions: :character-action, :camera, :timing, :branch, :branch-choice
EIGHTBOL uses: :move, :set, :call, :invoke, :if, :perform, :print, :dialogue, :comment, etc.

For Forth output, Tier 1 constructs are converted to actual Forth word definitions
via :forth-word-def nodes instead of :comment nodes.

SCRIPT-NAME is used to generate the Forth word name (defaults to 'Script' if nil)."
   (unless fountain-ast
     (return-from fountain-ast-to-eightbol nil))
   
   (cond
     ;; :program node - top-level wrapper
     ;; For Forth, convert entire program to a :forth-word-def node
     ((and (listp fountain-ast) (eq (first fountain-ast) :program))
      (let* ((scenes (getf (rest fountain-ast) :scenes))
             (statements (getf (rest fountain-ast) :statements))
             (all-stmts (append scenes statements))
             ;; Use provided script-name or generate default
             (word-name (or script-name "Script")))
        ;; Emit actual Forth word definition instead of intermediate EIGHTBOL
        (fountain-ast-statements-to-forth all-stmts word-name)))
     
     ;; :scene node - handled in Forth emission, not as separate statement
     ((and (listp fountain-ast) (eq (first fountain-ast) :scene))
      ;; Scenes are processed in the :program handler; skip here
      nil)
    
    ;; :dialogue node - convert to :print or :dialogue
    ((and (listp fountain-ast) (eq (first fountain-ast) :dialogue))
     (let* ((speaker (getf (rest fountain-ast) :speaker))
            (text (getf (rest fountain-ast) :text)))
       (list :dialogue :speaker speaker :text text)))
     
     ;; :action node - handled in Forth emission as comment
     ((and (listp fountain-ast) (eq (first fountain-ast) :action))
      ;; Actions are processed by fountain-forth-emit
      nil)
     
     ;; :transition node - handled in Forth emission
     ((and (listp fountain-ast) (eq (first fountain-ast) :transition))
      nil)
     
     ;; :character-entry node - handled in Forth emission
     ((and (listp fountain-ast) (eq (first fountain-ast) :character-entry))
      nil)
    
    ;; :variable-assignment node - convert to :set
    ((and (listp fountain-ast) (eq (first fountain-ast) :variable-assignment))
     (let ((var-name (getf (rest fountain-ast) :var))
           (value (getf (rest fountain-ast) :value)))
       (list :set var-name value)))
    
    ;; :conditional node - convert to :if
    ((and (listp fountain-ast) (eq (first fountain-ast) :conditional))
     (let ((condition (getf (rest fountain-ast) :condition))
           (then-block (getf (rest fountain-ast) :then))
           (else-block (getf (rest fountain-ast) :else)))
       (list :if
             :condition condition
             :then (mapcar #'fountain-ast-to-eightbol then-block)
             :else (when else-block (mapcar #'fountain-ast-to-eightbol else-block)))))
    
    ;; :print node - convert to :print
    ((and (listp fountain-ast) (eq (first fountain-ast) :print))
     (let ((expressions (getf (rest fountain-ast) :expressions)))
       (list :print :expressions expressions)))
    
    ;; :input node - convert to :input
    ((and (listp fountain-ast) (eq (first fountain-ast) :input))
     (let ((prompt (getf (rest fountain-ast) :prompt))
           (variables (getf (rest fountain-ast) :variables)))
       (list :input :prompt prompt :variables variables)))
     
     ;; Tier 1: :character-action node - converted to Forth animation calls
     ((and (listp fountain-ast) (eq (first fountain-ast) :character-action))
      ;; Processed by fountain-forth-emit; return as passthrough
      fountain-ast)
     
     ;; Tier 1: :camera node - converted to Forth camera words
     ((and (listp fountain-ast) (eq (first fountain-ast) :camera))
      ;; Processed by fountain-forth-emit; return as passthrough
      fountain-ast)
     
     ;; Tier 1: :timing node - converted to Forth PAUSE words
     ((and (listp fountain-ast) (eq (first fountain-ast) :timing))
      ;; Processed by fountain-forth-emit; return as passthrough
      fountain-ast)
     
     ;; Tier 1: :branch node - converted to Forth IF/THEN branching
     ((and (listp fountain-ast) (eq (first fountain-ast) :branch))
      ;; Processed by fountain-forth-emit; return as passthrough
      fountain-ast)
     
     ;; Tier 1: :branch-choice node - handled as part of :branch
     ((and (listp fountain-ast) (eq (first fountain-ast) :branch-choice))
      ;; Processed by fountain-forth-emit; return as passthrough
      fountain-ast)
     
     ;; Default: return as-is (already an EIGHTBOL node or passthrough)
     (t fountain-ast)))
