;; src/frontend-fountain/fountain-transpile.lisp — Fountain screenplay compilation
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

(defun fountain-ast-to-eightbol (fountain-ast)
  "Convert Fountain-specific AST nodes to canonical EIGHTBOL nodes.
  
Fountain uses: :scene, :dialogue, :action, :transition, :character-entry, :variable-assignment, :conditional
EIGHTBOL uses: :move, :set, :call, :invoke, :if, :perform, :print, :dialogue, :comment, etc."
  (unless fountain-ast
    (return-from fountain-ast-to-eightbol nil))
  
  (cond
    ;; :program node - top-level wrapper
    ((and (listp fountain-ast) (eq (first fountain-ast) :program))
     (let* ((scenes (getf (rest fountain-ast) :scenes))
            (statements (getf (rest fountain-ast) :statements))
            (converted-scenes (mapcar #'fountain-ast-to-eightbol scenes))
            (converted-stmts (mapcar #'fountain-ast-to-eightbol statements)))
       (make-method-node "Main"
                        :statements (append converted-scenes converted-stmts))))
    
    ;; :scene node - becomes a comment or label
    ((and (listp fountain-ast) (eq (first fountain-ast) :scene))
     (let* ((location (getf (rest fountain-ast) :location))
            (map-name (getf (rest fountain-ast) :map))
            (scene-desc (format nil "Scene: ~a - ~a" location map-name)))
       (list :comment :text scene-desc)))
    
    ;; :dialogue node - convert to :print or :dialogue
    ((and (listp fountain-ast) (eq (first fountain-ast) :dialogue))
     (let* ((speaker (getf (rest fountain-ast) :speaker))
            (text (getf (rest fountain-ast) :text)))
       (list :dialogue :speaker speaker :text text)))
    
    ;; :action node - convert to :comment
    ((and (listp fountain-ast) (eq (first fountain-ast) :action))
     (let ((description (getf (rest fountain-ast) :description)))
       (list :comment :text description)))
    
    ;; :transition node - convert to :comment
    ((and (listp fountain-ast) (eq (first fountain-ast) :transition))
     (let ((transition-type (getf (rest fountain-ast) :type)))
       (list :comment :text (format nil "Transition: ~a" transition-type))))
    
    ;; :character-entry node - convert to :comment
    ((and (listp fountain-ast) (eq (first fountain-ast) :character-entry))
     (let ((character (getf (rest fountain-ast) :character))
           (location (getf (rest fountain-ast) :location)))
       (list :comment :text (format nil "~a enters at ~a" character location))))
    
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
    
     ;; Default: return as-is (already an EIGHTBOL node or passthrough)
     (t fountain-ast)))
