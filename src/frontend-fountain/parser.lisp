;; src/frontend-fountain/parser.lisp — Parser for Fountain screenplay format
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :fountain-frontend)

;;;; AST Node Constructors

(defun make-scene-node (location-name map-name &key parent-context scene-type)
  "Build a :scene AST node.
LOCATION-NAME: narrative location string
MAP-NAME: game map identifier (PascalCase)
SCENE-TYPE: :normal, :blob, or other variant."
  (list :scene
        :location location-name
        :map map-name
        :scene-type (or scene-type :normal)
        :parent-context parent-context))

(defun make-dialogue-node (speaker text &key parenthetical character-modifiers)
  "Build a :dialogue AST node.
SPEAKER: character name
TEXT: spoken dialogue string
PARENTHETICAL: optional action/emotion in parentheses
CHARACTER-MODIFIERS: list of state changes (looks, faces, equips, etc)."
  (list :dialogue
        :speaker speaker
        :text text
        :parenthetical parenthetical
        :modifiers character-modifiers))

(defun make-action-node (description &key target actors)
  "Build an :action AST node.
DESCRIPTION: stage direction text
TARGET: object/location being acted upon
ACTORS: list of characters performing action."
  (list :action
        :description description
        :target target
        :actors actors))

(defun make-transition-node (transition-type &key parameters)
  "Build a :transition AST node.
TRANSITION-TYPE: :fade, :cut, :dissolve, :wipe, etc.
PARAMETERS: keyword args for fade direction, duration, etc."
  (list :transition
        :type transition-type
        :parameters parameters))

(defun make-character-entry-node (character-name location &key modifiers)
  "Build a :character-entry AST node.
CHARACTER-NAME: name of character entering
LOCATION: named location in scene
MODIFIERS: initial state (looks, faces, equips)."
  (list :character-entry
        :character character-name
        :location location
        :modifiers modifiers))

(defun make-variable-assignment-node (var-name value)
  "Build a :variable-assignment AST node.
VAR-NAME: PascalCase variable identifier
VALUE: expression (number, string, or complex expression)."
  (list :variable-assignment
        :variable var-name
        :value value))

(defun make-conditional-node (condition then-block &key else-block)
  "Build a :conditional AST node.
CONDITION: expression that evaluates to boolean
THEN-BLOCK: list of statements to execute when true
ELSE-BLOCK: optional list of statements when false."
  (list :conditional
        :condition condition
        :then then-block
        :else else-block))

(defun make-print-node (expressions)
  "Build a :print AST node for PRINT statement.
EXPRESSIONS: list of items to output (strings, variables, expressions)."
  (list :print
        :expressions expressions))

(defun make-input-node (prompt variable-names)
  "Build an :input AST node for INPUT statement.
PROMPT: optional string to display
VARIABLE-NAMES: list of variable identifiers to read."
  (list :input
        :prompt prompt
        :variables variable-names))

(defun make-expression-node (op left &optional right)
  "Build an :expression AST node for binary/unary operations.
OP: operator keyword (:plus, :minus, :greater-than, etc.)
LEFT: left operand
RIGHT: right operand (for binary ops)."
  (list :expression
        :operator op
        :left left
        :right right))

(defun make-comparison-node (operator left right)
  "Build a :comparison AST node.
OPERATOR: :equal, :not-equal, :greater-than, :less-than, :greater-equal, :less-equal
LEFT, RIGHT: expressions being compared."
  (list :comparison
        :operator operator
        :left left
        :right right))

(defun make-program-node (scenes statements)
  "Build a top-level :program AST node.
SCENES: list of scene nodes
STATEMENTS: list of top-level statements."
  (list :program
        :scenes scenes
        :statements statements))

;;;; Parser State and Utilities

(defstruct parser-state
  "Parser state for Fountain screenplay parsing."
  (tokens '())                  ; Token stream (reversed for pop efficiency)
  (position 0)                  ; Current position
  (current-scene nil)           ; Current scene context
  (ast '())                     ; Accumulated AST nodes
  (errors '()))                 ; Parse errors

(defun current-token (state)
  "Return current token without consuming it."
  (when (< (parser-state-position state) (length (parser-state-tokens state)))
    (aref (parser-state-tokens state) (parser-state-position state))))

(defun peek-token (state &optional (offset 1))
  "Peek at token OFFSET ahead (default 1)."
  (let ((pos (+ (parser-state-position state) offset)))
    (when (< pos (length (parser-state-tokens state)))
      (aref (parser-state-tokens state) pos))))

(defun consume-token (state &optional expected-type)
  "Consume and return current token, optionally checking type."
  (let ((token (current-token state)))
    (when (and expected-type token (not (eq (token-type token) expected-type)))
      (push (format nil "Expected ~A but got ~A at line ~A"
                    expected-type (token-type token) (token-line token))
            (parser-state-errors state)))
    (incf (parser-state-position state))
    token))

(defun skip-newlines (state)
  "Skip all newline tokens."
  (loop while (and (current-token state) (eq (token-type (current-token state)) :newline))
        do (consume-token state)))

(defun at-end-p (state)
  "Check if at end of token stream."
  (>= (parser-state-position state) (length (parser-state-tokens state))))

;;;; Scene Header Parsing

(defun parse-scene-header (state)
  "Parse scene header (slugline) like 'INT LOCATION - MAP NAME' or 'INT TITLE CARD (BLOB)'.
Returns scene node or nil on parse error."
  (let* ((token (current-token state))
         (line (token-line token)))
    (when (member (token-type token) '(:int :ext))
      (let ((prefix (consume-token state)) ; INT or EXT
            (location-parts '())
            (map-name nil)
            (scene-type :normal))
        ;; Collect location parts until - or (
        (loop while (and (current-token state)
                         (not (member (token-type (current-token state)) '(:minus :lparen :newline)))
                         (not (at-end-p state)))
              do (push (token-value (consume-token state)) location-parts))
        
        ;; Check for (BLOB)
        (when (and (current-token state) (eq (token-type (current-token state)) :lparen))
          (consume-token state) ; (
          (when (and (current-token state) (eq (token-type (current-token state)) :identifier))
            (if (string-equal (token-value (current-token state)) "BLOB")
                (setf scene-type :blob)
                nil))
          (consume-token state) ; identifier
          (consume-token state)) ; )
        
        ;; Check for map name after -
        (when (and (current-token state) (eq (token-type (current-token state)) :minus))
          (consume-token state) ; -
          (let ((map-parts '()))
            (loop while (and (current-token state)
                             (not (eq (token-type (current-token state)) :newline))
                             (not (at-end-p state)))
                  do (push (token-value (consume-token state)) map-parts))
            (setf map-name (format nil "~{~A~^ ~}" (nreverse map-parts)))))
        
        (skip-newlines state)
        
        (make-scene-node
         (format nil "~{~A~^ ~}" (nreverse location-parts))
         (or map-name (format nil "~{~A~^ ~}" (nreverse location-parts)))
         :scene-type scene-type)))))

;;;; Character Entry Parsing

(defun parse-character-entry (state)
  "Parse character entry like 'Enter PLAYER at \"Tavern Door\"'.
Returns character-entry node or nil."
  (when (and (current-token state) (eq (token-type (current-token state)) :enter))
    (consume-token state) ; ENTER
    (let ((character (when (current-token state)
                       (token-value (consume-token state)))))
      (when (and (current-token state) (eq (token-type (current-token state)) :at))
        (consume-token state) ; AT
        (let ((location (when (and (current-token state)
                                    (eq (token-type (current-token state)) :string-literal))
                          (token-value (consume-token state)))))
          (let ((modifiers '()))
            ;; Parse optional modifiers (looks, faces, equips)
            (loop while (and (current-token state)
                             (member (token-type (current-token state)) '(:looks :facing :equips))
                             (not (eq (token-type (current-token state)) :newline)))
                  do (let ((modifier-type (token-type (consume-token state)))
                           (modifier-value (when (current-token state)
                                             (token-value (consume-token state)))))
                       (push (list modifier-type modifier-value) modifiers)))
            (skip-newlines state)
            (make-character-entry-node character location :modifiers (nreverse modifiers))))))))

;;;; Dialogue Parsing

(defun parse-dialogue (state)
  "Parse dialogue block: speaker and text.
Returns dialogue node or nil."
  (when (and (current-token state) (eq (token-type (current-token state)) :identifier))
    (let* ((speaker (token-value (consume-token state)))
           (text-parts '()))
      (skip-newlines state)
      ;; Collect dialogue text until newline or speaker action
      (loop while (and (current-token state)
                       (not (eq (token-type (current-token state)) :newline))
                       (not (member (token-type (current-token state)) '(:identifier :int :ext :transition)))
                       (not (at-end-p state)))
            do (push (token-value (consume-token state)) text-parts))
      (skip-newlines state)
      (make-dialogue-node speaker (format nil "~{~A~^ ~}" (nreverse text-parts))))))

;;;; Conditional Parsing (WHEN/UNLESS)

(defun parse-conditional (state)
  "Parse conditional statement: 'WHEN condition THEN' or 'UNLESS condition'.
Returns conditional node or nil."
  (let ((is-when (and (current-token state) (eq (token-type (current-token state)) :when))))
    (when (or is-when (and (current-token state) (eq (token-type (current-token state)) :unless)))
      (consume-token state) ; WHEN or UNLESS
      (let ((condition (parse-expression state))
            (then-block '()))
        (when (and (current-token state) (eq (token-type (current-token state)) :then))
          (consume-token state)) ; THEN
        ;; Parse statements until next conditional or transition
        (loop while (and (current-token state)
                         (not (member (token-type (current-token state)) '(:when :unless :transition :int :ext)))
                         (not (at-end-p state)))
              do (let ((stmt (parse-statement state)))
                   (when stmt (push stmt then-block))))
        (skip-newlines state)
        (make-conditional-node
         (if is-when condition (list :not condition))
         (nreverse then-block))))))

;;;; Expression Parsing

(defun parse-expression (state)
  "Parse an expression (variable, literal, or compound expression).
Returns expression AST node or value."
  (let ((left (parse-primary state)))
    ;; Handle binary operators
    (loop while (and (current-token state)
                     (member (token-type (current-token state)) '(:equal :gt :lt :greater :less :and :or)))
          do (let ((op-token (consume-token state))
                   (right (parse-primary state)))
               (setf left (make-expression-node
                           (operator-type (token-type op-token))
                           left
                           right))))
    left))

(defun parse-primary (state)
  "Parse primary expression (literal, variable, or parenthesized expression)."
  (when (current-token state)
    (cond
      ;; Variable reference
      ((eq (token-type (current-token state)) :variable)
       (token-value (consume-token state)))
      ;; String literal
      ((eq (token-type (current-token state)) :string-literal)
       (token-value (consume-token state)))
      ;; Number literal
      ((member (token-type (current-token state)) '(:decimal-number :hex-number :octal-number :binary-number :dword-number))
       (token-value (consume-token state)))
      ;; Parenthesized expression
      ((eq (token-type (current-token state)) :lparen)
       (consume-token state)
       (let ((expr (parse-expression state)))
         (consume-token state) ; )
         expr))
      ;; Identifier or keyword (e.g. DIFFERENCE, SUM, etc.)
      ((eq (token-type (current-token state)) :identifier)
       (token-value (consume-token state)))
      (t nil))))

(defun operator-type (token-type)
  "Map token type to operator keyword."
  (case token-type
    (:equal :equal)
    (:gt :greater-than)
    (:lt :less-than)
    (:greater :greater-than)
    (:less :less-than)
    (:plus :plus)
    (:minus :minus)
    (:star :times)
    (:slash :divide)
    (:and :and)
    (:or :or)
    (t token-type)))

;;;; Variable Assignment and SET Statement

(defun parse-variable-assignment (state)
  "Parse 'Set $variable to expression'.
Returns variable-assignment node or nil."
  (when (and (current-token state) (eq (token-type (current-token state)) :set))
    (consume-token state) ; SET
    (let ((var-name (when (and (current-token state)
                               (eq (token-type (current-token state)) :variable))
                      (token-value (consume-token state)))))
      (when (and (current-token state) (eq (token-type (current-token state)) :to))
        (consume-token state) ; TO
        (let ((value (parse-expression state)))
          (skip-newlines state)
          (make-variable-assignment-node var-name value))))))

;;;; Print and Input Statements

(defun parse-print (state)
  "Parse PRINT statement.
Returns print node or nil."
  (when (and (current-token state) (eq (token-type (current-token state)) :print))
    (consume-token state) ; PRINT
    (let ((expressions '()))
      (loop until (or (at-end-p state)
                      (eq (token-type (current-token state)) :newline))
            do (let ((expr (parse-expression state)))
                 (when expr (push expr expressions))
                 (when (and (current-token state) (eq (token-type (current-token state)) :comma))
                   (consume-token state))))
      (skip-newlines state)
      (make-print-node (nreverse expressions)))))

(defun parse-input (state)
  "Parse INPUT statement.
Returns input node or nil."
  (when (and (current-token state) (eq (token-type (current-token state)) :input))
    (consume-token state) ; INPUT
    (let ((prompt (when (and (current-token state)
                             (eq (token-type (current-token state)) :string-literal))
                    (token-value (consume-token state))))
          (variables '()))
      (loop until (or (at-end-p state)
                      (eq (token-type (current-token state)) :newline))
            do (when (and (current-token state)
                          (eq (token-type (current-token state)) :variable))
                 (push (token-value (consume-token state)) variables))
               (when (and (current-token state) (eq (token-type (current-token state)) :comma))
                 (consume-token state)))
      (skip-newlines state)
      (make-input-node prompt (nreverse variables)))))

;;;; General Statement Parsing

(defun parse-statement (state)
  "Parse a top-level statement.
Returns AST node or nil."
  (skip-newlines state)
  (cond
    ((at-end-p state) nil)
    ;; Scene header
    ((member (token-type (current-token state)) '(:int :ext))
     (parse-scene-header state))
    ;; Character entry
    ((eq (token-type (current-token state)) :enter)
     (parse-character-entry state))
    ;; Conditionals
    ((member (token-type (current-token state)) '(:when :unless))
     (parse-conditional state))
    ;; Variable assignment
    ((eq (token-type (current-token state)) :set)
     (parse-variable-assignment state))
    ;; Print statement
    ((eq (token-type (current-token state)) :print)
     (parse-print state))
    ;; Input statement
    ((eq (token-type (current-token state)) :input)
     (parse-input state))
    ;; Transitions
    ((eq (token-type (current-token state)) :transition)
     (consume-token state)
     (make-transition-node :fade))
    ;; Dialogue
    ((eq (token-type (current-token state)) :identifier)
     (parse-dialogue state))
    ;; Action/stage direction
    (t
     (let ((description-parts '()))
       (loop until (or (at-end-p state)
                       (eq (token-type (current-token state)) :newline)
                       (member (token-type (current-token state)) '(:int :ext :enter :when :unless)))
             do (push (token-value (consume-token state)) description-parts))
       (skip-newlines state)
       (when description-parts
         (make-action-node (format nil "~{~A~^ ~}" (nreverse description-parts))))))))

;;;; Main Parser Entry Points

(defun parse-fountain-tokens (tokens)
  "Parse token stream into AST.
TOKENS: list of token structures from lexer
Returns (values program-ast error-list)."
  (let ((state (make-parser-state :tokens (coerce tokens 'vector))))
    (let ((statements '()))
      (loop until (at-end-p state)
            do (let ((stmt (parse-statement state)))
                 (when stmt (push stmt statements))))
      (values (make-program-node '() (nreverse statements))
              (parser-state-errors state)))))

(defun parse-fountain-source (source)
  "Lex and parse Fountain SOURCE code into AST.
Returns (values program-ast error-list)."
  (let ((tokens (lex-fountain-source source)))
    (parse-fountain-tokens tokens)))

(defun parse-fountain-file (filepath)
  "Lex and parse Fountain file from FILEPATH into AST.
Returns (values program-ast error-list)."
  (let ((tokens (lex-fountain-file filepath)))
    (parse-fountain-tokens tokens)))

(export '(parse-fountain-source
          parse-fountain-file
          parse-fountain-tokens
          make-scene-node
          make-dialogue-node
          make-action-node
          make-transition-node
          make-character-entry-node
          make-variable-assignment-node
          make-conditional-node
          make-print-node
          make-input-node
          make-program-node))
