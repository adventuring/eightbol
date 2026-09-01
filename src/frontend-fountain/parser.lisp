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

;;;; Tier 1 AST Node Constructors

(defun make-character-action-node (character action &key emotion gesture animation)
   "Build a :character-action AST node for character emotional/physical actions.
CHARACTER: character name
ACTION: action verb (e.g., 'looks', 'gestures', 'animates')
EMOTION: optional emotion type (:angry, :sad, :happy, :surprised, :confused, :neutral)
GESTURE: optional gesture direction (:north, :south, :east, :west, :left, :right, :up, :down)
ANIMATION: optional animation name or identifier."
   (list :character-action
         :character character
         :action action
         :emotion emotion
         :gesture gesture
         :animation animation))

(defun make-camera-node (direction &key target location speed parameters)
   "Build a :camera AST node for camera directions.
DIRECTION: :cut, :frame, :truck, :dolly, :close
TARGET: character or location name being focused
LOCATION: specific location coordinates (x, y) or name
SPEED: optional speed in tiles per frame
PARAMETERS: additional keyword arguments (fade-color, fade-duration, etc.)."
   (list :camera
         :direction direction
         :target target
         :location location
         :speed speed
         :parameters parameters))

(defun make-timing-node (timing-type &key duration beats value)
   "Build a :timing AST node for scene pacing and delays.
TIMING-TYPE: :beat, :wait, :pause
DURATION: optional duration in seconds
BEATS: number of beats (½ second each)
VALUE: numeric expression for duration."
   (list :timing
         :type timing-type
         :duration duration
         :beats beats
         :value value))

(defun make-branch-node (dialogue-speaker choices)
   "Build a :branch AST node for dialogue branching/player choices.
DIALOGUE-SPEAKER: character speaking the dialogue
CHOICES: list of branch options, each with (:label target-label :text display-text)."
   (list :branch
         :speaker dialogue-speaker
         :choices choices))

(defun make-branch-choice-node (target-label text)
   "Build a branch choice option.
TARGET-LABEL: label to jump to when this choice is selected
TEXT: display text for this choice option."
   (list :branch-choice
         :label target-label
         :text text))

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

;;;; Input Statement

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

;;;; Tier 1: Character Action Parsing (EMOTION, GESTURE, ANIMATION)

(defun parse-character-action (state)
   "Parse character action like 'PLAYER looks sad' or 'PLAYER gestures north'.
Returns character-action node or nil."
   (when (and (current-token state) (eq (token-type (current-token state)) :identifier))
     (let ((character (token-value (consume-token state))))
       (when (and (current-token state)
                  (member (token-type (current-token state)) '(:looks :faces :gestures :animates)))
         (let ((action-type (token-type (consume-token state)))
                (action-name (case action-type
                               (:looks "looks")
                               (:faces "faces")
                               (:gestures "gestures")
                               (:animates "animates")))
                emotion gesture animation)
           ;; Parse emotion modifier (e.g., 'sad', 'angry', 'happy')
           (when (and (current-token state)
                      (member (token-type (current-token state)) 
                              '(:angry :sad :happy :surprised :confused :neutral)))
             (setf emotion (token-type (consume-token state))))
           ;; Parse gesture direction (e.g., 'north', 'left', 'right')
           (when (and (current-token state)
                      (member (token-type (current-token state))
                              '(:north :south :east :west :left :right :up :down)))
             (setf gesture (token-type (consume-token state))))
           ;; Parse animation identifier (e.g., animation name)
           (when (and (current-token state) (eq (token-type (current-token state)) :identifier))
             (setf animation (token-value (consume-token state))))
           
           (skip-newlines state)
           (make-character-action-node character action-name 
                                        :emotion emotion 
                                        :gesture gesture 
                                        :animation animation))))))

;;;; Tier 1: Camera Direction Parsing

(defun parse-camera-direction (state)
   "Parse camera direction like 'Cut to include ACTOR' or 'Truck left to center on ACTOR'.
Returns camera node or nil."
   (when (and (current-token state)
              (member (token-type (current-token state)) '(:cut :frame :truck :dolly :close)))
     (let ((direction (token-type (consume-token state)))
           target location speed parameters)
       
       ;; For TRUCK and DOLLY, optionally parse direction and speed
       (when (member direction '(:truck :dolly))
         ;; Check for speed prefix (e.g., "Truck 4 left")
         (when (and (current-token state) (eq (token-type (current-token state)) :decimal-number))
           (setf speed (token-value (consume-token state)))
           ;; Skip direction token if present
           (when (and (current-token state)
                      (member (token-type (current-token state)) '(:left :right :up :down)))
             (consume-token state)))
         ;; Skip direction-related keywords (left, right, up, down)
         (when (and (current-token state)
                    (member (token-type (current-token state)) '(:left :right :up :down)))
           (consume-token state)))
       
       ;; Parse target/location specification
       (cond
         ;; "to include ACTOR" or "to include Location"
         ((and (current-token state) (eq (token-type (current-token state)) :to))
          (consume-token state) ; TO
          (when (and (current-token state) (eq (token-type (current-token state)) :include))
            (consume-token state) ; INCLUDE
            ;; Next token is target
            (cond
              ((and (current-token state) (eq (token-type (current-token state)) :identifier))
               (setf target (token-value (consume-token state))))
              ((and (current-token state) (eq (token-type (current-token state)) :string-literal))
               (setf location (token-value (consume-token state)))))))
         
         ;; "to center on ACTOR" or "to center on Location"
         ((and (current-token state) (eq (token-type (current-token state)) :center))
          (consume-token state) ; CENTER
          (when (and (current-token state) (eq (token-type (current-token state)) :on))
            (consume-token state) ; ON
            (cond
              ((and (current-token state) (eq (token-type (current-token state)) :identifier))
               (setf target (token-value (consume-token state))))
              ((and (current-token state) (eq (token-type (current-token state)) :string-literal))
               (setf location (token-value (consume-token state)))))))
         
         ;; Single argument (CLOSE on ACTOR, FRAME ACTOR)
         (t
          (cond
            ((and (current-token state) (eq (token-type (current-token state)) :identifier))
             (setf target (token-value (consume-token state))))
            ((and (current-token state) (eq (token-type (current-token state)) :string-literal))
             (setf location (token-value (consume-token state)))))))
       
       (skip-newlines state)
       (make-camera-node direction :target target :location location :speed speed :parameters parameters))))

;;;; Tier 1: Timing Parsing (BEAT, WAIT, PAUSE)

(defun parse-timing (state)
   "Parse timing directive like 'Beat.' or 'Wait for 2 seconds.' or '3 Beats.'.
Returns timing node or nil."
   (cond
     ;; BEAT or BEATS keyword (may be preceded by a number)
     ((and (current-token state) (member (token-type (current-token state)) '(:beat :beats)))
      (let ((timing-type :beat)
            beats-count)
        ;; Try to get count from previous token if available
        ;; Check one token back for a number (for "3 Beats" pattern)
        (let ((backup-pos (parser-state-position state)))
          (when (> backup-pos 0)
            (let ((prev-token (aref (parser-state-tokens state) (- backup-pos 1))))
              (when (and prev-token (eq (token-type prev-token) :decimal-number))
                (setf beats-count (token-value prev-token))))))
        ;; If no previous number, check after keyword
        (consume-token state) ; consume BEAT/BEATS
        (when (and (not beats-count)
                   (current-token state)
                   (eq (token-type (current-token state)) :decimal-number))
          (setf beats-count (token-value (consume-token state))))
        (skip-newlines state)
        (make-timing-node timing-type :beats (or beats-count 1))))
     
     ;; WAIT FOR seconds
     ((and (current-token state) (eq (token-type (current-token state)) :wait))
      (consume-token state) ; WAIT
      (let ((duration 0))
        (when (and (current-token state) (eq (token-type (current-token state)) :for))
          (consume-token state) ; FOR
          ;; Parse duration (could be a number)
          (when (and (current-token state) (eq (token-type (current-token state)) :decimal-number))
            (setf duration (token-value (consume-token state))))
          ;; Check for SECONDS keyword
          (when (and (current-token state) (eq (token-type (current-token state)) :seconds))
            (consume-token state)))
        (skip-newlines state)
        (make-timing-node :wait :duration duration)))
     
     ;; PAUSE (same as BEAT)
     ((and (current-token state) (eq (token-type (current-token state)) :pause))
      (consume-token state)
      (skip-newlines state)
      (make-timing-node :pause :beats 1))))

;;;; Tier 1: Dialogue Branch Parsing

(defun parse-dialogue-branch (state speaker)
   "Parse dialogue with player choice branches.
Looks for parenthetical choices like (to \"label\") or (to continue).
Returns branch node or nil."
   (let ((choices '()))
     ;; Parse dialogue lines with embedded choices
     (loop while (and (current-token state)
                      (not (member (token-type (current-token state)) '(:int :ext :newline))))
           do (cond
                ;; Parenthetical with branch target
                ((and (current-token state) (eq (token-type (current-token state)) :lparen))
                 (consume-token state) ; (
                 ;; Check for "to label" or "to continue"
                 (when (and (current-token state) (eq (token-type (current-token state)) :to))
                   (consume-token state) ; TO
                   (let ((target nil))
                     (cond
                       ;; String label (e.g., "Ask about cake")
                       ((and (current-token state) (eq (token-type (current-token state)) :string-literal))
                        (setf target (token-value (consume-token state))))
                       ;; CONTINUE keyword for non-branching dialogue
                       ((and (current-token state) (eq (token-type (current-token state)) :continue))
                        (setf target :continue)
                        (consume-token state)))
                     ;; Collect remaining text until )
                     (let ((text-parts '()))
                       (loop until (or (at-end-p state)
                                       (eq (token-type (current-token state)) :rparen))
                             do (push (token-value (consume-token state)) text-parts))
                       (when (and (current-token state) (eq (token-type (current-token state)) :rparen))
                         (consume-token state))
                       (when target
                         (push (make-branch-choice-node target
                                                        (format nil "~{~A~^ ~}" (nreverse text-parts)))
                               choices))))))
                (t
                 (consume-token state))))
     (skip-newlines state)
     (if choices
         (make-branch-node speaker (nreverse choices))
         nil)))

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
     ;; Tier 1: Timing (BEAT, WAIT, PAUSE)
     ;; Handle both "Beat." and "3 Beats." and "Wait for..." patterns
     ((member (token-type (current-token state)) '(:beat :beats :wait :pause))
      (parse-timing state))
     ;; Handle "N Beats." pattern where N is a decimal number followed by BEATS
     ((and (current-token state)
           (eq (token-type (current-token state)) :decimal-number)
           (peek-token state)
           (member (token-type (peek-token state)) '(:beat :beats)))
      (consume-token state) ; consume the number
      (parse-timing state)) ; parse-timing will look back for the count

    ;; Tier 1: Camera directions
    ((member (token-type (current-token state)) '(:cut :frame :truck :dolly :close-on))
     (parse-camera-direction state))
    ;; Dialogue (check for character action modifiers or dialogue)
    ((eq (token-type (current-token state)) :identifier)
     (let ((backup-pos (parser-state-position state)))
       ;; Try character action first
       (let ((action (parse-character-action state)))
         (if action
             action
             ;; Reset and try dialogue
             (progn
               (setf (parser-state-position state) backup-pos)
               (parse-dialogue state))))))
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
          make-program-node
          ;; Tier 1 AST node constructors
          make-character-action-node
          make-camera-node
          make-timing-node
          make-branch-node
          make-branch-choice-node))
