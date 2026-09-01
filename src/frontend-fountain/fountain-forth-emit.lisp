;; src/frontend-fountain/fountain-forth-emit.lisp — Emit Forth source from Fountain AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;
;; Converts Fountain AST nodes to actual Forth word definitions (:forth-word-def).
;; Each Fountain construct maps to Phantasia Forth primitives:
;; - Dialogue      → SAY or NARRATE with character/text
;; - Camera action → CAMERA-CUT, CAMERA-TRUCK, CAMERA-DOLLY, etc.
;; - Timing        → PAUSE with duration
;; - Character act → Animation/gesture Forth calls
;; - Scene heading → Comment or label
;; - Branches      → IF/THEN branching in Forth
;;
;; Output: (:forth-word-def :name word-name :source forth-code)
(in-package :eightbol)

;;;; Helper functions for building Forth source strings

(defun forth-escape-string (text)
  "Escape TEXT for use in Forth strings.
Replaces quotes, escapes special characters."
  (if (null text)
      ""
      (progn
        ;; Replace double-quotes with escaped versions
        (setf text (cl-ppcre:regex-replace-all "\"" text "\\\""))
        ;; Replace newlines with spaces (Forth doesn't like newlines in strings)
        (setf text (cl-ppcre:regex-replace-all "\\n" text " "))
        text)))

(defun forth-make-comment (text)
  "Build a Forth comment line from TEXT.
Returns a string like '( This is a comment )'"
  (format nil "( ~a )" text))

(defun forth-indent (level text)
  "Indent TEXT by LEVEL (0=no indent, 1=2 spaces, etc.)."
  (let ((indent (make-string (* level 2) :initial-element #\Space)))
    (concatenate 'string indent text)))

(defun forth-emit-dialogue-as-forth (speaker text)
  "Convert dialogue to Forth SAY command.
SPEAKER: Character name or identifier
TEXT: Dialogue text

Returns a Forth code snippet like:
  CharacterID_Inari C\" text\" do-dialogue"
  (let* ((escaped-text (forth-escape-string text))
         ;; Convert speaker to Forth identifier (e.g., "Inari" → CharacterID_Inari)
         (speaker-id (when speaker
                       (format nil "CharacterID_~a" 
                              (string-capitalize speaker))))
         (newline (string #\Newline))
         ;; If narration (no speaker), use off-camera-narrator
         (setup (if speaker
                   (format nil "~a~a" speaker-id newline)
                   (format nil "off-camera-narrator~a" newline))))
    (concatenate 'string
                setup
                (format nil "C\" ~a\"~a" escaped-text newline)
                (format nil "do-dialogue~a" newline))))

(defun forth-emit-character-action (character action emotion gesture animation)
  "Convert character action to Forth animation call.
Returns a Forth code snippet for character animation."
  (let* ((char-id (format nil "CharacterID_~a" (string-capitalize character)))
         (action-name (or animation action ""))
         (newline (string #\Newline))
         ;; Build emotion/gesture modifiers
         (modifiers '())
         (action-call ""))
    
    (when emotion
      (push (string-downcase emotion) modifiers))
    
    (when gesture
      (push (string-downcase gesture) modifiers))
    
    ;; Generate Forth call like: CharacterID_Inari animate-happy-gesture
    (setf action-call 
          (concatenate 'string
                      char-id " "
                      "animate-" (string-downcase action-name)
                      (if modifiers
                          (concatenate 'string "-" (format nil "~{~a~^-~}" modifiers))
                          "")))
    
    (format nil "~a~a" action-call newline)))

(defun forth-emit-camera-direction (direction target location speed)
  "Convert camera direction to Forth camera word.
DIRECTION: :cut, :truck, :dolly, :frame, :close, etc.
Returns a Forth code snippet."
  (let* ((camera-word (case direction
                       (:cut "camera-cut")
                       (:frame "camera-frame")
                       (:truck "camera-truck")
                       (:dolly "camera-dolly")
                       (:close "camera-close")
                       (t "camera-default")))
         (newline (string #\Newline))
         (params (list)))
    
    ;; Build parameter list
    (when location
      (push (format nil "~a" location) params))
    
    (when target
      (push (format nil "~a" target) params))
    
    (when speed
      (push (format nil "speed-~a" speed) params))
    
    ;; Emit as: camera-cut <params>
    (format nil "~a~{ ~a~}~a" camera-word params newline)))

(defun forth-emit-timing (timing-type duration beats)
  "Convert timing construct to Forth PAUSE command.
TIMING-TYPE: :beat, :wait, :pause
DURATION: seconds or frames
BEATS: number of beats
Returns a Forth code snippet."
  (let ((newline (string #\Newline)))
    (case timing-type
      (:beat
       (if beats
           (format nil "pause-beats ~a~a" beats newline)
           (format nil "pause-beat~a" newline)))
      (:wait
       (if duration
           (format nil "pause ~a~a" duration newline)
           (format nil "pause~a" newline)))
      (:pause
       (format nil "pause~a" newline))
      (t (format nil "pause~a" newline)))))

(defun forth-emit-scene-setup (location map-name)
  "Convert scene heading to Forth setup code.
Returns Forth code for loading map and setting up scene."
  (let* ((map-id (format nil "Map_~a_ID" 
                        (cl-ppcre:regex-replace-all " " (or map-name "") "_")))
         (newline (string #\Newline))
         (setup (format nil "~a load-map~a prepare-scene~a~a" map-id newline newline newline)))
    (concatenate 'string
                (forth-make-comment (format nil "Scene: ~a" (or location "")))
                newline
                setup)))

;;;; Main Forth emission functions

(defun fountain-statement-to-forth (ast-node)
  "Convert a single Fountain AST node to Forth code.
Returns a string of Forth source code or NIL if no code needed."
  (unless ast-node
    (return-from fountain-statement-to-forth nil))
  
  (let ((node-type (first ast-node))
        (node-data (rest ast-node)))
    
    (case node-type
      (:scene
       (let ((location (getf node-data :location))
             (map-name (getf node-data :map)))
         (forth-emit-scene-setup location map-name)))
      
      (:dialogue
       (let ((speaker (getf node-data :speaker))
             (text (getf node-data :text)))
         (forth-emit-dialogue-as-forth speaker text)))
      
       (:action
         ;; Scene description actions become comments
         (let ((description (getf node-data :description)))
           (when description
             (let ((newline (string #\Newline)))
               (format nil "~a~a" (forth-make-comment description) newline)))))
      
       (:transition
        ;; Transitions become comments
        (let ((transition-type (getf node-data :type)))
          (when transition-type
            (let ((newline (string #\Newline)))
              (format nil "~a~a" (forth-make-comment (format nil "Transition: ~a" transition-type)) newline)))))
      
       (:character-entry
        ;; Character entry: CharacterID_Name X Y enter-character scene-ready
        (let ((character (getf node-data :character))
              (location (getf node-data :location))
              (x (getf node-data :x))
              (y (getf node-data :y))
              (newline (string #\Newline)))
          (format nil "CharacterID_~a ~a ~a enter-character scene-ready~a"
                 (string-capitalize character)
                 (or x "0")
                 (or y "0")
                 newline)))
      
      (:character-action
       ;; Character action: gesture, emotion, animation
       (let ((character (getf node-data :character))
             (action (getf node-data :action))
             (emotion (getf node-data :emotion))
             (gesture (getf node-data :gesture))
             (animation (getf node-data :animation)))
         (forth-emit-character-action character action emotion gesture animation)))
      
      (:camera
       ;; Camera direction: :cut, :truck, :dolly, etc.
       (let ((direction (getf node-data :direction))
             (target (getf node-data :target))
             (location (getf node-data :location))
             (speed (getf node-data :speed)))
         (forth-emit-camera-direction direction target location speed)))
      
      (:timing
       ;; Timing: beat, wait, pause
       (let ((timing-type (getf node-data :type))
             (duration (getf node-data :duration))
             (beats (getf node-data :beats)))
         (forth-emit-timing timing-type duration beats)))
      
      (:branch
       ;; Dialogue choices: emit branching logic
       (let ((speaker (getf node-data :speaker))
             (choices (getf node-data :choices)))
         (forth-emit-dialogue-branches speaker choices)))
      
      ;; Default: emit as comment if we don't know the type
      (t
       (let ((comment (format nil "Unknown node type: ~a" node-type)))
         (concatenate 'string
                     (forth-make-comment comment)
                     "~%"))))))

(defun forth-emit-dialogue-branches (speaker choices)
  "Emit Forth code for dialogue branch menu.
CHOICES: List of (:branch-choice :label label :text text) nodes
Returns Forth code snippet."
  (unless choices
    (return-from forth-emit-dialogue-branches ""))
  
  ;; Emit the speaker setup
  (let* ((setup (format nil "CharacterID_~a~%" (string-capitalize speaker)))
         ;; For now, emit choices as comments with Forth branching structure
         (branch-code ""))
    
    (dolist (choice choices)
      (let* ((label (getf (cdr choice) :label))
             (text (getf (cdr choice) :text)))
        (when text
          (setf branch-code
                (concatenate 'string
                            branch-code
                            (format nil "~%C\" ~a\"~%" (forth-escape-string text)))))))
    
    (concatenate 'string
                setup
                branch-code
                "( TODO: Implement dialogue branching #1202 )~%")))

;;;; Main entry point: Build :forth-word-def node

(defun create-forth-word-def (script-name fountain-statements)
  "Create a :forth-word-def AST node from Fountain AST statements.

SCRIPT-NAME: Name for the generated Forth word (e.g., \"AtsiravTownHall\")
FOUNTAIN-STATEMENTS: List of Fountain AST nodes

Returns: (:forth-word-def :name word-name :source forth-code)"
  
  (let* ((word-name (concatenate 'string "Script_" script-name))
         ;; Convert all statements to Forth
         (forth-lines (remove nil 
                            (mapcar #'fountain-statement-to-forth 
                                   fountain-statements)))
         ;; Join all lines into one source string
         (forth-source (format nil "~{~a~^~}" forth-lines))
         ;; Wrap in Forth word definition
         (word-def (format nil ": ~a~%~a;~%" word-name forth-source))
         ;; Add header comments
         (full-source (format nil 
                             "( -*- forth -*- )~%( Generated from Fountain screenplay )~%( Script: ~a )~%( Auto-generated; do not edit manually. )~%~%~a~%~%BYE ( End of script file. )~%"
                             script-name
                             word-def)))
    
    ;; Return as :forth-word-def node
    (list :forth-word-def 
          :name word-name
          :source full-source)))

;;;; Integration: Replace :comment emission with :forth-word-def

(defun fountain-ast-statements-to-forth (fountain-ast script-name)
  "Convert Fountain AST statements to a :forth-word-def node.
Called from fountain-ast-to-eightbol to replace :comment emission.

FOUNTAIN-AST: Raw Fountain AST from parser
SCRIPT-NAME: Name for generated Forth word

Returns: (:forth-word-def :name word-name :source forth-code)"
  
  (unless fountain-ast
    (return-from fountain-ast-statements-to-forth nil))
  
  ;; Extract all statements (skip :program wrapper if present)
  (let* ((statements (if (and (listp fountain-ast) 
                             (eq (first fountain-ast) :program))
                        (getf (rest fountain-ast) :statements)
                        fountain-ast))
         ;; Convert Fountain statements to canonical EIGHTBOL nodes first
         (eightbol-statements (mapcar #'fountain-ast-to-eightbol statements)))
    
    ;; Create Forth word definition from converted statements
    (create-forth-word-def script-name eightbol-statements)))
