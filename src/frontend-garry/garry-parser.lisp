;; src/frontend-garry/garry-parser.lisp — YACC grammar for Garry GameMaker language to EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;; Token list for Garry YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun garry-token-list ()
    (mapcar (compose #'intern #'string)
            '(|EVENT| |IF| |THEN| |ELSE| |ENDIF| |FOR| |TO| |STEP| |NEXT| |WHILE| |WEND| |DO| |LOOP| |GOTO| |GOSUB| |RETURN| |END| |REM| |PRINT| |INPUT| |LET| |SET| |MOVE| |DRAW| |SPRITE| |BACKGROUND| |SOUND| |WAIT| |CLS| |COLOR| |PLOT| |LINE| |CIRCLE| |TEXT| |VAR| |DIM| |AS| |INT| |STRING| |FLOAT| |AND| |OR| |NOT|
              + - * / = <> < > <= >=
              : lparen rparen comma colon semicolon
              number string atom))))

;; Parser action functions for Garry

(defun garry-parse-program (forms)
  "Top-level program node."
  (make-program-node "Garry" :data (when forms (list (cons 'main-block forms)))))

(defun garry-parse-if (condition then-form &optional else-form)
  "(IF condition then-form [else-form]) – conditional statement"
  (make-if-node condition (list then-form) (if else-form (list else-form) '())))

(defun garry-parse-for (var start to step body)
  "(FOR var start TO end [STEP step] body) – for loop"
  (make-perform-node (list body) :varying var :from start :by (or step 1)
                         :until (if (eql step 1) (make-conditional-gt (make-identifier var) to)
                                       (make-conditional-lt (make-identifier var) to))))

(defun garry-parse-while (condition body)
  "(WHILE condition body) – while loop"
  (make-perform-node (list body) :until (make-conditional-not condition)))

(defun garry-parse-repeat (count body)
  "(REPEAT count TIMES body) – repeat loop"
  (make-perform-node (list body) :times count))

(defun garry-parse-goto (label)
  "(GOTO label) – jump to label"
  (make-goto-node label))

(defun garry-parse-gosub (proc)
  "(GOSUB proc) – subroutine call"
  (make-call-node proc))

(defun garry-parse-return ()
  "(RETURN) – return from subroutine"
  (make-goback-node))

(defun garry-parse-print (&rest args)
  "(PRINT arg1 arg2 ...) – output to console"
  (make-print-node args))

(defun garry-parse-input (var)
  "(INPUT var) – read input into variable"
  (make-input-node var))

(defun garry-parse-let (var value)
  "(LET var = value) – variable assignment"
  (make-move-node value (make-identifier var)))

(defun garry-parse-set (var value)
  "(SET var = value) – alternative assignment"
  (make-set-node var value))

(defun garry-parse-move (from to)
  "(MOVE from TO to) – direct move"
  (make-move-node from to))

(defun garry-parse-draw (x y)
  "(DRAW x, y) – graphics draw operation"
  (make-draw-node x y))

(defun garry-parse-plot (x y color)
  "(PLOT x, y, color) – graphics plot operation"
  (make-plot-node x y color))

(defun garry-parse-color (c)
  "(COLOR c) – set drawing color"
  (make-color-node c))

(defun garry-parse-sprite (name filename)
  "(SPRITE name, filename) – create sprite"
  (make-sprite-node name filename))

(defun garry-parse-background (name filename)
  "(BACKGROUND name, filename) – set background"
  (make-background-node name filename))

(defun garry-parse-sound (name filename)
  "(SOUND name, filename) – load sound"
  (make-sound-node name filename))

(defun garry-parse-wait (frames)
  "(WAIT frames) – wait operation"
  (make-wait-node frames))

(defun garry-parse-cls ()
  "(CLS) – clear screen"
  (make-clear-node))