;; src/basic-lexer.lisp — Tokenization for Dartmouth BASIC source
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token type definitions
(defparameter *basic-keyword-alist*
  '((("REM" "REM") . :REM)
    (("LET" "LET") . :LET)
    (("FOR" "FOR") . :FOR)
    (("TO" "TO") . :TO)
    (("STEP" "STEP") . :STEP)
    (("NEXT" "NEXT") . :NEXT)
    (("IF" "IF") . :IF)
    (("THEN" "THEN") . :THEN)
    (("ELSE" "ELSE") . :ELSE)
    (("GOTO" "GOTO") . :GOTO)
    (("GOSUB" "GOSUB") . :GOSUB)
    (("RETURN" "RETURN") . :RETURN)
    (("LOG" "LOG") . :LOG)
    (("FAULT" "FAULT") . :FAULT)
    (("STOP" "STOP") . :STOP)
    (("METHOD" "METHOD") . :METHOD)
    (("ON" "ON") . :ON)
    (("CLASS" "CLASS") . :CLASS)
    (("PROCEDURE" "PROCEDURE") . :PROCEDURE)
    (("END" "END") . :END)
    (("WHILE" "WHILE") . :WHILE)
    (("UNTIL" "UNTIL") . :UNTIL)
    (("DO" "DO") . :DO)
    (("LIBRARY" "LIBRARY") . :LIBRARY)
    (("AND" "AND") . :AND)
    (("OR" "OR") . :OR)
    (("NOT" "NOT") . :NOT))
  "Association list of BASIC keywords to token symbols.")

(defparameter *basic-operators-alist*
  '(("+" . :PLUS)
    ("-" . :MINUS)
    ("*" . :TIMES)
    ("/" . :DIVIDE)
    ("=" . :EQUAL)
    ("<" . :LT)
    (">" . :GT)
    ("<=" . :LE)
    (">=" . :GE)
    ("<>". :NE)
    ("." . :DOT))
  "Association list of BASIC operators to token symbols.")

(defun basic-token-type (lexeme)
  "Return token type for LEXEME, a string."
  (cond
    ((and (stringp lexeme) (digit-char-p (char lexeme 0)))
     :NUMBER)
    ((and (stringp lexeme) (char= (char lexeme 0) #\"))
     :STRING)
    ((assoc lexeme *basic-keyword-alist* :test #'string-equal)
     (cdr it))
    ((assoc lexeme *basic-operators-alist* :test #'string-equal)
     (cdr it))
    ((string= lexeme "(") :LPAREN)
    ((string= lexeme ")") :RPAREN)
    ((string= lexeme ",") :COMMA)
    ((string= lexeme ":") :COLON)
    ((basic-valid-identifier-p lexeme)
     :IDENT)
    (t :UNKNOWN)))

(defun basic-valid-identifier-p (s)
  "Check if S is a valid PascalCase alphanumeric BASIC identifier."
  (and (stringp s)
       (plusp (length s))
       (every (lambda (ch)
                (or (alphanumericp ch) (char= ch #\$) (char= ch #\_)))
              s)))

(defun basic-lex-line (line)
  "Split LINE into token list of (type value) pairs.
Handles quoted strings, operators, and identifiers."
  (let ((chars (concatenate 'list line))
        (tokens '())
        (current '()))
    (labels ((flush ()
               (when current
                 (let ((tok (coerce (reverse current) 'string)))
                   (unless (zerop (length tok))
                     (push tok tokens)))
                 (setf current nil)))
      (dolist (c chars)
        (cond
          ((char= c #\”)                              ; Quote starts quoted string
           (flush)
           (let ((str-chars '()))
             (loop for nc in chars
                   until (char= nc #\”)
                   do (push nc str-chars))
             (push (coerce (reverse (cons c (cons c str-chars)) 'string) tokens)
             (setf chars (nthcdr (1+ (length str-chars)) chars))))
          ((find c " \t\r\n" :test #'char=)
           (flush))
          ((find c "+-*/=><:,." :test #'char=)
           (flush)
           (push (string c) tokens))
          (t (push c current))))
      (flush)
      (nreverse (mapcar (lambda (tok)
                          (let ((typ (basic-token-type tok)))
                            (list typ (unless (member typ '(:NUMBER :STRING :IDENT)) tok))))
                        tokens))))

(defun basic-lex-source (source)
  "Lex complete BASIC SOURCE string into token stream."
  (let ((all-tokens '())
        (lines (split-sequence:split-sequence #\Newline source)))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\; (char trimmed 0))     ; Comment
                    (char= #\' (char trimmed 0)))    ; REM in BASIC
          (appendf all-tokens (basic-lex-line trimmed)))))
    all-tokens))

(defun basic-parse-line-prefix (tokens)
  "Parse line number and optional quoted label from start of TOKENS.
Returns (values line-number label remaining-tokens).
LINENO is the numeric start of line (e.g. 100).
LABEL is the quoted string after lineno (e.g. \"Loop\") or NIL.
REMAINING tokens after parsed prefix."
  (let ((lineno nil)
        (label nil))
    (when (and tokens (eq (first (first tokens)) :NUMBER))
      (setf lineno (second (first tokens)))
      (setf tokens (rest tokens))
      (when (and tokens (eq (first (first tokens)) :STRING))
        (setf label (second (first tokens)))
        (setf tokens (rest tokens))
        (when (and tokens (eq (first (first tokens)) :COLON))
          (setf tokens (rest tokens))))
    (values lineno label tokens)))