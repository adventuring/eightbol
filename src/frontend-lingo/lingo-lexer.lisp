;; src/frontend-lingo/lingo-lexer.lisp — Tokenization for Lingo source
;; 
;; Heritage notes:
;; - Based on minimal Lingo subset for PDA-era hardware
;; - Pixel-limited output through .LogFault
;; - Object/event system requires manual OOPS patterns
;; - Arrays supported via collection structures (basic containment strategies)
;; - Game state via :move operations with manual memory tracking
;; - LCD constraints: framebuffer only, one buffered 8/16 color display
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defparameter *lingo-keyword-alist*
  '(("ON" . :ON)
    ("END" . :END)
    ("IF" . :IF)
    ("THEN" . :THEN)
    ("ELSE" . :ELSE)
    ("END IF" . :ENDIF)
    ("REPEAT" . :REPEAT)
    ("WITH" . :WITH)
    ("FOR" . :FOR)
    ("IN" . :IN)
    ("TO" . :TO)
    ("STEP" . :STEP)
    ("UP" . :UP)
    ("DOWN" . :DOWN)
    ("WHERE" . :WHERE)
    ("WHILE" . :WHILE)
    ("UNTIL" . :UNTIL)
    ("DO" . :DO)
    ("LOOP" . :LOOP)
    ("EXIT" . :EXIT)
    ("NEXT" . :NEXT)
    ("PUT" . :PUT)
    ("GET" . :GET)
    ("SET" . :SET)
    ("CALL" . :CALL)
    ("GOTO" . :GOTO)
    ("GO" . :GO)
    ("FRAME" . :FRAME)
    ("PROPERTIES" . :PROPERTIES)
    ("PROPERTY" . :PROPERTY)
    ("TELL" . :TELL)
    ("END TELL" . :ENDTELL)
    ("METHOD" . :METHOD)
    ("PROCEDURE" . :PROCEDURE)
    ("CLASS" . :CLASS)
    ("RETURN" . :RETURN)
    ("PLAY" . :PLAY)
    ("STOP" . :STOP)
    ("HALT" . :HALT)
    ("LIBRARY" . :LIBRARY)
    ("SERVICE" . :SERVICE)
    ("BANK" . :BANK)
    ("OF" . :OF)
    ("THE" . :THE)
    ("AND" . :AND)
    ("OR" . :OR)
    ("NOT" . :NOT)
    ("TRUE" . :TRUE)
    ("FALSE" . :FALSE)
    ("NULL" . :NULL)
    ("SELF" . :SELF)
    ("ASSEMBLY" . :ASSEMBLY)
    ("ENTRY" . :ENTRY)
    ("COPY" . :COPY)
    ("AS" . :AS)
    ("DATA" . :DATA))
  "Association list of Lingo keywords to token symbols.")

(defparameter *lingo-operators-alist*
  '(("+" . :PLUS)
    ("-" . :MINUS)
    ("*" . :TIMES)
    ("/" . :DIVIDE)
    ("=" . :EQUAL)
    ("==" . :EQ)
    ("<>" . :NE)
    ("!=" . :NEQ)
    ("<" . :LT)
    (">" . :GT)
    ("<=" . :LE)
    (">=" . :GE)
    (":" . :COLON)
    ("," . :COMMA)
    ("(" . :LPAREN)
    (")" . :RPAREN)
    ("{" . :LCURL)
    ("}" . :RCURL)
    ("[" . :LBRACKET)
    ("]" . :RBRACKET)
    ("." . :DOT)
    ("::=" . :COLONEQ))
  "Association list of Lingo operators to token symbols.")

(defun lingo-valid-identifier-p (s)
  "Check if S is a valid Lingo identifier (PascalCase or camelCase)."
  (and (stringp s)
       (plusp (length s))
       (every (lambda (c)
                (or (alphanumericp c)
                    (char= c #\$)
                    (char= c #\_)))
              s)))

(defun lingo-token-type (lexeme)
  "Return token type for LEXEME, a string."
  (cond
    ((and (stringp lexeme)
          (every (lambda (c) (digit-char-p c)) lexeme))
     :NUMBER)
    ((and (stringp lexeme) (> (length lexeme) 0) (char= (char lexeme 0) #\"))
     :STRING)
    ((assoc lexeme *lingo-keyword-alist* :test #'string-equal)
     (cdr (assoc lexeme *lingo-keyword-alist* :test #'string-equal)))
    ((assoc lexeme *lingo-operators-alist* :test #'string-equal)
     (cdr (assoc lexeme *lingo-operators-alist* :test #'string-equal)))
    ((string= lexeme "(") :LPAREN)
    ((string= lexeme ")") :RPAREN)
    ((string= lexeme ",") :COMMA)
    ((string= lexeme ":") :COLON)
    ((lingo-valid-identifier-p lexeme) :IDENT)
    (t :UNKNOWN)))

(defun lingo-lex-line (line)
  "Split LINE into token list of (type value) pairs.
Handles quoted strings, operators, and identifiers."
  (let ((tokens '())
        (chars (coerce line 'list))
        (i 0))
    (labels ((peek () (nth i chars))
             (next-char () (prog1 (nth i chars) (incf i)))
             (eof? () (>= i (length chars)))
             (skip-whitespace ()
               (loop while (and (not (eof?))
                                (find (peek) '(#\Space #\Tab #\Return #\Newline)))
                     do (next-char)))
             (flush-token (start end type)
               (let ((text (coerce (subseq chars start end) 'string)))
                 (unless (zerop (length text))
                   (push (cons type text) tokens))))
             (scan-number (start)
               (loop while (and (not (eof?))
                                (or (digit-char-p (peek))
                                    (char= (peek) #\.)))
                     do (next-char))
               (flush-token start (1- i) :NUMBER))
             (scan-string (start)
               (next-char) ; skip opening quote
               (loop while (and (not (eof?)) (char/= (peek) #\"))
                     do (next-char))
               (when (not (eof?)) (next-char)) ; skip closing quote
               (let ((text (coerce (subseq chars (1+ start) (1- i)) 'string)))
                 (push (cons :STRING text) tokens)))
             (scan-identifier (start)
               (loop while (and (not (eof?))
                                 (or (alphanumericp (peek))
                                     (char= (peek) #\$)
                                     (char= (peek) #\_)
                                     (char= (peek) #\-)))
                     do (next-char))
               (let ((text (coerce (subseq chars start (1- i)) 'string)))
                 (push (cons (lingo-token-type text) text) tokens)))))
       (loop until (eof?)
             do (skip-whitespace)
                (cond
                  ((eof?) (return))
                  ((char= (peek) #\") (scan-string i))
                  ((digit-char-p (peek)) (scan-number i))
                  ((or (alphanumericp (peek))
                       (char= (peek) #\$)
                       (char= (peek) #\_)
                       (char= (peek) #\-)) (scan-identifier i))
                  (t
                   (let* ((op1 (string (next-char)))
                          (op2 (when (not (eof?)) (string (peek)))))
                     (cond
                       ((and op2 (string= (concatenate 'string op1 op2) "="))
                        (next-char) ; consume second char
                        (let ((op-tok (concatenate 'string op1 op2)))
                          (push (cons (lingo-token-type op-tok) op-tok) tokens)))
                       ((and op2 (string= (concatenate 'string op1 op2) ":="))
                        (next-char)
                        (push (cons :COLONEQ ":=") tokens))
                       ((string= op1 ":")
                        (push (cons :COLON ":") tokens))
                       (t
                         (let ((typ (lingo-token-type op1)))
                           (push (cons typ op1) tokens))))))))
        (nreverse tokens)))

(defun lingo-lex-source (source)
  "Lex complete Lingo SOURCE string into token stream."
  (let ((all-tokens '()))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\; (char trimmed 0))
                    (char= #\* (char trimmed 0)))
          (setf all-tokens (nconc all-tokens (lingo-lex-line trimmed))))))
    all-tokens))

(defun lingo-join-keywords (tokens)
  "Join tokens that form multi-word keywords like END IF."
  (let ((result '())
        (i 0))
    (loop while (< i (length tokens))
          do (let ((curr (nth i tokens)))
               (if (and (eq (first curr) :END) (< (1+ i) (length tokens))
                        (eq (first (nth (+ i 1) tokens)) :IF))
                   (progn
                     (push (cons :ENDIF "END IF") result)
                     (incf i 2))
                   (progn
                     (push curr result)
                     (incf i)))))
    (nreverse result)))

(defun lingo-lex (source)
  "Lex Lingo source string into token list for YACC."
  (let ((tokens (lingo-lex-source source)))
    (lingo-join-keywords
     (mapcar (lambda (tok)
               (list (first tok) (second tok)))
             tokens))))

(defun lingo-parse-line-prefix (tokens)
  "Parse line number and optional quoted label from start of TOKENS.
Returns (values line-number label remaining-tokens)."
  (declare (ignore tokens))
  (values nil nil tokens))

(defun make-goto-node (target)
  "Build a :goto AST node."
  (list :goto :target target))

(defun make-call-with-service (service)
  "Build a :call AST node with :service."
  (list :call :service service :bank nil))

(defun make-call-with-library (target)
  "Build a :call AST node with :library t."
  (list :call :target target :bank nil :library t))

(defun make-call-with-bank (name bank)
  "Build a :call AST node with :bank specified."
  (list :call :target name :bank bank))

(defun make-log-fault-node (code)
  "Build a :log-fault AST node from a string token."
  (list :log-fault :code code))

(defun make-debug-break-node (code)
  "Build a :stop-run AST node from a string token."
  (list :stop-run :code code))