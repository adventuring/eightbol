;; src/basic-lexer.lisp — Tokenization for Dartmouth BASIC source
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defparameter *basic-keyword-alist*
  '(("REM" . :REM)
    ("LET" . :LET)
    ("FOR" . :FOR)
    ("TO" . :TO)
    ("STEP" . :STEP)
    ("NEXT" . :NEXT)
    ("IF" . :IF)
    ("THEN" . :THEN)
    ("ELSE" . :ELSE)
    ("GOTO" . :GOTO)
    ("GOSUB" . :GOSUB)
    ("RETURN" . :RETURN)
    ("LOG" . :LOG)
    ("FAULT" . :FAULT)
    ("STOP" . :STOP)
    ("METHOD" . :METHOD)
    ("ON" . :ON)
    ("CLASS" . :CLASS)
    ("PROCEDURE" . :PROCEDURE)
    ("END" . :END)
    ("WHILE" . :WHILE)
    ("UNTIL" . :UNTIL)
    ("DO" . :DO)
    ("LIBRARY" . :LIBRARY)
    ("AND" . :AND)
    ("OR" . :OR)
    ("NOT" . :NOT)
    ("PRINT" . :PRINT)
    ("INPUT" . :INPUT)
    ("DIALOGUE" . :DIALOGUE)
    ("SERVICE" . :SERVICE))
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
    ("<>" . :NE)
    ("." . :DOT)
    ("$" . :DOLLAR))
  "Association list of BASIC operators to token symbols.")

(defun basic-token-type (lexeme)
  "Return token type for LEXEME, a string."
  (cond
    ((and (stringp lexeme) (digit-char-p (char lexeme 0)))
     :NUMBER)
((and (stringp lexeme) (char= (char lexeme 0) #\"))
      :STRING)
    ((assoc lexeme *basic-keyword-alist* :test #'string-equal)
      (cdr (assoc lexeme *basic-keyword-alist* :test #'string-equal)))
    ((assoc lexeme *basic-operators-alist* :test #'string-equal)
      (cdr (assoc lexeme *basic-operators-alist* :test #'string-equal)))
    ((string= lexeme "(") :lparen)
    ((string= lexeme ")") :rparen)
    ((string= lexeme ",") :comma)
    ((string= lexeme ":") :colon)
    ((basic-valid-identifier-p lexeme) :ident)
    (t :unknown)))

(defun basic-normalize-identifier (s)
  "Normalize identifier S to PascalCase: HELLO → Hello, hELLO → Hello."
  (let ((lower (string-downcase s)))
    (when (plusp (length lower))
      (concatenate 'string
                   (string-upcase (subseq lower 0 1))
                   (subseq lower 1)))))

(defun basic-parse-number-literal (s)
  "Parse number literal S supporting decimal, octal (&o), hex (&h), binary (&b), dword (&d).
Returns (values decimal-value format-keyword) or NIL if invalid."
  (let ((trimmed (string-trim '(#\Space #\Tab) s)))
    (cond
      ;; Octal: &o followed by digits
      ((and (>= (length trimmed) 3) (string-equal (subseq trimmed 0 2) "&o"))
       (handler-case
           (let ((oct-str (subseq trimmed 2)))
             (when (every (lambda (c) (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))) oct-str)
               (values (parse-integer oct-str :radix 8) :octal)))
         (error () nil)))
      ;; Hex: &h followed by hex digits
      ((and (>= (length trimmed) 3) (string-equal (subseq trimmed 0 2) "&h"))
       (handler-case
           (values (parse-integer (subseq trimmed 2) :radix 16) :hex)
         (error () nil)))
      ;; Binary: &b followed by 0/1
      ((and (>= (length trimmed) 3) (string-equal (subseq trimmed 0 2) "&b"))
       (handler-case
           (let ((bin-str (subseq trimmed 2)))
             (when (every (lambda (c) (member c '(#\0 #\1))) bin-str)
               (values (parse-integer bin-str :radix 2) :binary)))
         (error () nil)))
      ;; Dword: &d followed by decimal digits
      ((and (>= (length trimmed) 3) (string-equal (subseq trimmed 0 2) "&d"))
       (handler-case
           (let ((dec-str (subseq trimmed 2)))
             (when (every #'digit-char-p dec-str)
               (values (parse-integer dec-str :radix 10) :dword)))
         (error () nil)))
      ;; Decimal (standard)
      ((every #'digit-char-p trimmed)
       (handler-case
           (values (parse-integer trimmed :radix 10) :decimal)
         (error () nil)))
      (t nil))))

(defun basic-valid-identifier-p (s)
  "Check if S is a valid alphanumeric BASIC identifier (case-insensitive input)."
  (and (stringp s)
       (plusp (length s))
       (every (lambda (c)
                (or (alphanumericp c) (char= c #\$) (char= c #\_)))
              s)))

(defun basic-lex-line (line)
  "Split LINE into token list of (type value) pairs.
Handles quoted strings, operators, identifiers, and multiple number formats.
Identifiers are normalized to PascalCase."
  (let ((tokens '())
        (chars (coerce line 'list))
        (i 0))
    (labels ((peek () (nth i chars))
             (next-char () (prog1 (nth i chars) (incf i)))
             (peek-ahead (n) (nth (+ i n) chars))
             (eof? () (>= i (length chars)))
             (skip-whitespace ()
               (loop while (and (not (eof?)) (find (peek) '(#\Space #\Tab #\Return #\Newline)))
                     do (next-char)))
             (flush-token (start end type)
               (let ((text (coerce (subseq chars start end) 'string)))
                 (unless (zerop (length text))
                   (push (cons type text) tokens))))
             (scan-number (start)
               ;; Try to parse prefixed numbers (&o, &h, &b, &d) or standard decimal
               (let ((text (coerce (subseq chars start (min (length chars) (+ start 10))) 'string)))
                 (multiple-value-bind (val fmt) (basic-parse-number-literal text)
                   (if val
                       ;; Consume the appropriate number of characters
                       (progn
                         (loop while (and (not (eof?)) 
                                         (or (digit-char-p (peek))
                                             (char= (peek) #\&)
                                             (member (char-upcase (peek)) '(#\O #\H #\B #\D))))
                               do (next-char))
                         (flush-token start i :number))
                       ;; Fall back to regular decimal number parsing
                       (progn
                         (loop while (and (not (eof?)) (digit-char-p (peek)))
                               do (next-char))
                         (flush-token start i :number))))))
             (scan-string (start)
               (next-char) ; skip opening quote
               (loop while (and (not (eof?)) (char/= (peek) #\"))
                     do (next-char))
               (when (not (eof?)) (next-char)) ; skip closing quote
               (flush-token (1+ start) (1- i) :string))
             (scan-identifier (start)
               (loop while (and (not (eof?))
                                (or (alphanumericp (peek))
                                    (char= (peek) #\$)
                                    (char= (peek) #\_)))
                     do (next-char))
               (let ((text (coerce (subseq chars start i) 'string)))
                 (push (cons :ident (basic-normalize-identifier text)) tokens)))))
      (loop until (eof?)
            do (skip-whitespace)
               (cond
                 ((eof?) (return))
                 ((char= (peek) #\") (scan-string i))
                 ((digit-char-p (peek)) (scan-number i))
                 ((or (alphanumericp (peek))
                      (char= (peek) #\$)
                      (char= (peek) #\_)) (scan-identifier i))
                 ((char= (peek) #\&)
                  ;; Might be start of prefixed number
                  (if (and (not (eof?)) (digit-char-p (peek-ahead 1)))
                      (scan-number i)
                      (let ((op1 (string (next-char))))
                        (flush-token (1- i) i :op))))
                 (t
                  (let ((op1 (string (next-char)))
                        (op2 (when (not (eof?)) (string (peek)))))
                    (cond
                      ((and op2 (string= op2 "=")
                            (member (concatenate 'string op1 op2) '("=<" ">=" "<>")
                                    :test #'string-equal))
                       (next-char) ; consume second char
                       (flush-token (1- i) i :op))
                      (t
                       (flush-token (1- i) i :op)))))))
       (nreverse tokens)))

(defun basic-lex-source (source)
  "Lex complete BASIC SOURCE string into token stream.
Skips REM comments only (no apostrophe or semicolon support)."
  (let ((all-tokens '())
        (in-rem nil))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (zerop (length trimmed))
          ;; Check if line starts with REM (case-insensitive)
          (cond
            ((or (string-equal trimmed "REM")
                 (and (>= (length trimmed) 4) (string-equal (subseq trimmed 0 3) "REM")))
             ;; Skip REM lines entirely
             (setf in-rem t))
            ((zerop (length trimmed))
             ;; Skip empty lines
             nil)
            (t
             ;; Lex the line
             (setf all-tokens (nconc all-tokens (basic-lex-line trimmed))))))))
    all-tokens))

(defun basic-parse-line-prefix (tokens)
  "Parse line number and optional quoted label from start of TOKENS.
Returns (values line-number label remaining-tokens)."
  (let ((lineno nil)
        (label nil))
    (when (and tokens (eq (first (first tokens)) :number))
      (setf lineno (second (first tokens)))
      (setf tokens (rest tokens))
      (when (and tokens (eq (first (first tokens)) :string))
        (setf label (second (first tokens)))
        (setf tokens (rest tokens))
        (when (and tokens (eq (first (first tokens)) :colon))
          (setf tokens (rest tokens)))))
    (values lineno label tokens)))

