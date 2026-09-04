;; src/frontend-goal/goal-lexer.lisp — Tokenization for GOAL source
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Number format parsing — supports decimal, hex #x, octal #o, binary #b, dword #d"..."
(defun parse-goal-number (number-string)
  "Parse GOAL number token. Supports:
#x... — hexadecimal literal
#o... — octal literal  
#b... — binary literal
#d\"...\" — dword/string literal (returns as string)
plain decimal — bare digits
Signals error on invalid digit sequences."
  (flet ((parse (string radix)
           (handler-case
               (parse-integer string :radix radix)
             (parse-error (e)
               (declare (ignore e))
               (error "Invalid number ~s for radix ~d" string radix)))))
    (cond
      ((< (length number-string) 1)
       (error "Empty number string"))
      ;; #xHEX — hex literal
      ((and (>= (length number-string) 3)
            (char= (char number-string 0) #\#)
            (char-equal (char number-string 1) #\x))
       (parse (subseq number-string 2) 16))
      ;; #oOCTAL — octal literal
      ((and (>= (length number-string) 3)
            (char= (char number-string 0) #\#)
            (char-equal (char number-string 1) #\o))
       (parse (subseq number-string 2) 8))
      ;; #bBINARY — binary literal
      ((and (>= (length number-string) 3)
            (char= (char number-string 0) #\#)
            (char-equal (char number-string 1) #\b))
       (parse (subseq number-string 2) 2))
      ;; #d"DWORD" — dword/string literal; return as string token
      ((and (>= (length number-string) 4)
            (char= (char number-string 0) #\#)
            (char-equal (char number-string 1) #\d)
            (char= (char number-string 2) #\"))
       (subseq number-string 3 (1- (length number-string))))
      ;; Plain decimal: bare digits
      (t (parse number-string 10)))))

(defun goal-normalize-identifier (name)
  "Convert identifier to kebab-case (lowercase with hyphens).
Examples: PlayerName → player-name
          player_score → player-score
          PLAYER_HEALTH → player-health
Underscores and camelCase transitions become hyphens.
Result is normalized to kebab-case for AST nodes."
  (let ((substituted (substitute #\- #\_ name)))
    ;; Insert hyphens before uppercase letters (camelCase→kebab-case)
    (with-output-to-string (out)
      (loop for i from 0 below (length substituted)
            for c = (char substituted i)
            do (when (and (> i 0)
                         (upper-case-p c)
                         (lower-case-p (char substituted (1- i))))
                 (write-char #\- out))
               (write-char (char-downcase c) out)))))

(defparameter *goal-keyword-alist*
  '(("defun" . :DEFUN)
    ("defmethod" . :DEFMETHOD)
    ("deftype" . :DEFTYPE)
    ("let" . :LET)
    ("let*" . :LETSTAR)
    ("if" . :IF)
    ("when" . :WHEN)
    ("unless" . :UNLESS)
    ("cond" . :COND)
    ("case" . :CASE)
    ("loop" . :LOOP)
    ("do" . :DO)
    ("while" . :WHILE)
    ("until" . :UNTIL)
    ("quote" . :QUOTE)
    ("quasiquote" . :QUASIQUOTE)
    ("unquote" . :UNQUOTE)
    ("lambda" . :LAMBDA)
    ("and" . :AND)
    ("or" . :OR)
    ("not" . :NOT)
    ("nil" . :NIL)
    ("true" . :TRUE)
    ("false" . :FALSE)
    ("set!" . :SET)
    ("setq" . :SETQ)
    ("return" . :RETURN)
    ("break" . :BREAK)
    ("continue" . :CONTINUE))
  "Association list of GOAL keywords to token symbols.")

(defparameter *goal-operators-alist*
  '(("+" . :PLUS)
    ("-" . :MINUS)
    ("*" . :TIMES)
    ("/" . :DIVIDE)
    ("%" . :MOD)
    ("=" . :EQUAL)
    ("<" . :LT)
    (">" . :GT)
    ("<=" . :LE)
    (">=" . :GE)
    ("!=" . :NE)
    ("==" . :EQ)
    ("<<" . :LSHIFT)
    (">>" . :RSHIFT)
    ("&" . :BITAND)
    ("|" . :BITOR)
    ("^" . :BITXOR)
    ("~" . :BITNOT)
    ("." . :DOT)
    ("->" . :ARROW))
  "Association list of GOAL operators to token symbols.")

(defun goal-valid-identifier-p (s)
  "Check if S is a valid GOAL identifier (alphanumeric, hyphen, underscore)."
  (and (stringp s)
       (plusp (length s))
       (every (lambda (c)
                (or (alphanumericp c) (char= c #\-) (char= c #\_)))
              s)))

(defun goal-lex-line (line)
  "Tokenize a GOAL source line into (token-type . lexeme) pairs."
  (let ((tokens '())
        (chars (coerce line 'list))
        (i 0))
    (labels ((peek () (nth i chars))
             (next-char () (prog1 (nth i chars) (incf i)))
             (eof? () (>= i (length chars)))
             (skip-whitespace ()
               (loop while (and (not (eof?)) (find (peek) '(#\Space #\Tab #\Return #\Newline)))
                     do (next-char)))
             (scan-number (start)
               ;; Scan # prefix for #x, #o, #b, #d formats or plain decimal
               (let ((first-char (peek)))
                 (when (char= first-char #\#)
                   (next-char)
                   (when (not (eof?))
                     (let ((format-char (char-upcase (peek))))
                       (when (member format-char '(#\X #\O #\B #\D))
                         (next-char)
                         ;; For #d"...", scan until closing quote
                         (if (and (char-equal format-char #\D)
                                  (not (eof?)) (char= (peek) #\"))
                             (progn
                               (next-char) ; consume opening quote
                               (loop while (and (not (eof?)) (char/= (peek) #\"))
                                     do (next-char))
                               (unless (eof?) (next-char))) ; consume closing quote
                             ;; For #x, #o, #b, scan hex/octal/binary digits
                             (loop while (and (not (eof?))
                                             (or (digit-char-p (peek))
                                                 (member (char-upcase (peek))
                                                         '(#\A #\B #\C #\D #\E #\F))))
                                   do (next-char)))))))
                 ;; If not # format, scan plain decimal
                 (unless (char= first-char #\#)
                   (loop while (and (not (eof?)) (digit-char-p (peek)))
                         do (next-char)))))
               (let ((text (coerce (subseq chars start i) 'string)))
                 (let ((parsed (parse-goal-number text)))
                   (if (stringp parsed)
                       (push (cons :dword parsed) tokens)
                       (push (cons :number (format nil "~d" parsed)) tokens)))))
             (scan-string (start)
               (next-char) ; skip opening quote
               (loop while (and (not (eof?)) (char/= (peek) #\"))
                     do (if (char= (peek) #\\)
                            (progn (next-char) (unless (eof?) (next-char)))
                            (next-char)))
               (when (not (eof?)) (next-char)) ; skip closing quote
               (let ((text (coerce (subseq chars (1+ start) (1- i)) 'string)))
                 (push (cons :string text) tokens)))
             (scan-identifier (start)
               (loop while (and (not (eof?))
                               (or (alphanumericp (peek))
                                   (char= (peek) #\-) (char= (peek) #\_)
                                   (char= (peek) #\!)))
                     do (next-char))
               (let ((text (coerce (subseq chars start i) 'string))
                     (upper-text (string-upcase (coerce (subseq chars start i) 'string))))
                 (let ((keyword (assoc upper-text *goal-keyword-alist* :test #'string-equal)))
                   (if keyword
                       (push (cons (cdr keyword) text) tokens)
                       (let ((normalized (goal-normalize-identifier text)))
                         (push (cons :ident normalized) tokens))))))
             (scan-operator ()
               (let ((c1 (peek)))
                 (cond
                   ((eof?) nil)
                   ;; Two-char operators
                   ((and (not (eof?)) (< (1+ i) (length chars))
                         (or (and (char= c1 #\<) (char= (nth (1+ i) chars) #\=))
                             (and (char= c1 #\>) (char= (nth (1+ i) chars) #\=))
                             (and (char= c1 #\!) (char= (nth (1+ i) chars) #\=))
                             (and (char= c1 #\=) (char= (nth (1+ i) chars) #\=))
                             (and (char= c1 #\<) (char= (nth (1+ i) chars) #\<))
                             (and (char= c1 #\>) (char= (nth (1+ i) chars) #\>))
                             (and (char= c1 #\-) (char= (nth (1+ i) chars) #\>))))
                    (let ((op (coerce (list c1 (nth (1+ i) chars)) 'string)))
                      (next-char) (next-char)
                      (let ((kw (assoc op *goal-operators-alist* :test #'string-equal)))
                        (when kw (push (cons (cdr kw) op) tokens)))))
                   ;; Single-char operators and delimiters
                   (t
                    (let ((op (coerce (list c1) 'string)))
                      (next-char)
                      (let ((kw (assoc op *goal-operators-alist* :test #'string-equal)))
                        (cond
                          (kw (push (cons (cdr kw) op) tokens))
                          ((char= c1 #\() (push (cons :lparen "(") tokens))
                          ((char= c1 #\)) (push (cons :rparen ")") tokens))
                          ((char= c1 #\[) (push (cons :lbracket "[") tokens))
                          ((char= c1 #\]) (push (cons :rbracket "]") tokens))
                          ((char= c1 #\{) (push (cons :lbrace "{") tokens))
                          ((char= c1 #\}) (push (cons :rbrace "}") tokens))
                          ((char= c1 #\comma) (push (cons :comma ",") tokens))
                          ((char= c1 #\;) (push (cons :semicolon ";") tokens))
                           ((char= c1 #\apostrophe) (push (cons :quote "'") tokens))
                          ((char= c1 #\`) (push (cons :quasiquote "`") tokens))
                          ((char= c1 #\comma) (push (cons :unquote ",") tokens)))))))))
      (loop until (eof?) do
        (skip-whitespace)
        (unless (eof?)
          (let ((c (peek)))
            (cond
              ;; # prefix for numbers and dword
              ((and (char= c #\#)
                    (< (1+ i) (length chars))
                    (or (member (char-upcase (char chars (1+ i))) '(#\X #\O #\B #\D))
                        (digit-char-p (char chars (1+ i)))))
               (scan-number i))
              ;; Plain decimal
              ((digit-char-p c) (scan-number i))
              ;; Quoted strings
              ((char= c #\") (scan-string i))
              ;; Identifiers (including hyphenated and underscored)
              ((or (alpha-char-p c) (char= c #\_) (char= c #\-)) (scan-identifier i))
              ;; Comments: semicolon to end of line
              ((char= c #\;)
               (loop while (and (not (eof?)) (char/= (peek) #\Newline))
                     do (next-char)))
              ;; Operators and delimiters
              (t (scan-operator))))))
      (nreverse tokens))))

(defun goal-lex-source (source)
  "Lex complete GOAL SOURCE string into token stream."
  (let ((all-tokens '()))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (zerop (length trimmed))
          (setf all-tokens (nconc all-tokens (goal-lex-line trimmed))))))
    all-tokens))

(defun goal-lex-token ()
   "Read next token from *standard-input*."
   (declare (special *goal-lex-token-buffer*))
   (cond
     ((and (boundp '*goal-lex-token-buffer*) *goal-lex-token-buffer*)
      (prog1 (first *goal-lex-token-buffer*)
        (setf *goal-lex-token-buffer* (rest *goal-lex-token-buffer*))))
     (t
      (let ((line (read-line *standard-input* nil nil)))
        (when line
          (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
            (when (and (plusp (length trimmed)) (char/= #\; (char trimmed 0)))
              (setf *goal-lex-token-buffer* (goal-lex-line trimmed))
              (goal-lex-token))))))))

(defun goal-token-list ()
   "Return a thunk that reads tokens from *standard-input* using goal-lex-token."
   (lambda ()
     (let ((token (goal-lex-token)))
       (when token
         (cons token (funcall (goal-token-list))))))
