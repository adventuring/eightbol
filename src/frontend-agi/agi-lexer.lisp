;; src/frontend-agi/agi-lexer.lisp — Tokenization for AGI language
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Number format parsing — supports decimal, octal, hex, binary, and dword literals
(defun parse-agi-number (number-string)
  "Parse number token. Supports:
d'...' — decimal literal
x'...' — hexadecimal literal  
o'...' — octal literal
b'...' — binary literal
w'...'|w\"...\" — word/dword string (for assembler)
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
      ((and (char-equal (char number-string 0) #\d)
            (> (length number-string) 3)
            (char= (char number-string 1) #\apostrophe))
       (parse (subseq number-string 2 (1- (length number-string))) 10))
      ((and (char-equal (char number-string 0) #\x)
            (> (length number-string) 3)
            (char= (char number-string 1) #\apostrophe))
        (let ((digits (remove #\comma (subseq number-string 2 (1- (length number-string))))))
         (parse digits 16)))
      ((and (char-equal (char number-string 0) #\o)
            (> (length number-string) 3)
            (char= (char number-string 1) #\apostrophe))
       (parse (subseq number-string 2 (1- (length number-string))) 8))
      ((and (char-equal (char number-string 0) #\b)
            (> (length number-string) 3)
            (char= (char number-string 1) #\apostrophe))
       (parse (subseq number-string 2 (1- (length number-string))) 2))
      ;; w'XXXX'|w"XXXX" — word/dword string; return as string token for backend.
      ((and (char-equal (char number-string 0) #\w)
            (> (length number-string) 3)
            (or (char= (char number-string 1) #\apostrophe)
                (char= (char number-string 1) #\")))
       (subseq number-string 2 (1- (length number-string))))
      ;; Plain decimal: bare digits
      (t (parse number-string 10)))))

(defun agi-normalize-identifier (name)
  "Convert identifier to Header.Case with dots instead of hyphens.
Examples: player-name → Player.Name
           player_score → Player.Score
           playerName → Player.Name
Hyphens and underscores are both converted to dots in the output.
CamelCase is split into segments at case transitions.
Each segment between dots is capitalized, case-insensitive input."
  (let* (;; Convert underscores to hyphens for consistent processing
         (normalized (substitute #\- #\_ name))
         ;; Split camelCase by inserting hyphens before uppercase letters
         (with-case-split (let ((result (list)))
                            (dotimes (i (length normalized))
                              (let ((c (char normalized i)))
                                (when (and (> i 0)
                                          (upper-case-p c)
                                          (not (char= (char normalized (1- i)) #\-)))
                                  (push #\- result))
                                (push c result)))
                            (coerce (nreverse result) 'string)))
         ;; Split on hyphens and dots
         (parts (split-sequence:split-sequence-if (lambda (c) (or (char= c #\-) (char= c #\.)))
                                                   with-case-split
                                                   :remove-empty-subseqs t))
         ;; Titlecase each part (first letter uppercase, rest lowercase)
         (titlecased (mapcar (lambda (s)
                               (if (zerop (length s))
                                   s
                                   (concatenate 'string
                                               (string (char-upcase (char s 0)))
                                               (when (> (length s) 1)
                                                 (string-downcase (subseq s 1))))))
                             parts)))
    ;; Join parts with dots
    (format nil "~{~a~^.~}" titlecased)))

(defparameter *agi-keyword-alist*
  '(("IF" . :IF)
    ("ELSE" . :ELSE)
    ("END-IF" . :END-IF)
    ("THEN" . :THEN)
    ("GOSUB" . :GOSUB)
    ("GOTO" . :GOTO)
    ("NEWROOM" . :NEWROOM)
    ("LOADLOGICS" . :LOADLOGICS)
    ("RETURN" . :RETURN)
    ("QUIT" . :QUIT)
    ("ASSIGN" . :ASSIGN)
    ("INCREMENT" . :INCREMENT)
    ("DECREMENT" . :DECREMENT)
    ("SET" . :SET)
    ("RESET" . :RESET)
    ("SAID" . :SAID)
    ("TEST" . :TEST)
    ("POSN" . :POSN)
    ("CONTROLLER" . :CONTROLLER)
    ("HAVEKEY" . :HAVEKEY)
    ("AND" . :AND)
    ("OR" . :OR)
    ("NOT" . :NOT)
    ("PRINT" . :PRINT)
    ("INPUT" . :INPUT)
    ("DIALOGUE" . :DIALOGUE))
  "Association list of AGI keywords to token symbols.")

(defparameter *agi-operators-alist*
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
    ("<<" . :LSHIFT)
    (">>" . :RSHIFT)
    ("." . :DOT))
  "Association list of AGI operators to token symbols.")

(defun agi-valid-identifier-p (s)
  "Check if S is a valid AGI identifier."
  (and (stringp s)
       (plusp (length s))
       (every (lambda (c)
                (or (alphanumericp c) (char= c #\$) (char= c #\_)))
              s)))

(defun agi-lex-line (line)
  "Tokenize an AGI source line into (token-type . lexeme) pairs."
  (let ((tokens '())
        (i 0))
    (labels ((peek () (when (< i (length line)) (char line i)))
             (next-char () (prog1 (char line i) (incf i)))
             (eof? () (>= i (length line)))
             (skip-whitespace ()
               (loop while (and (not (eof?)) (find (peek) '(#\Space #\Tab #\Return #\Newline)))
                     do (next-char)))
             (scan-number (start)
               ;; Scan initial character and any following characters that form the number.
               ;; Supports formats: d'123', x'FF', o'777', b'1010', w'data', or plain decimal 123
               (let ((first-char (peek)))
                 (when (member (char-upcase first-char) '(#\D #\X #\O #\B #\W))
                   (next-char)  ; consume format prefix
                   (when (not (eof?))
                     (next-char) ; consume opening quote/apostrophe
                     (loop while (and (not (eof?)) (char/= (peek) #\apostrophe) (char/= (peek) #\"))
                           do (next-char))
                     (unless (eof?) (next-char)))) ; consume closing quote/apostrophe
                 (unless (member (char-upcase first-char) '(#\D #\X #\O #\B #\W))
                   (loop while (and (not (eof?)) (digit-char-p (peek)))
                         do (next-char))))
               (let ((text (subseq line start i)))
                 (let ((parsed (parse-agi-number text)))
                   (if (stringp parsed)
                       (push (cons :string parsed) tokens)
                       (push (cons :number (format nil "~d" parsed)) tokens)))))
             (scan-string (start)
               (next-char) ; skip opening quote
               (loop while (and (not (eof?)) (char/= (peek) #\"))
                     do (next-char))
               (when (not (eof?)) (next-char)) ; skip closing quote
               (let ((text (subseq line (1+ start) (1- i))))
                 (push (cons :string text) tokens)))
             (scan-identifier (start)
               (loop while (and (not (eof?)) (or (alphanumericp (peek)) (char= (peek) #\_) (char= (peek) #\-)))
                     do (next-char))
               (let ((text (subseq line start i))
                     (upper-text (string-upcase (subseq line start i))))
                 (let ((keyword (assoc upper-text *agi-keyword-alist* :test #'string-equal)))
                   (if keyword
                       (push (cons (cdr keyword) text) tokens)
                       (let ((normalized (agi-normalize-identifier text)))
                         (push (cons :ident normalized) tokens))))))
             (scan-operator ()
               (let ((c1 (peek)))
                 (cond
                   ((eof?) nil)
                   ((and (not (eof?)) (< (1+ i) (length line))
                         (or (and (char= c1 #\<) (char= (char line (1+ i)) #\=))
                             (and (char= c1 #\>) (char= (char line (1+ i)) #\=))
                             (and (char= c1 #\<) (char= (char line (1+ i)) #\>))
                             (and (char= c1 #\<) (char= (char line (1+ i)) #\<))
                             (and (char= c1 #\>) (char= (char line (1+ i)) #\>))))
                    (let ((op (subseq line i (+ i 2))))
                      (next-char) (next-char)
                      (let ((kw (assoc op *agi-operators-alist* :test #'string-equal)))
                        (when kw (push (cons (cdr kw) op) tokens)))))
                   (t
                    (let ((op (subseq line i (1+ i))))
                      (next-char)
                      (let ((kw (assoc op *agi-operators-alist* :test #'string-equal)))
                        (cond
                          (kw (push (cons (cdr kw) op) tokens))
                          ((char= c1 #\() (push (cons :lparen "(") tokens))
                          ((char= c1 #\)) (push (cons :rparen ")") tokens))
                            ((char= c1 #\comma) (push (cons :comma ",") tokens))
                            ((char= c1 #\:) (push (cons :colon ":") tokens))))))))))
      (loop until (eof?) do
        (skip-whitespace)
        (unless (eof?)
          (let ((c (peek)))
            (cond
              ;; Format prefixes (d, x, o, b, w) followed by quote/apostrophe indicate numbers
              ((and (member (char-upcase c) '(#\D #\X #\O #\B #\W))
                    (< (1+ i) (length line))
                    (or (char= (char line (1+ i)) #\apostrophe)
                        (char= (char line (1+ i)) #\")))
               (scan-number i))
              ((digit-char-p c) (scan-number i))
              ((char= c #\") (scan-string i))
              ((or (alpha-char-p c) (char= c #\_) (char= c #\-)) (scan-identifier i))
              (t (scan-operator))))))
      (nreverse tokens))))

(defun agi-lex-source (source)
  "Lex complete AGI source string into a flat token stream."
  (loop for line in (split-sequence:split-sequence #\Newline source)
        append (agi-lex-line line)))

(defun agi-lex-token ()
   "Read next token from *standard-input*."
   (declare (special *agi-lex-token-buffer*))
   (cond
     ((and (boundp '*agi-lex-token-buffer*) *agi-lex-token-buffer*)
      (prog1 (first *agi-lex-token-buffer*)
        (setf *agi-lex-token-buffer* (rest *agi-lex-token-buffer*))))
     (t
      (let ((line (read-line *standard-input* nil nil)))
        (when line
          (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
            (unless (or (zerop (length trimmed))
                        (char= #\; (char trimmed 0)))
              (setf *agi-lex-token-buffer* (agi-lex-line trimmed))
              (agi-lex-token))))))))

(defun agi-token-list ()
   "Return a thunk that reads tokens from *standard-input* using agi-lex-token."
   (lambda ()
     (let ((token (agi-lex-token)))
       (when token
         (cons token (funcall (agi-token-list)))))))
