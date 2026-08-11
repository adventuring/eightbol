;; src/frontend-smalltalk/smalltalk-lexer.lisp — Tokenization for SmallTalk source
;; 
;; Supports number literal formats: decimal, octal, hex, binary, and dword.
;; Identifiers are normalized to camelCase during lexing.
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defparameter *smalltalk-keyword-alist*
  '(("ifTrue" . :IF-TRUE)
     ("ifFalse" . :IF-FALSE)
     ("ifTrue:ifFalse:" . :IF-TRUE-FALSE)
     ("whileTrue:" . :WHILE-TRUE)
     ("whileFalse:" . :WHILE-FALSE)
     ("to:do:" . :TO-DO)
     ("do:" . :DO)
     ("new" . :NEW)
     ("self" . :SELF)
     ("super" . :SUPER)
     ("nil" . :NIL)
     ("true" . :TRUE)
     ("false" . :FALSE)
     ("and" . :AND)
     ("or" . :OR)
     ("not" . :NOT)
     ("dialogue" . :DIALOGUE)
     ("print" . :PRINT)
     ("input" . :INPUT)
     ("display" . :DISPLAY)
     ("say" . :SAY)
     ("ask" . :ASK)
     ("read" . :READ)
     ("COPY" . :COPY))
  "Association list of SmallTalk keywords to token symbols.")

(defparameter *smalltalk-operators-alist*
  '(("+". :PLUS)
    ("-" . :MINUS)
    ("*" . :TIMES)
    ("/" . :DIVIDE)
    ("=" . :EQUAL)
    ("==" . :EQ)
    ("~=" . :NE)
    ("<" . :LT)
    (">" . :GT)
    ("<=" . :LE)
    (">=" . :GE)
    (":=" . :ASSIGN)
    ("." . :PERIOD)
    (";" . :SEMICOLON)
    (":" . :COLON)
    ("|" . :VBAR)
    ("[" . :LBRACKET)
    ("]" . :RBRACKET)
    ("(" . :LPAREN)
    (")" . :RPAREN)
    ("#" . :HASH)
    ("@" . :AT))
  "Association list of SmallTalk operators to token symbols.")

(defun smalltalk-to-camel-case (s)
  "Convert string S to camelCase (first letter lowercase, rest PascalCase).
Examples: 'myVariable' stays 'myVariable', 'MyVariable' becomes 'myVariable'."
  (if (zerop (length s))
      s
      (let ((first-lower (string-downcase (char s 0)))
            (rest (if (> (length s) 1)
                      (subseq s 1)
                      "")))
        (concatenate 'string first-lower rest))))

(defun smalltalk-valid-identifier-p (s)
  "Check if S is a valid SmallTalk identifier (letters, digits, underscores).
Must start with a letter."
  (and (stringp s)
       (plusp (length s))
       (alpha-char-p (char s 0))
       (every (lambda (c)
                (or (alphanumericp c)
                    (char= c #\_)))
              s)))

(defun smalltalk-parse-number (lexeme)
  "Parse number LEXEME into (value . format-type).
Supports: decimal, octal (0o or 8r prefix), hex (0x or 16r prefix), binary (0b or 2r prefix), 
dword (0d\"WORD\" format).
Returns (value . :NUMBER-TYPE) or (value . :INVALID)."
  (cond
    ;; Dword format: 0d"<string>" or 0d'<string>' (convert string to 32-bit integer)
    ((and (> (length lexeme) 3)
          (or (string-equal (subseq lexeme 0 2) "0d\"")
              (string-equal (subseq lexeme 0 2) "0d'")))
     (let* ((quote-char (if (char= (char lexeme 2) #\") #\" #\apostrophe))
            (quote-pos (position quote-char lexeme :start 2))
            (string-part (and quote-pos (subseq lexeme 3 quote-pos))))
       (if (and quote-pos (stringp string-part) (= (length string-part) 4))
           ;; Convert 4-character string to 32-bit value
           (let ((val 0))
             (dotimes (i 4)
               (setf val (+ (ash val 8) (char-code (char string-part i)))))
             (cons val :DWORD))
           (cons 0 :INVALID))))
    ;; Hex format: 16r<hex> (old style)
    ((and (> (length lexeme) 3)
          (string-equal (subseq lexeme 0 3) "16r"))
     (let ((hex-part (subseq lexeme 3)))
       (if (every (lambda (c) (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
                                          #\8 #\9 #\a #\A #\b #\B #\c #\C
                                          #\d #\D #\e #\E #\f #\F)))
                  hex-part)
           (cons (parse-integer hex-part :radix 16) :HEX)
           (cons 0 :INVALID))))
    ;; Hex format: 0x<hex> (modern style)
    ((and (> (length lexeme) 2)
          (string-equal (subseq lexeme 0 2) "0x"))
     (let ((hex-part (subseq lexeme 2)))
       (if (every (lambda (c) (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
                                          #\8 #\9 #\a #\A #\b #\B #\c #\C
                                          #\d #\D #\e #\E #\f #\F)))
                  hex-part)
           (cons (parse-integer hex-part :radix 16) :HEX)
           (cons 0 :INVALID))))
    ;; Binary format: 2r<binary> (old style)
    ((and (> (length lexeme) 2)
          (string-equal (subseq lexeme 0 2) "2r"))
     (let ((bin-part (subseq lexeme 2)))
       (if (every (lambda (c) (member c '(#\0 #\1)))
                  bin-part)
           (cons (parse-integer bin-part :radix 2) :BINARY)
           (cons 0 :INVALID))))
    ;; Binary format: 0b<binary> (modern style)
    ((and (> (length lexeme) 2)
          (string-equal (subseq lexeme 0 2) "0b"))
     (let ((bin-part (subseq lexeme 2)))
       (if (every (lambda (c) (member c '(#\0 #\1)))
                  bin-part)
           (cons (parse-integer bin-part :radix 2) :BINARY)
           (cons 0 :INVALID))))
    ;; Octal format: 8r<octal> (old style)
    ((and (> (length lexeme) 2)
          (string-equal (subseq lexeme 0 2) "8r"))
     (let ((oct-part (subseq lexeme 2)))
       (if (every (lambda (c) (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))
                  oct-part)
           (cons (parse-integer oct-part :radix 8) :OCTAL)
           (cons 0 :INVALID))))
    ;; Octal format: 0o<octal> (modern style)
    ((and (> (length lexeme) 2)
          (string-equal (subseq lexeme 0 2) "0o"))
     (let ((oct-part (subseq lexeme 2)))
       (if (every (lambda (c) (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))
                  oct-part)
           (cons (parse-integer oct-part :radix 8) :OCTAL)
           (cons 0 :INVALID))))
    ;; Decimal format (including floating-point and scientific notation)
    (t
     (handler-case
         (cons (read-from-string lexeme) :DECIMAL)
       (error () (cons 0 :INVALID))))))

(defun smalltalk-token-type (lexeme)
  "Return token type for LEXEME, a string."
  (cond
    ;; Numbers: various formats (0x, 0b, 0o, 16r, 8r, 2r, 0d"...", decimal)
    ((or (and (stringp lexeme) (> (length lexeme) 2)
              (or (string-equal (subseq lexeme 0 2) "0x")
                  (string-equal (subseq lexeme 0 2) "0b")
                  (string-equal (subseq lexeme 0 2) "0o")
                  (string-equal (subseq lexeme 0 2) "0d")
                  (string-equal (subseq lexeme 0 2) "2r")
                  (string-equal (subseq lexeme 0 2) "8r")))
         (and (stringp lexeme) (> (length lexeme) 3)
              (string-equal (subseq lexeme 0 3) "16r"))
         (and (stringp lexeme) (every #'digit-char-p lexeme))
         (and (stringp lexeme)
              (position #\. lexeme)
              (every (lambda (c) (or (digit-char-p c) (char= c #\.)))
                     (remove #\e (remove #\E lexeme)))))
     :NUMBER)
    ;; Strings: single-quoted
    ((and (stringp lexeme) (> (length lexeme) 0) (char= (char lexeme 0) #\apostrophe))
     :STRING)
    ;; Symbols: hash-prefixed
    ((and (stringp lexeme) (> (length lexeme) 0) (char= (char lexeme 0) #\#))
     :SYMBOL)
    ;; Keywords
    ((assoc lexeme *smalltalk-keyword-alist* :test #'string-equal)
     (cdr (assoc lexeme *smalltalk-keyword-alist* :test #'string-equal)))
    ;; Operators
    ((assoc lexeme *smalltalk-operators-alist* :test #'string-equal)
     (cdr (assoc lexeme *smalltalk-operators-alist* :test #'string-equal)))
    ;; Identifiers
    ((smalltalk-valid-identifier-p lexeme)
     :IDENT)
    (t :UNKNOWN)))

(defun smalltalk-lex-line (line)
  "Split LINE into token list of (type value) pairs.
Handles quoted strings, numbers (multiple formats), symbols, operators, and identifiers.
Identifiers are normalized to camelCase."
  (let ((tokens '())
        (chars (coerce line 'list))
        (i 0))
    (labels ((peek () (when (< i (length chars)) (nth i chars)))
             (peek-ahead (n) (when (< (+ i n) (length chars)) (nth (+ i n) chars)))
             (next-char () (prog1 (nth i chars) (incf i)))
             (eof? () (>= i (length chars)))
             (skip-whitespace ()
               (loop while (and (not (eof?))
                                (find (peek) '(#\Space #\Tab #\Return #\Newline)))
                     do (next-char)))
             (flush-token (start end type)
               (let ((text (coerce (subseq chars start end) 'string)))
                 (unless (zerop (length text))
                   (if (eq type :IDENT)
                       (push (cons type (smalltalk-to-camel-case text)) tokens)
                       (push (cons type text) tokens)))))
             (scan-number (start)
               (loop while (and (not (eof?))
                                (or (digit-char-p (peek))
                                    (char= (peek) #\.)
                                    (char= (peek) #\e)
                                    (char= (peek) #\E)
                                    (char= (peek) #\+)
                                    (char= (peek) #\-)))
                     do (next-char))
               (flush-token start i :NUMBER))
             (scan-number-with-prefix (start prefix-end)
               "Scan number with prefix like 0x, 0b, 0o, 2r, 8r, 16r."
               (loop while (and (not (eof?))
                                (or (digit-char-p (peek))
                                    (find (peek) '(#\a #\b #\c #\d #\e #\f
                                                   #\A #\B #\C #\D #\E #\F))))
                     do (next-char))
               (flush-token start i :NUMBER))
             (scan-dword-literal (start)
               "Scan dword literal like 0d\"WORD\" or 0d'WORD'."
               (next-char) ; skip 0
               (next-char) ; skip d
               (let ((quote-char (peek)))
                 (when (member quote-char '(#\" #\apostrophe))
                   (next-char) ; skip quote
                   (dotimes (i 4)
                     (unless (eof?) (next-char))) ; consume 4 chars
                   (when (and (not (eof?)) (char= (peek) quote-char))
                     (next-char)))) ; skip closing quote
               (flush-token start i :NUMBER))
             (scan-string (start)
               (next-char) ; skip opening quote
               (loop while (and (not (eof?)) (char/= (peek) #\apostrophe))
                     do (next-char))
               (when (not (eof?)) (next-char)) ; skip closing quote
               (let ((text (coerce (subseq chars (1+ start) (1- i)) 'string)))
                 (push (cons :STRING text) tokens)))
             (scan-symbol (start)
               "Scan symbol literal (#name or #selector:)."
               (next-char) ; skip #
               (loop while (and (not (eof?))
                                (or (alphanumericp (peek))
                                    (char= (peek) #\_)
                                    (char= (peek) #\:)))
                     do (next-char))
               (let ((text (coerce (subseq chars start i) 'string)))
                 (push (cons :SYMBOL text) tokens)))
             (scan-identifier (start)
               (loop while (and (not (eof?))
                                (or (alphanumericp (peek))
                                    (char= (peek) #\_)))
                     do (next-char))
               (let ((text (coerce (subseq chars start i) 'string)))
                 (push (cons (smalltalk-token-type text) 
                             (if (eq (smalltalk-token-type text) :IDENT)
                                 (smalltalk-to-camel-case text)
                                 text))
                       tokens))))
      (scan-operator (start)
                     (let ((c1 (next-char))
                           (c2 (peek)))
                       (cond
                         ;; Two-character operators
                         ((and c2 (member (concatenate 'string (string c1) (string c2))
                                          '(":=" "==" "~=" "<=" ">=" "@" "#")
                                          :test #'string=))
                          (next-char)
                          (let ((op (concatenate 'string (string c1) (string c2))))
                            (push (cons (smalltalk-token-type op) op) tokens)))
                         ;; Single-character operators
                         (t
                          (let ((op (string c1)))
                            (push (cons (smalltalk-token-type op) op) tokens)))))))
    (loop until (eof?)
          do (skip-whitespace)
             (cond
               ((eof?) (return))
               ;; String literals
               ((char= (peek) #\apostrophe) (scan-string i))
               ;; Symbol literals
               ((and (char= (peek) #\#) (< (+ i 1) (length chars)))
                (scan-symbol i))
               ;; Dword format: 0d"..." or 0d'...'
               ((and (char= (peek) #\0) (peek-ahead 1) (char= (peek-ahead 1) #\d)
                     (peek-ahead 2) (member (peek-ahead 2) '(#\" #\apostrophe)))
                (scan-dword-literal i))
               ;; Numbers with prefixes: 0x, 0b, 0o, 16r, 2r, 8r
               ((and (char= (peek) #\0) (peek-ahead 1))
                (let ((next-c (peek-ahead 1)))
                  (cond
                    ((member next-c '(#\x #\X #\b #\B #\o #\O))
                     (next-char) (next-char) ; consume 0 and x/b/o
                     (scan-number-with-prefix i (+ i 2)))
                    (t
                     (scan-number i)))))
               ;; Numbers with old-style prefixes: 16r, 2r, 8r
               ((and (char= (peek) #\1) (peek-ahead 1) (char= (peek-ahead 1) #\6)
                     (peek-ahead 2) (char= (peek-ahead 2) #\r))
                (next-char) (next-char) (next-char) ; consume 1, 6, r
                (scan-number-with-prefix i (+ i 3)))
               ((and (char= (peek) #\2) (peek-ahead 1) (char= (peek-ahead 1) #\r))
                (next-char) (next-char) ; consume 2, r
                (scan-number-with-prefix i (+ i 2)))
               ((and (char= (peek) #\8) (peek-ahead 1) (char= (peek-ahead 1) #\r))
                (next-char) (next-char) ; consume 8, r
                (scan-number-with-prefix i (+ i 2)))
               ;; Regular numbers
               ((digit-char-p (peek)) (scan-number i))
               ;; Identifiers and keywords
               ((or (alpha-char-p (peek))
                    (char= (peek) #\_))
                (scan-identifier i))
               ;; Operators
               (t
                (scan-operator i))))
    (nreverse tokens)))

(defun smalltalk-lex-source (source)
  "Lex complete SmallTalk SOURCE string into token stream."
  (let ((all-tokens '()))
    (dolist (line (split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\" (char trimmed 0)))  ;; Comments start with "
          (setf all-tokens (nconc all-tokens (smalltalk-lex-line trimmed))))))
    all-tokens))

(defun smalltalk-lex (source)
  "Lex SmallTalk source string into token list for YACC."
  (let ((tokens (smalltalk-lex-source source)))
    (mapcar (lambda (tok)
              (list (first tok) (second tok)))
            tokens)))
