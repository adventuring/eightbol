;; src/frontend-zil/zil-lexer.lisp — Tokenization for ZIL
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defparameter *zil-keyword-alist*
  '(("<IF>" . :IF)
    ("<ELSE>" . :ELSE)
    ("<THEN>" . :THEN)
    ("<WHILE>" . :WHILE)
    ("<FOR>" . :FOR)
    ("<BREAK>" . :BREAK)
    ("<CONTINUE>" . :CONTINUE)
    ("<RETURN>" . :RETURN)
    ("<DEFINE>" . :DEFINE)
    ("<SET>" . :SET)
    ("<SETG>" . :SETG)
    ("<GET>" . :GET)
    ("<PUT>" . :PUT)
    ("<CALL>" . :CALL)
    ("<INVOKE>" . :INVOKE)
    ("<GO>" . :GO)
    ("<EXIT>" . :EXIT)
    ("<LABEL>" . :LABEL)
    ("<ASSERT>" . :ASSERT)
    ("<WAIT>" . :WAIT)
    ("<STOP>" . :STOP)
    ("<START>" . :START)
    ("<FADE>" . :FADE)
    ("<RESTORE>" . :RESTORE)
    ("<SAVE>" . :SAVE)
    ("<RESTART>" . :RESTART)
    ("<VERSION>" . :VERSION)
    ("<OBJECT>" . :OBJECT)
    ("<PROPERTY>" . :PROPERTY)
    ("<GLOBAL>" . :GLOBAL)
    ("<LOCAL>" . :LOCAL)
    ("<P>" . :PROPACCESS)
    ("<PRINT>" . :PRINT)
    ("<INPUT>" . :INPUT)
    ("<DIALOGUE>" . :DIALOGUE))
  "Association list of ZIL keywords to token symbols.")

(defparameter *zil-operators-alist*
   '(("+" . :PLUS)
     ("-" . :MINUS)
     ("*" . :TIMES)
     ("/" . :DIVIDE)
     ("=" . :EQUAL)
     ("<>" . :NE)
     ("<" . :LT)
     (">" . :GT)
     ("<=" . :LE)
     (">=" . :GE)
     ("AND" . :AND)
     ("OR" . :OR)
     ("NOT" . :NOT))
   "Association list of ZIL operators to token symbols.")

(defun zil-normalize-identifier (s)
  "Normalize identifier S to uppercase (ZIL is case-insensitive).
Returns normalized identifier as a string suitable for comparison."
  (string-upcase s))

(defun zil-valid-identifier-p (s)
  "Check if S is a valid ZIL identifier (case-insensitive with hyphens)."
  (and (stringp s)
       (plusp (length s))
       (or (alpha-char-p (char s 0))
           (char= (char s 0) #\-))
       (every (lambda (c)
                (or (alphanumericp c) (char= c #\-) (char= c #\_) (char= c #\?)))
              s)))

(defun zil-parse-number-literal (text)
  "Parse a number literal TEXT in decimal, hex (#x), octal (#o), binary (#b), or dword (#d) format.
Returns (values number-value token-type) or signals error on invalid format."
  (cond
    ;; Dword format: d'WORD' - store as dword token with the word
    ((cl-ppcre:scan "^d'[^']*'$" text)
     (let* ((word (cl-ppcre:register-groups-bind (w) ("^d'([^']*)'$" text) w)))
       (values word :dword)))
    ;; Hexadecimal: #x<hex> or 0x<hex>
    ((cl-ppcre:scan "^(?:#x|0x)[0-9a-fA-F]+$" text)
     (let* ((hex-str (cl-ppcre:register-groups-bind (h) ("^(?:#x|0x)([0-9a-fA-F]+)$" text) h)))
       (values (parse-integer hex-str :radix 16) :number)))
    ;; Octal: #o<octal> or 0o<octal>
    ((cl-ppcre:scan "^(?:#o|0o)[0-7]+$" text)
     (let* ((oct-str (cl-ppcre:register-groups-bind (o) ("^(?:#o|0o)([0-7]+)$" text) o)))
       (values (parse-integer oct-str :radix 8) :number)))
    ;; Binary: #b<binary> or 0b<binary>
    ((cl-ppcre:scan "^(?:#b|0b)[01]+$" text)
     (let* ((bin-str (cl-ppcre:register-groups-bind (b) ("^(?:#b|0b)([01]+)$" text) b)))
       (values (parse-integer bin-str :radix 2) :number)))
    ;; Dword decimal: #d<decimal>
    ((cl-ppcre:scan "^#d-?\\d+$" text)
     (let* ((num-str (cl-ppcre:register-groups-bind (n) ("^#d(-?\\d+)$" text) n)))
       (values (parse-integer num-str) :number)))
    ;; Decimal (default)
    ((cl-ppcre:scan "^-?\\d+$" text)
     (values (parse-integer text) :number))
    (t (error "Invalid number literal: ~A" text))))

(defun zil-number-prefix-p (chars i)
  "Check if a number literal starts at position I in CHARS list.
Handles all formats: decimal, #x, 0x, #o, 0o, #b, 0b, #d, d'..."
  (and (< i (length chars))
       (let ((c (nth i chars)))
         (or (digit-char-p c)
             (and (< (+ i 1) (length chars))
                  (let ((next (nth (+ i 1) chars)))
                    (or (and (char= c #\#) 
                             (member next '(#\x #\o #\b #\d)))
                        (and (char= c #\0)
                             (member next '(#\x #\o #\b)))
                        (and (char= c #\d)
                             (char= next #\apostrophe)))))))))

(defun zil-lex-line (line)
  "Split LINE into token list of (type value) pairs.
Supports decimal/hex/octal/binary/dword number literals, strings, and atoms."
  (let ((tokens '())
        (chars (coerce line 'list))
        (i 0))
    (labels ((peek () (nth i chars))
             (next-char () (prog1 (nth i chars) (incf i)))
             (eof? () (>= i (length chars)))
             (peek-ahead (n) (nth (+ i n) chars))
             (flush-token (start end type)
               (let ((text (coerce (subseq chars start end) 'string)))
                 (unless (zerop (length text))
                   (push (cons type text) tokens))))
             (skip-whitespace ()
               (loop while (and (not (eof?)) (find (peek) '(#\Space #\Tab #\Return #\Newline)))
                     do (next-char)))
             (scan-number-literal (start)
               ;; Scan full number literal including prefix chars
               (cond
                 ;; Dword format: d'...'
                 ((and (char= (peek) #\d) (< (+ i 1) (length chars)) (char= (peek-ahead 1) #\apostrophe))
                  (next-char) ; skip 'd'
                  (next-char) ; skip quote
                  (loop while (and (not (eof?)) (char/= (peek) #\apostrophe))
                        do (next-char))
                  (when (not (eof?)) (next-char))) ; skip closing quote
                 ;; Hex, octal, binary: #x, #o, #b, 0x, 0o, 0b
                 ((and (or (char= (peek) #\#) (char= (peek) #\0))
                       (< (+ i 1) (length chars)))
                  (next-char) ; skip # or 0
                  (next-char) ; skip x/o/b
                  (loop while (and (not (eof?)) 
                                   (or (digit-char-p (peek))
                                       (and (member (peek) '(#\a #\b #\c #\d #\e #\f))
                                            (not (char= (nth (+ i 0) chars) #\o))))) ; not octal
                        do (next-char)))
                 ;; Regular decimal
                 (t
                  (loop while (and (not (eof?)) (digit-char-p (peek)))
                        do (progn (next-char)
                                  (let ((text (coerce (subseq chars start i) 'string)))
                                    (handler-case
                                        (multiple-value-bind (value type) (zil-parse-number-literal text)
                                          (push (cons type (format nil "~A" value)) tokens))
                                      (error (c)
                                        (warn "Failed to parse number literal ~A: ~A" text c)
                                        (flush-token start i :number)))))))))
             (scan-string (start)
               (next-char) ; skip opening quote
               (loop while (and (not (eof?)) (char/= (peek) #\"))
                     do (next-char))
               (when (not (eof?)) (next-char)) ; skip closing quote
               (flush-token (1+ start) (1- i) :string))
             (scan-atom (start)
               (loop while (and (not (eof?))
                                (or (alphanumericp (peek))
                                    (char= (peek) #\-)
                                    (char= (peek) #\_)
                                    (char= (peek) #\?)))
                     do (progn (next-char)
                               (let* ((text (coerce (subseq chars start i) 'string))
                                      (normalized (zil-normalize-identifier text)))
                                 (push (cons (or (cdr (assoc normalized *zil-keyword-alist* :test #'string-equal))
                                                 :atom)
                                             normalized)
                                       tokens)
                                 (loop until (eof?)
                                       do (skip-whitespace)
                                          (cond
                                            ((eof?) (return))
                                            ((char= (peek) #\") (scan-string i))
                                            ((zil-number-prefix-p chars i) (scan-number-literal i))
                                            ((or (alpha-char-p (peek))
                                                 (char= (peek) #\-)
                                                 (char= (peek) #\_)
                                                 (char= (peek) #\?)) (scan-atom i))
                                            ((char= (peek) #\<)
                                             (let ((start i))
                                               (next-char)
                                               (loop while (and (not (eof?)) (char/= (peek) #\>))
                                                     do (next-char))
                                               (when (not (eof?)) (next-char))
                                               (let* ((text (coerce (subseq chars start i) 'string))
                                                      (normalized (zil-normalize-identifier text)))
                                                 (push (cons (or (cdr (assoc normalized *zil-keyword-alist* :test #'string-equal))
                                                                 :keyword)
                                                             normalized)
                                                       tokens))))
                                            ((char= (peek) #\() (push (cons :lparen "(") tokens) (next-char))
                                            ((char= (peek) #\)) (push (cons :rparen ")") tokens) (next-char))
                                            ((char= (peek) #\apostrophe) (push (cons :quote "'") tokens) (next-char))
                                            ((char= (peek) #\comma) (push (cons :comma ",") tokens) (next-char))
                                            (t (next-char))))))))))
    (nreverse tokens)))

(defun zil-lex-source (source)
  "Lex complete ZIL source string into token stream."
  (let ((all-tokens '()))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (zerop (length trimmed))
          (setf all-tokens (nconc all-tokens (zil-lex-line trimmed))))))
    all-tokens))
