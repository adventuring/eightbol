;; src/frontend-scumm/scumm-lexer.lisp — Tokenization for SCUMM
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defparameter *scumm-keyword-alist*
  '(("if" . :IF)
    ("else" . :ELSE)
    ("endif" . :ENDIF)
    ("while" . :WHILE)
    ("endwhile" . :ENDWHILE)
    ("for" . :FOR)
    ("endfor" . :ENDFOR)
    ("break" . :BREAK)
    ("continue" . :CONTINUE)
    ("return" . :RETURN)
    ("define" . :DEFINE)
    ("set" . :SET)
    ("get" . :GET)
    ("put" . :PUT)
    ("call" . :CALL)
    ("exit" . :EXIT)
    ("goto" . :GOTO)
    ("label" . :LABEL)
    ("assert" . :ASSERT)
    ("wait" . :WAIT)
    ("stop" . :STOP)
    ("start" . :START)
    ("fade" . :FADE)
    ("restore" . :RESTORE)
    ("save" . :SAVE)
    ("restart" . :RESTART)
    ("version" . :VERSION)
    ("to" . :TO)
    ("end" . :END)
    ("dialogue" . :DIALOGUE)
    ("print" . :PRINT)
    ("input" . :INPUT))
  "Association list of SCUMM keywords to token symbols.")

(defparameter *scumm-operators-alist*
  '(("+" . :PLUS)
    ("-" . :MINUS)
    ("*" . :TIMES)
    ("/" . :DIVIDE)
    ("%" . :MOD)
    ("=" . :ASSIGN)
    ("==" . :EQUAL)
    ("!=" . :NE)
    ("<" . :LT)
    (">" . :GT)
    ("<=" . :LE)
    (">=" . :GE)
    ("&&" . :AND)
    ("||" . :OR)
    ("!" . :NOT))
  "Association list of SCUMM operators to token symbols.")

(defun normalize-identifier-to-camel-case (s)
  "Convert snake_case or mixed-case identifier S to camelCase.

   Examples:
   - 'my_variable' → 'myVariable'
   - 'MyVariable' → 'myVariable'
   - 'x' → 'x'"
  (let* ((lower-s (string-downcase s))
         (parts (split-sequence:split-sequence #\_ lower-s))
         (first-part (first parts))
         (rest-parts (rest parts)))
    (concatenate 'string
                 first-part
                 (apply #'concatenate 'string
                        (mapcar (lambda (part)
                                  (when (> (length part) 0)
                                    (concatenate 'string
                                                 (string-upcase (string (char part 0)))
                                                 (subseq part 1))))
                                rest-parts)))))

(defun parse-hex-literal (s)
  "Parse hexadecimal literal '0x' prefix (e.g., '0xFF' → 255)."
  (parse-integer (subseq s 2) :radix 16))

(defun parse-octal-literal (s)
  "Parse octal literal '0o' prefix (e.g., '0o77' → 63)."
  (parse-integer (subseq s 2) :radix 8))

(defun parse-binary-literal (s)
  "Parse binary literal '0b' prefix (e.g., '0b1010' → 10)."
  (parse-integer (subseq s 2) :radix 2))

(defun parse-dword-literal (s)
  "Parse DWORD literal '0d\"WORD\"' format (returns the 4-character string as-is).

   Example: 0d\"SCUM\" returns the string \"SCUM\"."
  (let ((start (position #\" s)))
    (if start
        (let ((end (position #\" s :start (1+ start))))
          (if end
              (subseq s (1+ start) end)
              s))
        s)))

(defun scumm-valid-identifier-p (s)
  "Check if S is a valid SCUMM identifier."
  (and (stringp s)
       (plusp (length s))
       (or (alpha-char-p (char s 0))
           (char= (char s 0) #\_))
       (every (lambda (c)
                (or (alphanumericp c) (char= c #\_)))
              s)))

(defun scumm-lex-line (line)
  "Split LINE into token list of (type value) pairs.

   Supports:
   - Identifiers with automatic camelCase normalization
   - Number literals: decimal, hex (0x), octal (0o), binary (0b), dword (0d\"WORD\")"
  (let ((tokens '())
        (chars (coerce line 'list))
        (i 0))
    (labels ((peek () (nth i chars))
             (peek-ahead (n) (nth (+ i n) chars))
             (next-char () (prog1 (nth i chars) (incf i)))
             (eof? () (>= i (length chars)))
             (skip-whitespace ()
               (loop while (and (not (eof?)) (find (peek) '(#\Space #\Tab #\Return #\Newline)))
                     do (next-char)))
             (flush-token (start end type)
               (let ((text (coerce (subseq chars start end) 'string)))
                 (unless (zerop (length text))
                   (push (cons type text) tokens))))
             (scan-number (start)
               ;; Check for special number formats
               (cond
                 ;; Hexadecimal: 0x...
                 ((and (char= (peek) #\0) (peek-ahead 1)
                       (char= (peek-ahead 1) #\x))
                  (next-char) (next-char)
                  (loop while (and (not (eof?)) (or (digit-char-p (peek))
                                                     (find (peek) '(#\a #\b #\c #\d #\e #\f
                                                                    #\A #\B #\C #\D #\E #\F))))
                        do (next-char))
                  (flush-token start i :hex-number))
                 ;; Octal: 0o...
                 ((and (char= (peek) #\0) (peek-ahead 1)
                       (char= (peek-ahead 1) #\o))
                  (next-char) (next-char)
                  (loop while (and (not (eof?)) (find (peek) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))
                        do (next-char))
                  (flush-token start i :octal-number))
                 ;; Binary: 0b...
                 ((and (char= (peek) #\0) (peek-ahead 1)
                       (char= (peek-ahead 1) #\b))
                  (next-char) (next-char)
                  (loop while (and (not (eof?)) (find (peek) '(#\0 #\1)))
                        do (next-char))
                  (flush-token start i :binary-number))
                 ;; DWORD: 0d"WORD"
                 ((and (char= (peek) #\0) (peek-ahead 1)
                       (char= (peek-ahead 1) #\d) (peek-ahead 2)
                       (char= (peek-ahead 2) #\"))
                  (next-char) (next-char) (next-char)
                  (loop while (and (not (eof?)) (char/= (peek) #\"))
                        do (next-char))
                  (when (not (eof?)) (next-char))
                  (flush-token start i :dword-number))
                 ;; Default: decimal
                 (t
                  (loop while (and (not (eof?)) (digit-char-p (peek)))
                        do (next-char))
                  (flush-token start i :number))))
             (scan-string (start)
               (next-char)
               (loop while (and (not (eof?)) (char/= (peek) #\"))
                     do (next-char))
               (when (not (eof?)) (next-char))
               (flush-token (1+ start) (1- i) :string))
             (scan-identifier (start)
               (loop while (and (not (eof?))
                                (or (alphanumericp (peek))
                                    (char= (peek) #\_)))
                     do (next-char))
               (let ((text (coerce (subseq chars start i) 'string)))
                 (push (cons (or (cdr (assoc text *scumm-keyword-alist* :test #'string-equal))
                                 :ident)
                             (normalize-identifier-to-camel-case text))
                       tokens))))
      (loop until (eof?)
            do (skip-whitespace)
               (cond
                 ((eof?) (return))
                 ((char= (peek) #\") (scan-string i))
                 ((digit-char-p (peek)) (scan-number i))
                 ((or (alpha-char-p (peek)) (char= (peek) #\_)) (scan-identifier i))
                 ((char= (peek) #\() (push (cons :lparen "(") tokens) (next-char))
                 ((char= (peek) #\)) (push (cons :rparen ")") tokens) (next-char))
                 ((char= (peek) #\[) (push (cons :lbracket "[") tokens) (next-char))
                 ((char= (peek) #\]) (push (cons :rbracket "]") tokens) (next-char))
                 ((char= (peek) #\;) (push (cons :semicolon ";") tokens) (next-char))
                 ((char= (peek) #\Comma) (push (cons :comma ",") tokens) (next-char))
                 (t (next-char))))
      (nreverse tokens))))

(defun scumm-lex-source (source)
  "Lex complete SCUMM source string into token stream."
  (let ((all-tokens '()))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\/ (char trimmed 0)))
          (setf all-tokens (nconc all-tokens (scumm-lex-line trimmed))))))
    all-tokens))

(defun scumm-lex-token ()
   "Read next token from *standard-input*."
   (declare (special *scumm-lex-token-buffer*))
   (cond
     ((and (boundp '*scumm-lex-token-buffer*) *scumm-lex-token-buffer*)
      (prog1 (first *scumm-lex-token-buffer*)
        (setf *scumm-lex-token-buffer* (rest *scumm-lex-token-buffer*))))
     (t
      (let ((line (read-line *standard-input* nil nil)))
        (when line
          (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
            (when (and (plusp (length trimmed)) (char/= #\; (char trimmed 0)))
              (setf *scumm-lex-token-buffer* (scumm-lex-line trimmed))
              (scumm-lex-token))))))))

(defun scumm-token-list ()
   "Return a thunk that reads tokens from *standard-input* using scumm-lex-token."
   (lambda ()
     (let ((token (scumm-lex-token)))
       (when token
         (cons token (funcall (scumm-token-list)))))))
