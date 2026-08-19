;; src/frontend-sci/sci-lexer.lisp — Tokenization for SCI (Sierra Creative Interpreter)
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defparameter *sci-keyword-alist*
  '(("if" . :IF)
    ("else" . :ELSE)
    ("then" . :THEN)
    ("while" . :WHILE)
    ("for" . :FOR)
    ("foreach" . :FOREACH)
    ("break" . :BREAK)
    ("continue" . :CONTINUE)
    ("return" . :RETURN)
    ("define" . :DEFINE)
    ("setq" . :SETQ)
    ("set" . :SET)
    ("get" . :GET)
    ("put" . :PUT)
    ("call" . :CALL)
    ("proc" . :PROC)
    ("method" . :METHOD)
    ("class" . :CLASS)
    ("instance" . :INSTANCE)
    ("send" . :SEND)
    ("super" . :SUPER)
    ("self" . :SELF)
    ("print" . :PRINT)
    ("dialogue" . :DIALOGUE)
    ("dialog" . :DIALOGUE)
    ("say" . :SAY)
    ("input" . :INPUT)
    ("ask" . :ASK)
    ("format" . :FORMAT)
    ("strcat" . :STRCAT)
    ("strlen" . :STRLEN)
    ("substr" . :SUBSTR)
    ("upcase" . :UPCASE)
    ("downcase" . :DOWNCASE)
    ("numtostr" . :NUMTOSTR)
    ("strtonum" . :STRTONUM))
  "Association list of SCI keywords to token symbols.")

(defparameter *sci-operators-alist*
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
    ("&" . :AND)
    ("|" . :OR)
    ("!" . :NOT))
  "Association list of SCI operators to token symbols.")

(defun sci-valid-identifier-p (s)
  "Check if S is a valid SCI identifier."
  (and (stringp s)
       (plusp (length s))
       (or (alpha-char-p (char s 0))
           (char= (char s 0) #\_))
       (every (lambda (c)
                (or (alphanumericp c) (char= c #\_)))
              s)))

(defun sci-normalize-identifier (s)
  "Normalize identifier S to PascalCase.
  
  Removes underscores and capitalizes first letter of each word:
  - 'hello_world' → 'HelloWorld'
  - 'myVar' → 'MyVar'
  - '_private' → 'Private'
  
  Returns a string in PascalCase format."
  (unless (or (null s) (zerop (length s)))
    (let* ((words (split-sequence:split-sequence #\_ s))
           (pascal-parts (mapcar (lambda (word)
                                   (when (plusp (length word))
                                     (concatenate 'string
                                                  (string-upcase (subseq word 0 1))
                                                  (string-downcase (subseq word 1)))))
                                 words)))
      (apply #'concatenate 'string (remove nil pascal-parts)))))

(defun sci-lex-line (line)
  "Split LINE into token list of (type value) pairs."
  (let ((tokens '())
        (chars (coerce line 'list))
        (i 0))
    (labels ((peek () (nth i chars))
             (next-char () (prog1 (nth i chars) (incf i)))
             (end-of-file-p () (>= i (length chars)))
             (skip-whitespace ()
               (loop while (and (not (end-of-file-p)) (find (peek) '(#\Space #\Tab #\Return #\Newline)))
                     do (next-char)))
             (flush-token (start end type)
               (let ((text (coerce (subseq chars start end) 'string)))
                 (unless (zerop (length text))
                   (push (cons type text) tokens))))
             (scan-number (start)
               ;; Check for 0x (hex), 0o (octal), 0b (binary) prefixes
               (let ((c (peek)))
                 (if (and (char= c #\0) (plusp (- (length chars) (1+ i))))
                     (let ((next-c (elt chars (1+ i))))
                       (cond
                         ((char-equal next-c #\x)
                          ;; Handle 0xHEXDIGITS
                          (next-char) (next-char) ;; Skip 0x
                          (let ((hex-start i))
                            (loop while (and (not (end-of-file-p))
                                             (or (digit-char-p (peek))
                                                 (find (char-upcase (peek)) "ABCDEF")))
                                  do (next-char))
                            (flush-token hex-start i :hex)))
                         ((char-equal next-c #\o)
                          ;; Handle 0oOCTALDIGITS
                          (next-char) (next-char) ;; Skip 0o
                          (let ((oct-start i))
                            (loop while (and (not (end-of-file-p)) (find (peek) "01234567"))
                                  do (next-char))
                            (flush-token oct-start i :octal)))
                         ((char-equal next-c #\b)
                          ;; Handle 0bBINARYDIGITS
                          (next-char) (next-char) ;; Skip 0b
                          (let ((bin-start i))
                            (loop while (and (not (end-of-file-p)) (find (peek) "01"))
                                  do (next-char))
                            (flush-token bin-start i :binary)))
                         (t
                          ;; Regular decimal number
                          (loop while (and (not (end-of-file-p)) (digit-char-p (peek)))
                                do (next-char))
                          (flush-token start i :number))))
                     ;; Regular decimal number
                     (loop while (and (not (end-of-file-p)) (digit-char-p (peek)))
                           do (next-char)))
                 (flush-token start i :number)))
             (scan-prefixed-number (start)
               (declare (ignore start))
               ;; Handle #x (hex), #o (octal), #b (binary), #d"..." (dword)
               (next-char) ;; Skip the '#'
               (let ((format-char (when (not (end-of-file-p)) (peek))))
                 (cond
                   ((char-equal format-char #\x)
                    (next-char) ;; Skip 'x'
                    (let ((hex-start i))
                      (loop while (and (not (end-of-file-p))
                                       (or (digit-char-p (peek))
                                           (find (char-upcase (peek)) "ABCDEF")))
                            do (next-char))
                      (flush-token hex-start i :hex)))
                   ((char-equal format-char #\o)
                    (next-char) ;; Skip 'o'
                    (let ((oct-start i))
                      (loop while (and (not (end-of-file-p)) (find (peek) "01234567"))
                            do (next-char))
                      (flush-token oct-start i :octal)))
                   ((char-equal format-char #\b)
                    (next-char) ;; Skip 'b'
                    (let ((bin-start i))
                      (loop while (and (not (end-of-file-p)) (find (peek) "01"))
                            do (next-char))
                      (flush-token bin-start i :binary)))
                   ((char-equal format-char #\d)
                    (next-char) ;; Skip 'd'
                    (when (and (not (end-of-file-p)) (find (peek) "'\""))
                      (let ((quote-char (peek)))
                        (next-char) ;; Skip opening quote
                        (let ((dword-start i))
                          (loop while (and (not (end-of-file-p)) (char/= (peek) quote-char))
                                do (next-char))
                          (let ((dword-text (coerce (subseq chars dword-start i) 'string)))
                            (when (not (end-of-file-p)) (next-char)) ;; Skip closing quote
                            (push (cons :dword dword-text) tokens)))))))))
             (scan-string (start)
               (next-char)
               (loop while (and (not (end-of-file-p)) (char/= (peek) #\"))
                     do (next-char))
               (when (not (end-of-file-p)) (next-char))
               (flush-token (1+ start) (1- i) :string))
             (scan-identifier (start)
               (loop while (and (not (end-of-file-p))
                                (or (alphanumericp (peek))
                                    (char= (peek) #\_)))
                     do (next-char))
               (let* ((text (coerce (subseq chars start i) 'string))
                      ;; Normalize identifier to PascalCase
                      (normalized (sci-normalize-identifier text)))
                 (push (cons (or (cdr (assoc text *sci-keyword-alist* :test #'string-equal))
                                 :ident)
                             normalized)
                       tokens))))
      (loop until (end-of-file-p)
            do (skip-whitespace)
               (cond
                 ((end-of-file-p) (return))
                 ((char= (peek) #\") (scan-string i))
                 ((char= (peek) #\#) (scan-prefixed-number i))
                 ((and (char= (peek) #\d) 
                       (plusp (- (length chars) (1+ i)))
                       (find (elt chars (1+ i)) "'\""))
                  ;; Handle d'WORD' or d"WORD" dword literal
                  (next-char) ;; Skip 'd'
                  (let ((quote-char (peek)))
                    (next-char) ;; Skip quote
                    (let ((dword-start i))
                      (loop while (and (not (end-of-file-p)) (char/= (peek) quote-char))
                            do (next-char))
                      (let ((dword-text (coerce (subseq chars dword-start i) 'string)))
                        (when (not (end-of-file-p)) (next-char)) ;; Skip closing quote
                        (push (cons :dword dword-text) tokens)))))
                 ((digit-char-p (peek)) (scan-number i))
                 ((or (alpha-char-p (peek)) (char= (peek) #\_)) (scan-identifier i))
                 ((char= (peek) #\() (push (cons :lparen "(") tokens) (next-char))
                 ((char= (peek) #\)) (push (cons :rparen ")") tokens) (next-char))
                 ((char= (peek) #\apostrophe) (push (cons :quote "'") tokens) (next-char))
                 ((char= (peek) #\comma) (push (cons :comma ",") tokens) (next-char))
                 (t (next-char))))
      (nreverse tokens))))

(defun sci-lex-source (source)
  "Lex complete SCI source string into token stream."
  (let ((all-tokens '()))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\; (char trimmed 0)))
          (setf all-tokens (nconc all-tokens (sci-lex-line trimmed))))))
    all-tokens))
