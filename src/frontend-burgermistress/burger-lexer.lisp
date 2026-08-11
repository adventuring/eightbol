;; src/frontend-burgermistress/burger-lexer.lisp — Tokenization for Burgermistress scripting
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defparameter *burgermistress-keyword-alist*
  '(("IF" . :IF)
    ("ELSE" . :ELSE)
    ("THEN" . :THEN)
    ("ENDIF" . :ENDIF)
    ("WHILE" . :WHILE)
    ("WEND" . :WEND)
    ("FOR" . :FOR)
    ("TO" . :TO)
    ("STEP" . :STEP)
    ("NEXT" . :NEXT)
    ("DO" . :DO)
    ("LOOP" . :LOOP)
    ("UNTIL" . :UNTIL)
    ("EXIT" . :EXIT)
    ("SUB" . :SUB)
    ("END" . :END)
    ("FUNCTION" . :FUNCTION)
    ("CALL" . :CALL)
    ("RETURN" . :RETURN)
    ("GOSUB" . :GOSUB)
    ("AND" . :AND)
    ("OR" . :OR)
    ("NOT" . :NOT)
    ("DIM" . :DIM)
    ("AS" . :AS)
    ("INTEGER" . :INTEGER)
    ("STRING" . :STRING)
    ("BOOLEAN" . :BOOLEAN)
    ("DOUBLE" . :DOUBLE)
    ("DIALOGUE" . :DIALOGUE)
    ("PRINT" . :PRINT)
    ("INPUT" . :INPUT))
  "Association list of Burgermistress keywords to token symbols.")

(defparameter *burgermistress-operators-alist*
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
  "Association list of Burgermistress operators to token symbols.")

;;; Identifier normalization to Header-Case

(defun normalize-identifier-to-header-case (s)
  "Normalize identifier S to Header-Case format (MyIdentifier or My.Nested.Field).
Handles case-insensitive input with dots for structure.
Example: myIdentifier → MyIdentifier, MY.FIELD → My.Field"
  (when (and (stringp s) (plusp (length s)))
    (let ((parts (split-sequence:split-sequence #\. s)))
      (format nil "~{~:(~A~)~^.~}" parts))))

(defun burgermistress-valid-identifier-p (s)
  "Check if S is a valid Burgermistress identifier (Header-Case with optional dots for structure)."
  (and (stringp s)
       (plusp (length s))
       (or (alpha-char-p (char s 0))
           (char= (char s 0) #\.))
       (every (lambda (c)
                (or (alphanumericp c) (char= c #\_) (char= c #\.)))
              s)))

;;; Number literal parsing: decimal, hex (&h/0x), octal (&o/0o), binary (&b/0b), dword (&d"WORD"/0d"WORD")

(defun parse-number-literal (text)
  "Parse TEXT as a number literal; return (type . value) or nil on error.
Supports: decimal, hex (&h or 0x), octal (&o or 0o), binary (&b or 0b), 
dword (&d\"WORD\" or 0d\"WORD\")."
  (cond
    ;; Dword literal: &d"WORD" or 0d"WORD"
    ((and (>= (length text) 4)
          (or (string-equal (subseq text 0 2) "&d")
              (string-equal (subseq text 0 2) "0d"))
          (char= (char text 2) #\"))
     (let ((end-quote (position #\" text :start 3)))
       (when end-quote
         (cons :dword (subseq text 3 end-quote)))))
    
    ;; Hex literal: &h prefix or 0x prefix
    ((and (>= (length text) 3)
          (or (string-equal (subseq text 0 2) "&h")
              (string-equal (subseq text 0 2) "0x")))
     (let ((hex-part (subseq text 2)))
       (when (every (lambda (c) (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                             #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F)))
                    hex-part)
         (cons :hex (parse-integer hex-part :radix 16)))))
    
    ;; Octal literal: &o prefix or 0o prefix
    ((and (>= (length text) 3)
          (or (string-equal (subseq text 0 2) "&o")
              (string-equal (subseq text 0 2) "0o")))
     (let ((octal-part (subseq text 2)))
       (when (every (lambda (c) (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))
                    octal-part)
         (cons :octal (parse-integer octal-part :radix 8)))))
    
    ;; Binary literal: &b prefix or 0b prefix
    ((and (>= (length text) 3)
          (or (string-equal (subseq text 0 2) "&b")
              (string-equal (subseq text 0 2) "0b")))
     (let ((binary-part (subseq text 2)))
       (when (every (lambda (c) (member c '(#\0 #\1)))
                    binary-part)
         (cons :binary (parse-integer binary-part :radix 2)))))
    
    ;; Decimal literal (default)
    ((every #'digit-char-p text)
     (cons :decimal (parse-integer text)))
    
    (t nil)))

(defun burgermistress-lex-line (line)
  "Split LINE into token list of (type value) pairs."
  (let ((tokens '())
        (chars (coerce line 'list))
        (i 0))
    (labels ((peek () (nth i chars))
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
               ;; Try to parse as extended number literal
               (let* ((end (loop for j from start below (length chars)
                                 while (or (digit-char-p (nth j chars))
                                          (find (nth j chars) '(#\. #\x #\X #\h #\H #\o #\O #\b #\B #\d #\D #\&)))
                                 finally (return j)))
                      (text (coerce (subseq chars start end) 'string))
                      (parsed (parse-number-literal text)))
                 (setf i end)
                 (if parsed
                     (push (cons :number (format nil "~A:~A" (car parsed) (cdr parsed))) tokens)
                     (push (cons :number text) tokens))))
             (scan-string (start)
               (next-char) ; skip opening quote
               (loop while (and (not (eof?)) (char/= (peek) #\"))
                     do (next-char))
               (when (not (eof?)) (next-char)) ; skip closing quote
               (flush-token (1+ start) (1- i) :string))
             (scan-identifier (start)
               (loop while (and (not (eof?))
                                (or (alphanumericp (peek))
                                    (char= (peek) #\_)
                                    (char= (peek) #\.)))
                     do (next-char))
               (let* ((text (coerce (subseq chars start i) 'string))
                      (normalized (normalize-identifier-to-header-case text))
                      (keyword-sym (cdr (assoc text *burgermistress-keyword-alist* :test #'string-equal))))
                 (push (cons (or keyword-sym :ident) normalized)
                       tokens))))
      (loop until (eof?)
            do (skip-whitespace)
               (cond
                 ((eof?) (return))
                 ((char= (peek) #\") (scan-string i))
                 ((or (digit-char-p (peek))
                      (and (char= (peek) #\&) (< (1+ i) (length chars)))) 
                  (scan-number i))
                 ((or (alphanumericp (peek)) (char= (peek) #\_) (char= (peek) #\.)) 
                  (scan-identifier i))
                 (t
                  (let ((op1 (string (next-char)))
                        (op2 (when (not (eof?)) (string (peek)))))
                    (cond
                      ((and op2 (member (concatenate 'string op1 op2) '("<<" ">>" "<=" ">=" "<>")
                                        :test #'string-equal))
                       (next-char)
                       (flush-token (1- i) i :op))
                      (t (flush-token (1- i) i :op)))))))
      (nreverse tokens))))

(defun burgermistress-lex-source (source)
  "Lex complete Burgermistress source string into token stream."
  (let ((all-tokens '()))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\apostrophe (char trimmed 0)))
          (setf all-tokens (nconc all-tokens (burgermistress-lex-line trimmed))))))
    all-tokens))
