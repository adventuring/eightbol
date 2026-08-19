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
     ("INPUT" . :INPUT)
     ("PRINT" . :PRINT)
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

(defun lingo-normalize-identifier (s)
  "Normalize identifier S to Header-Case (Title-Case-With-Hyphens).
   Examples: myVariable -> My-Variable, PlayerHealth -> Player-Health."
  (when (stringp s)
    (let ((chars (coerce s 'list))
          (result '())
          (capitalize-next t))
      (dolist (c chars)
        (cond
          ;; Preserve underscores but capitalize next letter
          ((char= c #\_)
           (push #\- result)
           (setf capitalize-next t))
          ;; On transition from lowercase to uppercase (camelCase boundary)
          ((and (not capitalize-next)
                (lower-case-p c)
                (peek-ahead-uppercase chars result))
           (push #\- result)
           (push (char-upcase c) result)
           (setf capitalize-next nil))
          ;; Skip dollar signs (keep as identifier part)
          ((char= c #\$)
           (push c result))
          ;; Capitalize if needed, otherwise preserve case and add hyphens
          (capitalize-next
           (push (char-upcase c) result)
           (setf capitalize-next nil))
          ;; Regular lowercase letter
          (t (push c result))))
      (coerce (nreverse result) 'string))))

(defun peek-ahead-uppercase (chars visited-chars)
  "Check if next character in stream is uppercase. Simplified heuristic."
  (declare (ignore chars visited-chars))
  nil) ;; Simplified for now; full implementation would check ahead

(defun lingo-valid-identifier-p (s)
  "Check if S is a valid Lingo identifier (PascalCase or camelCase)."
  (and (stringp s)
       (plusp (length s))
       (every (lambda (c)
                (or (alphanumericp c)
                    (char= c #\$)
                    (char= c #\_)))
              s)))

(defun lingo-parse-number (s)
  "Parse number literal S and return (values integer-value base-indicator).
   Supports: decimal, 0x/$ hex, 0o/$o octal, 0b/$b binary, 0d/$d dword.
   Returns nil if not a valid number."
  (when (stringp s)
    (let ((len (length s)))
      (cond
        ;; Hex: 0x... or 0X... or $...
        ((and (>= len 2) (string-equal (subseq s 0 2) "0x"))
         (let ((hex-str (subseq s 2)))
           (ignore-errors (values (parse-integer hex-str :radix 16) :hex))))
        ((and (>= len 2) (string-equal (subseq s 0 2) "0X"))
         (let ((hex-str (subseq s 2)))
           (ignore-errors (values (parse-integer hex-str :radix 16) :hex))))
        ((char= (char s 0) #\$)
         (let ((rest (subseq s 1)))
           (cond
             ;; $h... hex
             ((and (>= len 2) (char-equal (char s 1) #\h))
              (let ((hex-str (subseq s 2)))
                (ignore-errors (values (parse-integer hex-str :radix 16) :hex))))
             ;; $o... octal
             ((and (>= len 2) (char-equal (char s 1) #\o))
              (let ((oct-str (subseq s 2)))
                (ignore-errors (values (parse-integer oct-str :radix 8) :octal))))
             ;; $b... binary
             ((and (>= len 2) (char-equal (char s 1) #\b))
              (let ((bin-str (subseq s 2)))
                (ignore-errors (values (parse-integer bin-str :radix 2) :binary))))
             ;; $d... dword
             ((and (>= len 2) (char-equal (char s 1) #\d))
              (let ((dw-str (subseq s 2)))
                (ignore-errors (values (parse-integer dw-str :radix 10) :dword))))
             ;; Plain $ prefix is hex
             (t
              (ignore-errors (values (parse-integer rest :radix 16) :hex))))))
        ;; Octal: 0o... or 0O...
        ((and (>= len 2) (string-equal (subseq s 0 2) "0o"))
         (let ((oct-str (subseq s 2)))
           (ignore-errors (values (parse-integer oct-str :radix 8) :octal))))
        ((and (>= len 2) (string-equal (subseq s 0 2) "0O"))
         (let ((oct-str (subseq s 2)))
           (ignore-errors (values (parse-integer oct-str :radix 8) :octal))))
        ;; Binary: 0b... or 0B... or %...
        ((and (>= len 2) (string-equal (subseq s 0 2) "0b"))
         (let ((bin-str (subseq s 2)))
           (ignore-errors (values (parse-integer bin-str :radix 2) :binary))))
        ((and (>= len 2) (string-equal (subseq s 0 2) "0B"))
         (let ((bin-str (subseq s 2)))
           (ignore-errors (values (parse-integer bin-str :radix 2) :binary))))
        ((char= (char s 0) #\%)
         (let ((bin-str (subseq s 1)))
           (ignore-errors (values (parse-integer bin-str :radix 2) :binary))))
        ;; Dword: 0d...
        ((and (>= len 2) (string-equal (subseq s 0 2) "0d"))
         (let ((dw-str (subseq s 2)))
           (ignore-errors (values (parse-integer dw-str :radix 10) :dword))))
        ((and (>= len 2) (string-equal (subseq s 0 2) "0D"))
         (let ((dw-str (subseq s 2)))
           (ignore-errors (values (parse-integer dw-str :radix 10) :dword))))
        ;; Decimal (default)
        ((every (lambda (c) (or (digit-char-p c) (char= c #\.))) s)
         (ignore-errors (values (parse-integer s :radix 10) :decimal)))
        (t nil)))))

(defun lingo-token-type (lexeme)
  "Return token type for LEXEME, a string."
  (cond
    ;; Try to parse as number
    ((and (stringp lexeme) (lingo-parse-number lexeme))
     :NUMBER)
    ;; String literals
    ((and (stringp lexeme) (> (length lexeme) 0) (char= (char lexeme 0) #\"))
     :STRING)
    ;; Keywords (case-insensitive)
    ((assoc lexeme *lingo-keyword-alist* :test #'string-equal)
     (cdr (assoc lexeme *lingo-keyword-alist* :test #'string-equal)))
    ;; Operators
    ((assoc lexeme *lingo-operators-alist* :test #'string-equal)
     (cdr (assoc lexeme *lingo-operators-alist* :test #'string-equal)))
    ;; Identifiers (alpha start, alphanumeric + _ + $)
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
                                    (member (peek) '(#\. #\x #\X #\o #\O #\b #\B #\d #\D #\h #\H))
                                    (char= (peek) #\$)))
                     do (next-char))
               (flush-token start i :NUMBER))
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
               (let* ((text (coerce (subseq chars start i) 'string))
                      (token-type (lingo-token-type text))
                      (normalized (if (eq token-type :IDENT)
                                      (lingo-normalize-identifier text)
                                      text)))
                 (push (cons token-type normalized) tokens))))
      (loop until (eof?)
            do (progn
                 (skip-whitespace)
                 (cond
                   ((eof?) (return))
                   ((char= (peek) #\") (scan-string i))
                   ;; Number prefixes: $, %, 0
                   ((member (peek) '(#\$ #\%))
                    (let ((start-pos i))
                      (next-char)
                      ;; Peek ahead for hex/oct/bin/dword markers
                      (if (and (not (eof?)) (member (peek) '(#\h #\H #\o #\O #\b #\B #\d #\D)))
                          ;; It's a prefixed number
                          (progn
                            (next-char)
                            (loop while (and (not (eof?))
                                             (or (digit-char-p (peek))
                                                 (member (peek) '(#\a #\b #\c #\d #\e #\f
                                                                  #\A #\B #\C #\D #\E #\F))))
                                  do (next-char))
                            (flush-token start-pos i :NUMBER))
                          ;; No marker, could be $ prefix hex or plain identifier
                          (if (or (digit-char-p (peek))
                                  (member (peek) '(#\a #\b #\c #\d #\e #\f
                                                   #\A #\B #\C #\D #\E #\F)))
                              ;; Dollar hex number
                              (progn
                                (loop while (and (not (eof?))
                                                 (or (digit-char-p (peek))
                                                     (member (peek) '(#\a #\b #\c #\d #\e #\f
                                                                      #\A #\B #\C #\D #\E #\F))))
                                      do (next-char))
                                (flush-token start-pos i :NUMBER))
                              ;; Not a number, could be identifier starting with $
                              (progn
                                (decf i)
                                (scan-identifier start-pos))))))
                   ((and (char= (peek) #\0)
                         (< (1+ i) (length chars))
                         (member (nth (1+ i) chars) '(#\x #\X #\o #\O #\b #\B #\d #\D)))
                    ;; Prefixed number like 0x, 0o, 0b, 0d
                    (let ((start-pos i))
                      (next-char)
                      (next-char)           ; consume x/o/b/d
                      (loop while (and (not (eof?))
                                       (or (digit-char-p (peek))
                                           (member (peek) '(#\a #\b #\c #\d #\e #\f
                                                            #\A #\B #\C #\D #\E #\F))))
                            do (next-char))
                      (flush-token start-pos i :NUMBER)))
                   ((digit-char-p (peek)) (scan-number i))
                   ((or (alphanumericp (peek))
                        (char= (peek) #\_)
                        (char= (peek) #\-)) (scan-identifier i))
                   (t
                    (let* ((op1 (string (next-char)))
                           (op2 (when (not (eof?)) (string (peek)))))
                      (cond
                        ((and op2 (string= (concatenate 'string op1 op2) "="))
                         (next-char)
                         (let ((op-tok (concatenate 'string op1 op2)))
                           (push (cons (lingo-token-type op-tok) op-tok) tokens)))
                        ((and op2 (string= (concatenate 'string op1 op2) ":="))
                         (next-char)
                         (push (cons :COLONEQ ":=") tokens))
                        ((string= op1 ":")
                         (push (cons :COLON ":") tokens))
                        (t
                         (let ((typ (lingo-token-type op1)))
                           (push (cons typ op1) tokens)))))))))
      (nreverse tokens))))

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
