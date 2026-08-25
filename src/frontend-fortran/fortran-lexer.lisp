;; src/fortran-lexer.lisp — Tokenization for FORTRAN source
;; Following the pattern of basic-lexer.lisp
(in-package :eightbol)

(defparameter *fortran-keyword-alist*
  '(("PROGRAM" . :PROGRAM)
    ("SUBROUTINE" . :SUBROUTINE)
    ("FUNCTION" . :FUNCTION)
    ("END" . :END)
    ("RETURN" . :RETURN)
    ("STOP" . :STOP)
    ("CALL" . :CALL)
    ("GOTO" . :GOTO)
    ("IF" . :IF)
    ("THEN" . :THEN)
    ("ELSE" . :ELSE)
    ("DO" . :DO)
    ("READ" . :READ)
    ("WRITE" . :WRITE)
    ("PRINT" . :PRINT)
    ("OPEN" . :OPEN)
    ("CLOSE" . :CLOSE)
    ("REAL" . :REAL-TYPE)
    ("INTEGER" . :INTEGER-TYPE)
    ("LOGICAL" . :LOGICAL-TYPE)
    ("CHARACTER" . :CHARACTER-TYPE)
    ("COMPLEX" . :COMPLEX-TYPE)
    ("TYPE" . :TYPE)
    ("CLASS" . :CLASS)
    ("METHOD" . :METHOD)
    ("INSTANCE" . :INSTANCE)
    ("NEW" . :NEW)
    ("DELETE" . :DELETE)
    ("INVOKE" . :INVOKE)
    ("SUPER" . :SUPER)
    ("SELF" . :SELF)
    ("PROCEDURE" . :PROCEDURE)
    ("DATA" . :DATA)
    ("PARAMETER" . :PARAMETER)
    ("DIMENSION" . :DIMENSION)
    ("COMMON" . :COMMON)
    ("EQUIVALENCE" . :EQUIVALENCE)
    ("INCLUDE" . :INCLUDE)
    ("WHILE" . :WHILE)
    ("UNTIL" . :UNTIL)
    ("AND" . :AND)
    ("OR" . :OR)
    ("NOT" . :NOT)
    ("TRUE" . :TRUE)
    ("FALSE" . :FALSE))
  "Association list of FORTRAN keywords to token symbols.
   OOP keywords added for OOPS system integration.")

(defparameter *fortran-operators-alist*
  '(("+" . :PLUS)
    ("-" . :MINUS)
    ("*" . :TIMES)
    ("/" . :DIVIDE)
    ("**" . :POWER)
    ("=" . :EQUAL)
    ("==" . :EQUAL-EQUAL)
    ("/=" . :NOT-EQUAL)
    ("<>" . :NOT-EQUAL)
    ("<" . :LESS-THAN)
    (">" . :GREATER-THAN)
    ("<=" . :LESS-EQUAL)
    (">=" . :GREATER-EQUAL)
    ("(" . :LEFT-PAREN)
    (")" . :RIGHT-PAREN)
    ("," . :|,|)
    (":" . :|:|)
    ("." . :|.|))
  "Association list of FORTRAN operators to token symbols.")

(defun fortran-valid-number-prefix-p (s)
  "Check if S starts with a valid number literal prefix.
   Supports: 0X/0B/0O (modern), o'...' x'...' b'...' (FORTRAN quoted), DWORD."
  (and (>= (length s) 2)
       (or (string-equal (subseq s 0 2) "0X") ; Hexadecimal modern
           (string-equal (subseq s 0 2) "0B") ; Binary modern
           (string-equal (subseq s 0 2) "0O") ; Octal modern
           (string-equal (subseq s 0 2) "0D") ; Dword modern
           (and (>= (length s) 3) (char-equal (char s 1) #\apostrophe))  ; FORTRAN quoted: X'...', O'...', B'...'
           (and (>= (length s) 5)
                (string-equal (subseq s 0 5) "DWORD"))))) ; Legacy DWORD keyword

(defun fortran-token-type (lexeme)
  "Return token type for LEXEME, a string."
  (cond
    ((and (stringp lexeme) (digit-char-p (char lexeme 0)))
     :NUMBER)
    ((and (stringp lexeme) (fortran-valid-number-prefix-p lexeme))
     :NUMBER)
    ((and (stringp lexeme) (char= (char lexeme 0) #\"))
     :STRING)
    ((assoc lexeme *fortran-keyword-alist* :test #'string-equal)
     (cdr (assoc lexeme *fortran-keyword-alist* :test #'string-equal)))
    ((assoc lexeme *fortran-operators-alist* :test #'string-equal)
     (cdr (assoc lexeme *fortran-operators-alist* :test #'string-equal)))
    ((string= lexeme "(") :LEFT-PAREN)
    ((string= lexeme ")") :RIGHT-PAREN)
    ((string= lexeme ",") :COMMA)
    ((string= lexeme ":") :COLON)
    ((fortran-valid-identifier-p lexeme) :IDENT)
    (t :UNKNOWN)))

(defun fortran-valid-identifier-p (s)
  "Check if S is a valid FORTRAN identifier."
  (and (stringp s)
       (plusp (length s))
       (every (lambda (c)
                (or (alphanumericp c)
                    (char= c #\_)))
              s)))

(defun fortran-lex-line (line)
  "Split LINE into token list of (type value) pairs.
   Handles quoted strings, operators, identifiers, and prefixed number literals."
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
                ;; Scan number with support for:
                ;; - Modern: 0X (hex), 0B (binary), 0O (octal), 0D (dword)
                ;; - FORTRAN quoted: X'...' O'...' B'...' D'WORD...'
                ;; - Decimal: 123, 0, 255
                ;; - Legacy: DWORD prefix
                (let ((base 10)
                      (quoted-p nil))
                  ;; Check for FORTRAN quoted notation (X'...', O'...', B'...', D'...')
                  (when (and (not (eof?)) (>= (1+ i) (length chars))
                             (char-equal (char chars (1+ i)) #\apostrophe))
                    (cond
                      ((char-equal (peek) #\X)
                       (setf base 16 quoted-p t)
                       (next-char) (next-char)) ; consume X'
                      ((char-equal (peek) #\O)
                       (setf base 8 quoted-p t)
                       (next-char) (next-char)) ; consume O'
                      ((char-equal (peek) #\B)
                       (setf base 2 quoted-p t)
                       (next-char) (next-char)) ; consume B'
                      ((char-equal (peek) #\D)
                       (setf base 16 quoted-p t)
                       (next-char) (next-char)) ; consume D'

                  ;; Check for modern 0X, 0B, 0O, 0D prefixes
                  (when (and (not (eof?)) (char= (peek) #\0))
                    (let ((next-pos (1+ i)))
                      (when (< next-pos (length chars))
                        (let ((next-c (nth next-pos chars)))
                          (cond
                            ((char-equal next-c #\X)
                             (setf base 16)
                             (next-char) (next-char)) ; consume 0X
                            ((char-equal next-c #\B)
                             (setf base 2)
                             (next-char) (next-char)) ; consume 0B
                            ((char-equal next-c #\O)
                             (setf base 8)
                             (next-char) (next-char)) ; consume 0O
                            ((char-equal next-c #\D)
                             (setf base 16)
                             (next-char) (next-char)) ; consume 0D
                          ))))

                  ;; Check for DWORD keyword prefix
                  (when (and (not (eof?)) (char-equal (peek) #\D) (zerop base))
                    (let ((test-pos (min (+ i 5) (length chars))))
                      (when (>= test-pos (+ i 5))
                        (let ((prefix (coerce (subseq chars i test-pos) 'string)))
                          (when (string-equal prefix "DWORD")
                            (setf base 16)
                            (loop for _ from 0 below 5 do (next-char)))))))

                  ;; Consume digits appropriate to base
                  (loop while (and (not (eof?))
                                   (not (and quoted-p (char= (peek) #\apostrophe)))
                                   (or (digit-char-p (peek))
                                       (and (> base 10) (find (char-upcase (peek)) "ABCDEF"))))
                        do (next-char))

                  ;; Skip closing quote for quoted notation
                  (when (and quoted-p (not (eof?)) (char= (peek) #\apostrophe))
                    (next-char))

                  (flush-token start i :NUMBER)))
             (scan-string (start)
               (next-char) ; skip opening quote
               (loop while (and (not (eof?)) (char/= (peek) #\"))
                     do (next-char))
               (when (not (eof?)) (next-char)) ; skip closing quote
               (flush-token (1+ start) i :STRING))
             (scan-identifier (start)
               (loop while (and (not (eof?))
                                (or (alphanumericp (peek))
                                    (char= (peek) #\_)))
                     do (next-char))
               (let ((text (coerce (subseq chars start i) 'string)))
                 (let ((type (fortran-token-type text)))
                   (when (not (eq type :UNKNOWN))
<<<<<<< HEAD
                     (flush-token start i type))))))
       (loop until (eof?)
             do (skip-whitespace)
                (cond
                  ((eof?) (return))
                  ((char= (peek) #\") (scan-string i))
                  ((digit-char-p (peek)) (scan-number i))
                  ;; Handle FORTRAN quoted number literals: X'...', O'...', B'...', D'...'
                  ((and (< (1+ i) (length chars))
                        (char-equal (char chars (1+ i)) #\apostrophe))
                   (let ((c (char-upcase (peek))))
                     (if (member c '(#\X #\O #\B #\D))
                         (scan-number i)
                         (scan-identifier i))))
                  ;; Handle DWORD prefix
                  ((and (char-equal (peek) #\D))
                   (let ((test-pos (min (+ i 5) (length chars))))
                     (if (and (>= test-pos (+ i 5))
                              (string-equal (coerce (subseq chars i test-pos) 'string) "DWORD"))
                         (scan-number i)
                         (scan-identifier i))))
                  ((or (alphanumericp (peek))
                       (char= (peek) #\_)) (scan-identifier i))
                  (t
                   (let* ((op1 (string (next-char)))
                          (op2 (when (not (eof?)) (string (peek)))))
                     (cond
                       ((and op2 (string= (concatenate 'string op1 op2) "=="))
                        (next-char)
                        (flush-token (- i 2) i :EQUAL-EQUAL))
                       ((and op2 (string= (concatenate 'string op1 op2) "/="))
                        (next-char)
                        (flush-token (- i 2) i :NOT-EQUAL))
                       ((and op2 (string= (concatenate 'string op1 op2) "<="))
                        (next-char)
                        (flush-token (- i 2) i :LESS-OR-EQUAL))
                       ((and op2 (string= (concatenate 'string op1 op2) ">="))
                        (next-char)
                        (flush-token (- i 2) i :GREATER-OR-EQUAL))
                       ((string= op1 ",")
                        (flush-token (- i 1) i :COMMA))
                       (t
                        (flush-token (- i 1) i :OP)))))))
              (nreverse tokens))))))

(defun fortran-lex-source (source)
  "Lex complete FORTRAN SOURCE string into token stream."
  (let ((all-tokens '()))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\; (char trimmed 0))
                    (char= #\' (char trimmed 0)))
          (setf all-tokens (nconc all-tokens (fortran-lex-line trimmed))))))
    all-tokens))

(defun fortran-normalize-identifier (identifier)
  "Normalize IDENTIFIER to PascalCase form.

FORTRAN identifiers contain alphanumerics and underscores. This function:
- Converts to PascalCase (uppercase first letter, lowercase rest unless preceded by underscore)
- Replaces underscores with uppercase for proper camelCase boundaries
- Ensures result conforms to eightbol naming conventions

INPUTS: IDENTIFIER — a string or symbol

OUTPUTS: normalized string in PascalCase suitable for use in AST nodes

EXAMPLE: fortran-normalize-identifier \"my_variable\" → \"MyVariable\""
  (let* ((str (if (symbolp identifier) (string identifier) identifier))
         (parts (split-sequence:split-sequence #\_ str :remove-empty-subseqs t))
         (normalized-parts (mapcar (lambda (part)
                                     (if (zerop (length part))
                                         ""
                                         (concatenate 'string
                                                      (string-upcase (subseq part 0 1))
                                                      (string-downcase (subseq part 1)))))
                                   parts)))
    (apply #'concatenate 'string normalized-parts)))

(defun fortran-parse-line-prefix (tokens)
  "Parse line number and optional quoted label from start of TOKENS.
    Returns (values line-number label remaining-tokens)."
  (declare (ignore tokens))
  (values nil nil))

(provide 'fortran-lexer)
