;; src/frontend-forth/lexer.lisp — Forth tokenizer and lexer
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Forth keyword and operation mappings

(defparameter *forth-core-words-alist*
  '(;; Stack operations
    ("DUP" . :DUP)
    ("SWAP" . :SWAP)
    ("DROP" . :DROP)
    ("OVER" . :OVER)
    ("ROT" . :ROT)
    ("2DUP" . :TWODUP)
    ("2SWAP" . :TWOSWAP)
    ("2DROP" . :TWODROP)
    ("NIP" . :NIP)
    ("TUCK" . :TUCK)
    ("PICK" . :PICK)
    ("ROLL" . :ROLL)
    ;; Arithmetic
    ("+" . :PLUS)
    ("-" . :MINUS)
    ("*" . :TIMES)
    ("/" . :DIVIDE)
    ("MOD" . :MOD)
    ("ABS" . :ABS)
    ("NEGATE" . :NEGATE)
    ;; Logic
    ("AND" . :AND)
    ("OR" . :OR)
    ("XOR" . :XOR)
    ("NOT" . :NOT)
    ("<<" . :LSHIFT)
    (">>" . :RSHIFT)
    ;; Comparison
    ("=" . :EQUAL)
    ("<" . :LT)
    (">" . :GT)
    ("<=" . :LE)
    (">=" . :GE)
    ("<>" . :NE)
    ("0=" . :ZEQUAL)
    ("0<" . :ZLT)
    ("0>" . :ZGT)
    ;; Control flow
    ("IF" . :IF)
    ("THEN" . :THEN)
    ("ELSE" . :ELSE)
    ("BEGIN" . :BEGIN)
    ("UNTIL" . :UNTIL)
    ("WHILE" . :WHILE)
    ("REPEAT" . :REPEAT)
    ("DO" . :DO)
    ("LOOP" . :LOOP)
    ("+LOOP" . :PLUSLOOP)
    ("BREAK" . :BREAK)
    ;; Output / Dialogue
    ("." . :PRINT)
    (".\"" . :PRINTS)
    ("EMIT" . :EMIT)
    ("CR" . :CR)
    ("SPACE" . :SPACE)
    ("SPACES" . :SPACES)
    ("PAGE" . :PAGE)
    ;; Input
    ("KEY" . :KEY)
    ("ACCEPT" . :ACCEPT)
    ;; Variables and memory
    ("VARIABLE" . :VARIABLE)
    ("@" . :FETCH)
    ("!" . :STORE)
    ("+!" . :PLUSSTORE)
    ("ALLOT" . :ALLOT)
    ("," . :COMMA)
    ("C@" . :CFETCH)
    ("C!" . :CSTORE)
    ;; Return stack
    (">R" . :TORET)
    ("R>" . :FROMRET)
    ("R@" . :RATFETCH)
    ("2>R" . :TWORET)
    ("2R>" . :TWOFROMRET)
    ;; Dictionary
    (":" . :COLON)
    (";" . :SEMICOLON)
    ("CONSTANT" . :CONSTANT)
    ("CREATE" . :CREATE)
    ("DOES>" . :DOES)
    ;; Miscellaneous
    ("EXIT" . :EXIT)
    ("QUIT" . :QUIT)
    ("BYE" . :BYE)
    ("WORDS" . :WORDS)
    ("FORGET" . :FORGET)
    ("EXECUTE" . :EXECUTE)
    ("CALL" . :CALL)
    ("GOBACK" . :GOBACK))
  "Association list of Forth core words to token symbols.")

(defparameter *forth-dialogue-words-alist*
  '(;; Dialogue/narrative
    ("SAY" . :SAY)
    ("NARRATE" . :NARRATE)
    ("CHOICE" . :CHOICE)
    ("OPTION" . :OPTION)
    ("WAIT" . :WAIT)
    ("PAUSE" . :PAUSE)
    ("SHOW" . :SHOW)
    ("HIDE" . :HIDE)
    ("FADE" . :FADE)
    ("MUSIC" . :MUSIC)
    ("SOUND" . :SOUND)
    ("CLEAR" . :CLEAR))
  "Forth dialogue and narrative extension words.")

;;; Lexical analysis

(defun forth-normalize-identifier (s)
  "Normalize FORTH identifier to SHOUT-CASE (uppercase).
Replaces underscores and hyphens with underscores in output.
Examples: 'my-var' → 'MY_VAR', 'myVar' → 'MYVAR'"
  (when (stringp s)
    (string-upcase
      (substitute #\_ #\- (remove #\apostrophe s)))))

(defun forth-valid-identifier-p (s)
  "Check if S is a valid Forth identifier.
Valid Forth identifiers may contain:
  - alphanumeric characters
  - hyphens, underscores
  - apostrophes (for dword syntax)
Must not start with a digit or special char (except for #)."
  (when (and (stringp s) (plusp (length s)))
    (let ((first-char (char s 0)))
      (and (or (alpha-char-p first-char) (char= first-char #\#))
           (every (lambda (c)
                    (or (alphanumericp c) (find c "-_'")))
                  s)))))

(defun forth-lex-decimal (chars pos)
  "Lex a decimal number starting at POS.
Returns (values token-value new-pos) or (values NIL pos) if not a number."
  (let ((start pos))
    (loop while (and (< pos (length chars))
                     (digit-char-p (aref chars pos)))
          do (incf pos))
    (if (> pos start)
        (values (parse-integer (subseq chars start pos)) pos)
        (values nil pos))))

(defun forth-lex-hex (chars pos prefix-len)
  "Lex a hexadecimal number.
PREFIX-LEN indicates: 1 for '$' prefix, 2 for '0x' prefix."
  (let ((start (+ pos prefix-len)))
    (loop while (and (< pos (length chars))
                     (digit-char-p (aref chars pos) 16))
          do (incf pos))
    (if (> pos start)
        (values (parse-integer (subseq chars start pos) :radix 16) pos)
        (values nil (+ pos prefix-len)))))

(defun forth-lex-octal (chars pos)
  "Lex an octal number with '0o' prefix."
  (let ((start (+ pos 2)))
    (loop while (and (< pos (length chars))
                     (find (aref chars pos) "01234567"))
          do (incf pos))
    (if (> pos start)
        (values (parse-integer (subseq chars start pos) :radix 8) pos)
        (values nil (+ pos 2)))))

(defun forth-lex-binary (chars pos prefix-len)
  "Lex a binary number.
PREFIX-LEN indicates: 1 for '%' prefix, 2 for '0b' prefix."
  (let ((start (+ pos prefix-len)))
    (loop while (and (< pos (length chars))
                     (find (aref chars pos) "01"))
          do (incf pos))
    (if (> pos start)
        (values (parse-integer (subseq chars start pos) :radix 2) pos)
        (values nil (+ pos prefix-len)))))

(defun forth-lex-dword (chars pos)
  "Lex a dword literal: d'WORD' format.
Returns the 4-character word as a string, or nil if invalid."
  (when (and (>= (length chars) (+ pos 4))
             (char= (aref chars pos) #\d)
             (char= (aref chars (+ pos 1)) #\apostrophe))
    (let ((word-end (position #\apostrophe chars :start (+ pos 2))))
      (when (and word-end (= (- word-end (+ pos 2)) 4))
        (values (subseq chars (+ pos 2) word-end) (+ word-end 1))))))

(defun forth-lex-number (chars pos)
  "Lex a number literal starting at POS.
Supports: decimal, hex ($xxxx or 0x), octal (0o), binary (%xxxx or 0b), dword (d'WORD')
Returns (values token-type token-value new-pos) or NIL if not a number."
  (when (< pos (length chars))
    (let ((ch (aref chars pos)))
      (cond
        ;; Hex with $ prefix
        ((char= ch #\$)
         (multiple-value-bind (val new-pos)
             (forth-lex-hex chars pos 1)
           (when val (values :hex-number val new-pos))))
        ;; Dword literal d'WORD'
        ((and (char= ch #\d) (< (+ pos 1) (length chars))
              (char= (aref chars (+ pos 1)) #\apostrophe))
         (multiple-value-bind (val new-pos)
             (forth-lex-dword chars pos)
           (when val (values :dword-literal val new-pos))))
        ;; 0x, 0o, 0b prefixes
        ((and (char= ch #\0) (< (+ pos 1) (length chars)))
         (let ((next-ch (aref chars (+ pos 1))))
           (cond
             ((char= next-ch #\x)
              (multiple-value-bind (val new-pos)
                  (forth-lex-hex chars pos 2)
                (when val (values :hex-number val new-pos))))
             ((char= next-ch #\o)
              (multiple-value-bind (val new-pos)
                  (forth-lex-octal chars pos)
                (when val (values :octal-number val new-pos))))
             ((char= next-ch #\b)
              (multiple-value-bind (val new-pos)
                  (forth-lex-binary chars pos 2)
                (when val (values :binary-number val new-pos))))
             ;; Fallback to decimal
             (t (multiple-value-bind (val new-pos)
                    (forth-lex-decimal chars pos)
                  (when val (values :number val new-pos)))))))
        ;; Binary with % prefix
        ((char= ch #\%)
         (multiple-value-bind (val new-pos)
             (forth-lex-binary chars pos 1)
           (when val (values :binary-number val new-pos))))
        ;; Decimal number
        ((digit-char-p ch)
         (multiple-value-bind (val new-pos)
             (forth-lex-decimal chars pos)
           (when val (values :number val new-pos))))))))

(defun forth-lex-string (chars pos)
  "Lex a quoted string from POS.
Supports both single and double quotes.
Returns (values string-content new-pos)."
  (let ((quote-char (aref chars pos))
        (start (1+ pos))
        (pos (1+ pos)))
    (loop while (and (< pos (length chars))
                     (char/= (aref chars pos) quote-char))
          do (if (and (char= (aref chars pos) #\\)
                      (< (1+ pos) (length chars)))
                 (incf pos 2)
                 (incf pos)))
    (if (< pos (length chars))
        (values (subseq chars start pos) (1+ pos))
        (error "Unterminated string literal at position ~D" pos))))

(defun forth-lex-word (chars pos)
  "Lex a word (identifier or keyword) starting at POS.
Returns (values word new-pos)."
  (let ((start pos))
    (loop while (and (< pos (length chars))
                     (not (find (aref chars pos) " \t\n\r(")))
          do (incf pos))
    (values (subseq chars start pos) pos)))

(defun forth-lex-comment-line (chars pos)
  "Skip a line comment starting with \\ or ( ... )
Returns new position after comment."
  (cond
    ;; Line comment with backslash
    ((char= (aref chars pos) #\\)
     (loop while (and (< pos (length chars))
                      (not (find (aref chars pos) '(#\Newline #\Return))))
           do (incf pos))
     pos)
    ;; Parenthetical comment
    ((char= (aref chars pos) #\()
     (incf pos)
     (loop while (and (< pos (length chars))
                      (char/= (aref chars pos) #\)))
           do (incf pos))
     (if (< pos (length chars)) (1+ pos) pos))))

(defun forth-lex-line (line)
  "Tokenize a single LINE of Forth source code.
Returns a list of (type . value) tokens.
Token types: :word, :number, :hex-number, :octal-number, :binary-number,
             :dword-literal, :string, :lparen, :rparen."
  (let ((tokens '())
        (chars (coerce line 'list))
        (pos 0)
        (len (length line)))
    (flet ((peek () (when (< pos len) (aref line pos)))
           (skip-whitespace ()
             (loop while (and (< pos len) (find (aref line pos) " \t"))
                   do (incf pos))))
      (loop while (< pos len)
            do (skip-whitespace)
               (when (< pos len)
                 (let ((ch (aref line pos)))
                   (cond
                     ;; Comments
                     ((char= ch #\\)
                      (setq pos (forth-lex-comment-line line pos)))
                     ;; Parenthetical comment
                     ((char= ch #\()
                      (setq pos (forth-lex-comment-line line pos)))
                     ;; Quoted strings
                     ((find ch "\"'")
                      (multiple-value-bind (str new-pos)
                          (forth-lex-string line pos)
                        (push (cons :string str) tokens)
                        (setq pos new-pos)))
                     ;; Numbers
                     (t
                      (multiple-value-bind (type val new-pos)
                          (forth-lex-number line pos)
                        (if type
                            (progn
                              (push (cons type val) tokens)
                              (setq pos new-pos))
                            ;; Otherwise lex as word
                            (multiple-value-bind (word new-pos)
                                (forth-lex-word line pos)
                              (push (cons :word word) tokens)
                              (setq pos new-pos))))))))))
    (nreverse tokens)))

(defun forth-tokenize-source (source-string)
  "Tokenize a complete Forth source file.
INPUT:  source-string — the Forth source code as a single string
OUTPUT: list of token lists, one per line; each token is (type . value)

Example:
  (forth-tokenize-source \"DUP SWAP +\")
  → (((:word . \"DUP\") (:word . \"SWAP\") (:word . \"+\")))"
  (let ((tokens-per-line '())
        (lines (cl-ppcre:split "\\n" source-string)))
    (dolist (line lines)
      (let ((line-tokens (forth-lex-line line)))
        (when line-tokens
          (push line-tokens tokens-per-line))))
    (nreverse tokens-per-line)))

(defun forth-token-type (lexeme)
  "Determine the token type for a LEXEME (string).
Returns a keyword describing the token class:
  :keyword — a Forth core word
  :dialogue-word — dialogue/narrative extension
  :ident — user-defined identifier
  :unknown — unrecognized token"
  (cond
    ((and (stringp lexeme)
          (assoc (forth-normalize-identifier lexeme)
                 *forth-core-words-alist* :test #'string-equal))
     :keyword)
    ((and (stringp lexeme)
          (assoc (forth-normalize-identifier lexeme)
                 *forth-dialogue-words-alist* :test #'string-equal))
     :dialogue-word)
    ((forth-valid-identifier-p lexeme)
     :ident)
    (t :unknown)))

(defun forth-get-keyword-token (lexeme)
  "Get the keyword token symbol for a Forth word.
Returns the token symbol (e.g., :DUP) or NIL if not found."
  (let ((normalized (forth-normalize-identifier lexeme)))
    (cdr (assoc normalized *forth-core-words-alist* :test #'string-equal))))

(defun forth-get-dialogue-token (lexeme)
  "Get the dialogue word token symbol.
Returns the token symbol (e.g., :SAY) or NIL if not found."
  (let ((normalized (forth-normalize-identifier lexeme)))
    (cdr (assoc normalized *forth-dialogue-words-alist* :test #'string-equal))))

(defun forth-lex-source (source)
  "Tokenize a complete Forth SOURCE string.
INPUT:  source-string — the Forth source code as a single string
OUTPUT: list of token lists, one per line; each token is (type . value)"
  (let ((all-tokens '()))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\; (char trimmed 0)))
          (setq all-tokens (append all-tokens (forth-lex-line trimmed))))))
    all-tokens))

(defun forth-lex-token ()
  "Read next token from *standard-input*."
  (declare (special *forth-lex-token-buffer*))
  (cond
    ((and (boundp '*forth-lex-token-buffer*) *forth-lex-token-buffer*)
     (prog1 (first *forth-lex-token-buffer*)
       (setf *forth-lex-token-buffer* (rest *forth-lex-token-buffer*))))
    (t
     (let ((line (read-line *standard-input* nil nil)))
       (when line
         (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
           (unless (or (zerop (length trimmed))
                       (char= #\; (char trimmed 0)))
             (setq all-tokens (append all-tokens (forth-lex-line trimmed))))))
     (nreverse all-tokens))))

(defun forth-token-list ()
  "Return a thunk that reads tokens from *standard-input* using forth-lex-token."
  (lambda ()
    (let ((token (forth-lex-token)))
      (when token
        (cons token (funcall (forth-token-list)))))))
