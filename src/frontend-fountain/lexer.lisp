;; src/frontend-fountain/lexer.lisp — Tokenization for Fountain screenplay format
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :fountain-frontend)

;;;; Number Literal Recognition

(defun digit-p (char)
  "Check if CHAR is a digit 0-9."
  (and char (digit-char-p char)))

(defun hex-digit-p (char)
  "Check if CHAR is a valid hex digit 0-9, A-F."
  (and char (or (digit-char-p char)
                (and (char-code char)
                     (let ((code (char-upcase char)))
                       (and (>= (char-code code) (char-code #\A))
                            (<= (char-code code) (char-code #\F))))))))

(defun parse-hex-literal (string)
  "Parse STRING as hexadecimal (0x prefix assumed).
Returns (values numeric-value success-p)."
  (let ((hex-part (subseq string 2)))
    (if (every #'hex-digit-p hex-part)
        (values (parse-integer hex-part :radix 16) t)
        (values nil nil))))

(defun parse-octal-literal (string)
  "Parse STRING as octal (0o prefix assumed).
Returns (values numeric-value success-p)."
  (let ((octal-part (subseq string 2)))
    (if (every (lambda (c) (and c (>= (char-code c) (char-code #\0)) (<= (char-code c) (char-code #\7)))) octal-part)
        (values (parse-integer octal-part :radix 8) t)
        (values nil nil))))

(defun parse-binary-literal (string)
  "Parse STRING as binary (0b prefix assumed).
Returns (values numeric-value success-p)."
  (let ((binary-part (subseq string 2)))
    (if (every (lambda (c) (and c (or (char= c #\0) (char= c #\1)))) binary-part)
        (values (parse-integer binary-part :radix 2) t)
        (values nil nil))))

(defun parse-dword-literal (string)
  "Parse STRING as dword (0d\"WORD\" format).
Returns (values numeric-value success-p)."
  (if (and (>= (length string) 5)
           (string-equal (subseq string 0 2) "0d")
           (char= (aref string 2) #\")
           (char= (aref string (- (length string) 1)) #\"))
      (let* ((dword-text (subseq string 3 (- (length string) 1)))
             (bytes (map 'list #'char-code dword-text)))
        (values (apply #'+ (mapcar (lambda (b i) (* b (expt 256 i))) bytes (reverse (loop for i below (length bytes) collect i)))) t))
      (values nil nil)))

(defun parse-number-literal (string)
  "Parse STRING as a number literal (decimal, hex, octal, binary, or dword).
Returns (values numeric-value token-type success-p)."
  (cond
    ;; Hex: 0x prefix
    ((and (>= (length string) 3)
          (string-equal (subseq string 0 2) "0x"))
     (multiple-value-bind (val success) (parse-hex-literal string)
       (if success
           (values val :hex-number t)
           (values nil nil nil))))
    ;; Octal: 0o prefix
    ((and (>= (length string) 3)
          (string-equal (subseq string 0 2) "0o"))
     (multiple-value-bind (val success) (parse-octal-literal string)
       (if success
           (values val :octal-number t)
           (values nil nil nil))))
    ;; Binary: 0b prefix
    ((and (>= (length string) 3)
          (string-equal (subseq string 0 2) "0b"))
     (multiple-value-bind (val success) (parse-binary-literal string)
       (if success
           (values val :binary-number t)
           (values nil nil nil))))
    ;; Dword: 0d"WORD" format
    ((and (>= (length string) 5)
          (string-equal (subseq string 0 2) "0d")
          (char= (aref string 2) #\"))
     (multiple-value-bind (val success) (parse-dword-literal string)
       (if success
           (values val :dword-number t)
           (values nil nil nil))))
    ;; Decimal: regular digits
    ((every #'digit-p string)
     (values (parse-integer string) :decimal-number t))
    (t
     (values nil nil nil))))

;;;; Identifier Normalization

(defun to-pascal-case (string)
  "Convert STRING to PascalCase.
Handles underscores, hyphens, and mixed case input.
Examples: 'my-var' → 'MyVar', 'my_var' → 'MyVar', 'myVar' → 'MyVar'."
  (let ((parts (split-on-delimiters string '(#\- #\_))))
    (format nil "~{~A~}" (mapcar (lambda (part)
                                   (if (> (length part) 0)
                                       (concatenate 'string
                                                    (string-upcase (subseq part 0 1))
                                                    (string-downcase (subseq part 1)))
                                       part))
                                 parts))))

(defun split-on-delimiters (string delimiters)
  "Split STRING on any of DELIMITERS, returning list of parts."
  (let ((parts '())
        (current '()))
    (loop for char across string
          if (member char delimiters)
          do (when current
               (push (coerce (nreverse current) 'string) parts)
               (setf current '()))
          else
          do (push char current))
    (when current
      (push (coerce (nreverse current) 'string) parts))
    (nreverse parts)))

(defun valid-identifier-p (string)
  "Check if STRING is a valid Fountain identifier.
Identifiers start with letter/underscore, contain alphanumerics/underscores/hyphens."
  (and (stringp string)
       (> (length string) 0)
       (or (alpha-char-p (aref string 0))
           (char= (aref string 0) #\_))
       (every (lambda (c) (or (alphanumericp c) (char= c #\-) (char= c #\_)))
              string)))

;;;; Keyword Recognition

(defparameter *fountain-keywords*
   '(("INT" . :int)
     ("EXT" . :ext)
     ("FADE" . :fade)
     ("OUT" . :out)
     ("CUT" . :cut)
     ("TO" . :to)
     ("OPEN" . :open)
     ("ON" . :on)
     ("CLOSE" . :close)
     ("THEN" . :then)
     ("WHEN" . :when)
     ("UNLESS" . :unless)
     ("ENTER" . :enter)
     ("AT" . :at)
     ("LOOKS" . :looks)
     ("FACING" . :facing)
     ("EQUIPS" . :equips)
     ("IS" . :is)
     ("SET" . :set)
     ("TO" . :to)
     ("PRINT" . :print)
     ("INPUT" . :input)
     ("DIALOGUE" . :dialogue)
     ("WHERE" . :where)
     ("IF" . :if)
     ("ELSE" . :else)
     ("AND" . :and)
     ("OR" . :or)
     ("NOT" . :not)
     ("NORTH" . :north)
     ("SOUTH" . :south)
     ("EAST" . :east)
     ("WEST" . :west)
     ("GREATER" . :greater)
     ("THAN" . :than)
     ("LESS" . :less)
     ("EQUAL" . :equal)
     ("THAN" . :than)
     ("DIFFERENCE" . :difference)
     ("SUM" . :sum)
     ("PRODUCT" . :product)
     ("QUOTIENT" . :quotient)
     ("OF" . :of)
     ("FLOOR" . :floor)
     ("CEILING" . :ceiling)
     ("ROUND" . :round)
     ("MODULO" . :modulo)
     ("ABS" . :abs)
     ("MIN" . :min)
     ("MAX" . :max)
     ("BLOB" . :blob)
     ("--" . :transition)
     ("..." . :ellipsis)
     ("[[" . :comment-start)
     ("]]" . :comment-end)
     ;; Tier 1: Character action modifiers (EMOTION, GESTURE, ANIMATION)
     ("EMOTION" . :emotion)
     ("GESTURE" . :gesture)
     ("ANIMATION" . :animation)
     ("ANGRY" . :angry)
     ("SAD" . :sad)
     ("HAPPY" . :happy)
     ("SURPRISED" . :surprised)
     ("CONFUSED" . :confused)
     ("NEUTRAL" . :neutral)
     ;; Tier 1: Camera directions
     ("CAMERA" . :camera)
     ("FRAME" . :frame)
     ("TRUCK" . :truck)
     ("DOLLY" . :dolly)
     ("CLOSE" . :close-on)
     ("INCLUDE" . :include)
     ("CENTER" . :center)
     ("LEFT" . :left)
     ("RIGHT" . :right)
     ("UP" . :up)
     ("DOWN" . :down)
     ;; Tier 1: Timing keywords
     ("BEAT" . :beat)
     ("BEATS" . :beats)
     ("WAIT" . :wait)
     ("FOR" . :for)
     ("SECONDS" . :seconds)
     ("PAUSE" . :pause)
     ;; Tier 1: Dialogue branching
     ("CONTINUE" . :continue))
   "Mapping of Fountain keywords to token types.")

(defun keyword-token-type (string)
  "Return token type for keyword STRING, or nil if not a keyword."
  (cdr (assoc string *fountain-keywords* :test #'string-equal)))

;;;; Lexer State and Tokenization

(defstruct lexer-state
  "Lexer state for Fountain screenplay parsing."
  (input "")                    ; Source code string
  (position 0)                  ; Current position in input
  (line-number 1)               ; Current line number for error reporting
  (column-number 0)             ; Current column in line
  (tokens '())                  ; Accumulated tokens (reversed)
  (current-line ""))            ; Current line being processed

(defun make-token (type value &optional line column)
  "Create a token structure (type value line column)."
  (list type value line column))

(defun token-type (token) (first token))
(defun token-value (token) (second token))
(defun token-line (token) (third token))
(defun token-column (token) (fourth token))

(defun peek-char-at (state &optional (offset 0))
  "Peek at character at current position + OFFSET in lexer STATE."
  (let ((pos (+ (lexer-state-position state) offset)))
    (if (< pos (length (lexer-state-input state)))
        (aref (lexer-state-input state) pos)
        nil)))

(defun advance-char (state &optional count)
  "Advance lexer position by COUNT characters (default 1)."
  (let ((count (or count 1)))
    (setf (lexer-state-position state) (+ (lexer-state-position state) count))
    (setf (lexer-state-column-number state) (+ (lexer-state-column-number state) count))))

(defun skip-whitespace (state)
  "Skip whitespace characters at current position."
  (loop while (and (peek-char-at state)
                   (member (peek-char-at state) '(#\Space #\Tab)))
        do (advance-char state)))

(defun scan-line-comment (state)
  "Scan a line comment starting with ;; and return comment token."
  (let ((start (lexer-state-position state))
        (start-col (lexer-state-column-number state)))
    (loop while (and (peek-char-at state)
                     (not (char= (peek-char-at state) #\Newline)))
          do (advance-char state))
    (let ((comment-text (subseq (lexer-state-input state) (+ start 2))))
      (make-token :line-comment comment-text (lexer-state-line-number state) start-col))))

(defun scan-block-comment (state)
  "Scan a block comment [[...]] and return comment token."
  (let ((start (lexer-state-position state))
        (start-col (lexer-state-column-number state))
        (start-line (lexer-state-line-number state)))
    (advance-char state 2) ; skip [[
    (loop until (or (> (+ (lexer-state-position state) 1) (length (lexer-state-input state)))
                    (and (char= (peek-char-at state) #\])
                         (char= (peek-char-at state 1) #\])))
          do (if (char= (peek-char-at state) #\Newline)
                 (progn
                   (incf (lexer-state-line-number state))
                   (setf (lexer-state-column-number state) 0)
                   (advance-char state))
                 (advance-char state)))
    (when (and (peek-char-at state) (peek-char-at state 1))
      (advance-char state 2)) ; skip ]]
    (let ((comment-text (subseq (lexer-state-input state) (+ start 2) (- (lexer-state-position state) 2))))
      (make-token :block-comment comment-text start-line start-col))))

(defun scan-string-literal (state quote-char)
  "Scan a string literal delimited by QUOTE-CHAR."
  (let ((start (lexer-state-position state))
        (start-col (lexer-state-column-number state))
        (chars '()))
    (advance-char state) ; skip opening quote
    (loop until (or (> (lexer-state-position state) (length (lexer-state-input state)))
                    (char= (peek-char-at state) quote-char))
          do (if (char= (peek-char-at state) #\Newline)
                 (progn
                   (incf (lexer-state-line-number state))
                   (setf (lexer-state-column-number state) 0)
                   (push (peek-char-at state) chars)
                   (advance-char state))
                 (progn
                   (push (peek-char-at state) chars)
                   (advance-char state))))
    (when (peek-char-at state)
      (advance-char state)) ; skip closing quote
    (make-token :string-literal
                (coerce (nreverse chars) 'string)
                (lexer-state-line-number state)
                start-col)))

(defun scan-identifier-or-keyword (state)
  "Scan an identifier or keyword starting at current position."
  (let ((start (lexer-state-position state))
        (start-col (lexer-state-column-number state)))
    (loop while (and (peek-char-at state)
                     (or (alphanumericp (peek-char-at state))
                         (member (peek-char-at state) '(#\- #\_))))
          do (advance-char state))
    (let* ((text (subseq (lexer-state-input state) start (lexer-state-position state)))
           (keyword-type (keyword-token-type text)))
      (if keyword-type
          (make-token keyword-type text (lexer-state-line-number state) start-col)
          (make-token :identifier (to-pascal-case text) (lexer-state-line-number state) start-col)))))

(defun scan-number (state)
  "Scan a number literal (decimal, hex, octal, binary, or dword)."
  (let ((start (lexer-state-position state))
        (start-col (lexer-state-column-number state)))
    (cond
      ;; Check for 0x (hex), 0o (octal), 0b (binary), or 0d (dword)
      ((and (char= (peek-char-at state) #\0)
            (member (peek-char-at state 1) '(#\x #\X #\o #\O #\b #\B #\d #\D)))
       (advance-char state 2) ; skip 0x/0o/0b/0d
       ;; For 0d, expect quote
       (if (char= (aref (lexer-state-input state) (- (lexer-state-position state) 1)) #\d)
           (if (char= (peek-char-at state) #\")
               (progn
                 (advance-char state) ; skip opening quote
                 (loop while (and (peek-char-at state) (not (char= (peek-char-at state) #\")))
                       do (advance-char state))
                 (when (peek-char-at state)
                   (advance-char state))) ; skip closing quote
               (loop until (or (> (lexer-state-position state) (length (lexer-state-input state)))
                               (not (hex-digit-p (peek-char-at state))))
                     do (advance-char state)))
           (loop until (or (> (lexer-state-position state) (length (lexer-state-input state)))
                           (not (hex-digit-p (peek-char-at state))))
                 do (advance-char state))))
      ;; Regular decimal number
      (t
       (loop while (and (peek-char-at state) (digit-p (peek-char-at state)))
             do (advance-char state))))
    (let* ((text (subseq (lexer-state-input state) start (lexer-state-position state))))
      (multiple-value-bind (val type success) (parse-number-literal text)
        (if success
            (make-token type val (lexer-state-line-number state) start-col)
            (make-token :unknown text (lexer-state-line-number state) start-col))))))

(defun scan-variable (state)
  "Scan a variable reference starting with $ prefix."
  (let ((start (lexer-state-position state))
        (start-col (lexer-state-column-number state)))
    (advance-char state) ; skip $
    (loop while (and (peek-char-at state)
                     (or (alphanumericp (peek-char-at state))
                         (member (peek-char-at state) '(#\- #\_))))
          do (advance-char state))
    (let ((var-name (subseq (lexer-state-input state) (+ start 1) (lexer-state-position state))))
      (make-token :variable (to-pascal-case var-name) (lexer-state-line-number state) start-col))))

(defun scan-operator (state)
  "Scan an operator or punctuation token."
  (let ((start-col (lexer-state-column-number state))
        (line (lexer-state-line-number state))
        (char (peek-char-at state))
        (next-char (peek-char-at state 1)))
    ;; Two-character operators
    (when next-char
      (let ((two-char (concatenate 'string (string char) (string next-char))))
        (cond
          ((string-equal two-char "..")
           (advance-char state 2)
           (return-from scan-operator (make-token :ellipsis ".." line start-col)))
          ((string-equal two-char "--")
           (advance-char state 2)
           (return-from scan-operator (make-token :transition "--" line start-col))))))
    ;; Single-character operators
    (advance-char state)
    (make-token (case char
                  (#\( :lparen)
                  (#\) :rparen)
                  (#\[ :lbracket)
                  (#\] :rbracket)
                  (#\{ :lbrace)
                  (#\} :rbrace)
                  (#\, :comma)
                  (#\. :dot)
                  (#\= :equal)
                  (#\+ :plus)
                  (#\- :minus)
                  (#\* :star)
                  (#\/ :slash)
                  (#\> :gt)
                  (#\< :lt)
                  (#\! :bang)
                  (#\& :ampersand)
                  (#\| :pipe)
                  (#\^ :caret)
                  (t :unknown))
                (string char) line start-col)))

(defun lex-fountain-source (source)
  "Tokenize Fountain SOURCE code into a list of tokens."
  (let ((state (make-lexer-state :input source)))
    (loop until (>= (lexer-state-position state) (length (lexer-state-input state)))
          do (let ((char (peek-char-at state)))
               (cond
                 ;; Newline
                 ((char= char #\Newline)
                  (incf (lexer-state-line-number state))
                  (setf (lexer-state-column-number state) 0)
                  (push (make-token :newline "" (lexer-state-line-number state) 0) (lexer-state-tokens state))
                  (advance-char state))
                 ;; Whitespace
                 ((member char '(#\Space #\Tab))
                  (skip-whitespace state))
                 ;; Comments: ;; (line comment) or [[ (block comment)
                 ((and (char= char #\;) (char= (peek-char-at state 1) #\;))
                  (push (scan-line-comment state) (lexer-state-tokens state)))
                 ((and (char= char #\[) (char= (peek-char-at state 1) #\[))
                  (push (scan-block-comment state) (lexer-state-tokens state)))
                 ;; String literals
                 ((char= char #\")
                  (push (scan-string-literal state #\") (lexer-state-tokens state)))
                 ((char= char #\')
                  (push (scan-string-literal state #\') (lexer-state-tokens state)))
                 ;; Variables: $name
                 ((char= char #\$)
                  (push (scan-variable state) (lexer-state-tokens state)))
                 ;; Numbers
                 ((or (digit-p char) (and (char= char #\0) (member (peek-char-at state 1) '(#\x #\X #\o #\O #\b #\B #\d #\D))))
                  (push (scan-number state) (lexer-state-tokens state)))
                 ;; Identifiers and keywords
                 ((or (alpha-char-p char) (char= char #\_))
                  (push (scan-identifier-or-keyword state) (lexer-state-tokens state)))
                 ;; Operators and punctuation
                 (t
                  (push (scan-operator state) (lexer-state-tokens state))))))
    (nreverse (lexer-state-tokens state))))

(defun lex-fountain-file (filepath)
  "Tokenize a Fountain file from FILEPATH."
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (let ((source (make-string (file-length stream))))
      (read-sequence source stream)
      (lex-fountain-source source))))

(export '(lex-fountain-source
          lex-fountain-file
          make-token
          token-type
          token-value
          token-line
          token-column
          to-pascal-case
          parse-number-literal
          valid-identifier-p))
