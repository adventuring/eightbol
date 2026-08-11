;;; lua-lexer.lisp
;;; Lexer for Lua subset
;;; Produces tokens for the Lua parser.
;;; Supports: decimal, hex (0x), octal (0o), binary (0b) number formats
;;; Normalizes identifiers to snake_case

(in-package :eightbol)

;;; Token types for Lua lexer
;;; We define our own token types, but note that the parser will expect these.
;;; We will use keywords that are a subset of the EightBol token-list where possible,
;;; but for Lua-specific tokens we use new symbols. However, to avoid changing
;;; the EightBol token-list, we will use the existing token types as much as possible
;;; and for the rest we will use :symbol and :bareword and let the parser handle
;;; the Lua-specific meaning.

;;; However, to keep the lexer simple, we will produce tokens that are
;;; compatible with the EightBol parser's expectation by mapping Lua tokens to
;;; existing EightBol token types where possible, and for the rest we will
;;; produce :symbol tokens and let the parser interpret them based on context.

;;; We will define:
;;;   - Lua keywords that are in the EightBol token-list (like IF, THEN, ELSE, END, etc.) 
;;;     as :keyword tokens.
;;;   - Lua-specific keywords (like UNTIL, FOR, IN, etc.) that are not in the EightBol
;;;     token-list will be produced as :symbol tokens, and the parser will treat them
;;;     as keywords based on the grammar rule.
;;;   - Identifiers and Lua operators will be :symbol tokens.
;;;   - Numbers will be :number tokens.
;;;   - Strings will be :string tokens.
;;;   - Comments will be :comment tokens.

(defun tokenize-lua (source)
  "Tokenize Lua SOURCE string.
Return a list of tokens, each token is a plist:
  (:type :value :line :column)"
  (let ((tokens '())
        (line 1)
        (column 0)
        (position 0)
        (length (length source)))
    (labels
        ((peek (&optional (offset 0))
           (when (< (+ position offset) length)
             (char source (+ position offset))))
         (current-char ()
           (peek 0))
         (advance (&optional (n 1))
           (incf position n)
           (when (>= position length)
             (setf position length))
           (incf column n))
         (skip-whitespace ()
           (loop while (and (< position length)
                            (whitespace-char-p (peek)))
                 do (when (char= (peek) #\Newline)
                      (incf line)
                      (setf column 0))
                    (advance))))
      (loop while (< position length) do
            (skip-whitespace)
            (when (>= position length) (return))
            (let ((start-position position)
                  (start-line line)
                  (start-column column))
              (cond
;; Handle comments
                 ((and (char= (peek) #\-)
                       (char= (peek 1) #\-))
                  (progn
                    (loop while (and (< position length)
                                     (not (char= (peek) #\Newline)))
                          do (advance))
                    ;; Skip the comment, no token produced
                    ))
                 ;; Handle multi-line strings [[...]] [=[...]=] [==[...]==] etc.
                 ((and (char= (peek) #\[)
                       (char= (peek 1) #\[))
                  (advance 2) ;; skip [[
                  (let* ((eq-count (count-equals source position))
                         (closing-bracket (format nil "]~a]" (make-string eq-count :initial-element #\=)))
                         (string-value (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
                         (done nil))
                    (loop while (and (< position length) (not done)) do
                          (if (and (char= (peek) #\])
                                   (string= (subseq source position (min (+ position (length closing-bracket)) length))
                                            closing-bracket))
                              (progn
                                (incf position (length closing-bracket))
                                (incf column (length closing-bracket))
                                (setf done t))
                              (progn
                                (when (char= (peek) #\Newline)
                                  (incf line)
                                  (setf column 0))
                                (vector-push-extend (peek) string-value)
                                (advance))))
                    (push (list :type :string
                                :value (coerce string-value 'string)
                                :line start-line
                                :column start-column)
                          tokens)))
                 ;; Handle single-line strings
                 ((or (char= (peek) #\") (char= (peek) #\apostrophe))
                  (let ((quote (peek))
                        (string-value (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
                    (advance) ;; skip opening quote
                    (loop while (and (< position length)
                                     (not (char= (peek) quote))
                                     (not (char= (peek) #\Newline)))
                          do (vector-push-extend (peek) string-value)
                             (advance))
                    (when (and (< position length) (char= (peek) quote))
                      (advance)) ;; skip closing quote
                    (push (list :type :string
                                :value (coerce string-value 'string)
                                :line start-line
                                :column start-column)
                          tokens)))
                 ;; Handle numbers (decimal, hex 0x, octal 0o, binary 0b, dword 0d"WORD")
                ((digit-char-p (peek))
                 (let ((number-value (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
                       (has-decimal-point nil)
                       (radix 10)
                       (is-special-radix nil)
                       (is-dword nil))
                   ;; Check for hex (0x), octal (0o), binary (0b), dword (0d"...") prefixes
                   (when (char= (peek) #\0)
                     (vector-push-extend (peek) number-value)
                     (advance)
                     (cond
                       ;; Hexadecimal: 0x...
                       ((and (< position length) (char-equal (peek) #\x))
                        (advance) ;; skip the x, don't include in number-value
                        (setf radix 16)
                        (setf is-special-radix t)
                        (loop while (and (< position length)
                                         (or (digit-char-p (peek))
                                             (member (char-upcase (peek)) '(#\A #\B #\C #\D #\E #\F))))
                              do (vector-push-extend (char-upcase (peek)) number-value)
                                 (advance)))
                       ;; Octal: 0o...
                       ((and (< position length) (char-equal (peek) #\o))
                        (advance) ;; skip the o, don't include in number-value
                        (setf radix 8)
                        (setf is-special-radix t)
                        (loop while (and (< position length)
                                         (digit-char-p (peek))
                                         (< (digit-char-p (peek)) 8))
                              do (vector-push-extend (peek) number-value)
                                 (advance)))
                       ;; Binary: 0b...
                       ((and (< position length) (char-equal (peek) #\b))
                        (advance) ;; skip the b, don't include in number-value
                        (setf radix 2)
                        (setf is-special-radix t)
                        (loop while (and (< position length)
                                         (or (char= (peek) #\0)
                                             (char= (peek) #\1)))
                              do (vector-push-extend (peek) number-value)
                                 (advance)))
                       ;; Dword: 0d"..."
                       ((and (< position length) (char-equal (peek) #\d))
                        (advance) ;; skip the d
                        (when (and (< position length) (char= (peek) #\"))
                          (advance) ;; skip opening quote
                          (setf is-dword t)
                          ;; Read up to 4 characters into the dword value
                          (loop while (and (< position length)
                                           (not (char= (peek) #\"))
                                           (not (char= (peek) #\Newline))
                                           (< (fill-pointer number-value) 4))
                                do (vector-push-extend (peek) number-value)
                                   (advance))
                          (when (and (< position length) (char= (peek) #\"))
                            (advance)))) ;; skip closing quote
                       ;; Otherwise, it's a regular decimal
                       (t
                        (loop while (and (< position length)
                                         (or (digit-char-p (peek))
                                             (and (char= (peek) #\.)
                                                  (not has-decimal-point))))
                              do (when (char= (peek) #\.)
                                   (setf has-decimal-point t))
                                 (vector-push-extend (peek) number-value)
                                 (advance)))))
                   ;; If not a special prefix, continue parsing decimal
                   (when (and (= radix 10) (not is-special-radix) (not is-dword))
                     (loop while (and (< position length)
                                      (or (digit-char-p (peek))
                                          (and (char= (peek) #\.)
                                               (not has-decimal-point))))
                           do (when (char= (peek) #\.)
                                (setf has-decimal-point t))
                              (vector-push-extend (peek) number-value)
                              (advance)))
                   ;; Parse the collected number string
                   (let ((num-str (coerce number-value 'string)))
                     (when (> (length num-str) 0)
                       (if is-dword
                           ;; For dword, convert each character to a byte and combine
                           (let ((dword-value 0))
                             (loop for c across num-str
                                   do (setf dword-value (logior (ash dword-value 8) (char-code c))))
                             (push (list :type :number
                                         :value dword-value
                                         :line start-line
                                         :column start-column)
                                    tokens))
                           ;; For other radixes, parse as integer
                           (push (list :type :number
                                       :value (parse-integer num-str :radix radix)
                                       :line start-line
                                       :column start-column)
                                  tokens))))))
                 ;; Handle identifiers and keywords
                ((or (alpha-char-p (peek))
                     (char= (peek) #\_))
                 (let ((identifier-value (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
                   (loop while (and (< position length)
                                    (or (alphanumericp (peek))
                                        (char= (peek) #\_)))
                         do (vector-push-extend (peek) identifier-value)
                            (advance))
                   (let ((id (coerce identifier-value 'string)))
                     (unless (lua-keyword-p id)
                       ;; Normalize non-keyword identifiers to snake_case
                       (setf id (identifier-to-snake-case id)))
                     (push (list :type (if (lua-keyword-p id) :keyword :symbol)
                                 :value id
                                 :line start-line
                                 :column start-column)
                           tokens))))
                ;; Handle operators and punctuation
                (t
                 (let ((op (peek)))
                   (advance)
                   (push (list :type :symbol
                               :value (string op)
                               :line start-line
                               :column start-column)
                         tokens)))))))
    tokens))

(defun count-equals (source start-pos)
  "Count the number of consecutive = characters starting at START-POS in SOURCE."
  (let ((count 0))
    (loop while (and (< (+ start-pos count) (length source))
                     (char= (char source (+ start-pos count)) #\=))
          do (incf count))
    count))

(defun whitespace-char-p (char)
  "Return true if CHAR is a whitespace character."
  (find char '(#\Space #\Tab #\Newline #\Linefeed #\Page #\Return) :test #'char=))

(defun lua-keyword-p (token)
  "Return true if TOKEN is a Lua keyword."
  (member token '("and" "break" "do" "else" "elseif"
                   "end" "false" "for" "function" "if" "in"
                   "local" "nil" "not" "or" "repeat" "return"
                   "then" "true" "until" "while"
                   "dialogue" "print" "input" "dialog")
           :test #'string-equal))

(defun identifier-to-snake-case (identifier)
  "Convert IDENTIFIER to snake_case format.
Handles PascalCase (e.g. 'MyVariable' -> 'my_variable'),
camelCase (e.g. 'myVariable' -> 'my_variable'),
and preserves existing snake_case identifiers."
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (len (length identifier)))
    (loop for i below len
          for char = (char identifier i)
          do (cond
               ;; Insert underscore before uppercase letters (except at start)
               ((and (> i 0)
                     (upper-case-p char)
                     (> (fill-pointer result) 0)
                     (not (char= (char result (- (fill-pointer result) 1)) #\_)))
                (vector-push-extend #\_ result)
                (vector-push-extend (char-downcase char) result))
               ;; Convert uppercase to lowercase
               ((upper-case-p char)
                (vector-push-extend (char-downcase char) result))
               ;; Keep everything else as-is
               (t (vector-push-extend char result))))
    (coerce result 'string)))
