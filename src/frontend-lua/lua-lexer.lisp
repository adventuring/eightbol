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

(defun lua-lex-line (line)
  "Tokenize a single LINE of Lua source code.
   Returns a list of tokens, each token is (type value)."
  (let ((result '())
        (pos 0)
        (len (length line)))
    (labels
        ((skip-whitespace ()
           (loop while (and (< pos len)
                            (find (char line pos) '(#\Space #\Tab)))
                 do (incf pos)))
         (read-string ()
           (let ((quote (char line pos))
                 (start (1+ pos))
                 (str (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
             (incf pos)
             (loop while (and (< pos len) (not (char= (char line pos) quote)))
                   do (vector-push-extend (char line pos) str)
                      (incf pos))
             (when (< pos len) (incf pos))
             (push (list :type :string :value (coerce str 'string)) result)))
         (read-number ()
           (let ((start pos)
                 (radix 10))
             (when (and (< pos len) (char= (char line pos) #\0) (< (1+ pos) len))
               (case (char-upcase (char line (1+ pos)))
                 (#\X (setf radix 16) (incf pos 2))
                 (#\O (setf radix 8) (incf pos 2))
                 (#\B (setf radix 2) (incf pos 2))
                 (otherwise ())))
             (loop while (and (< pos len)
                              (if (> radix 10)
                                  (or (digit-char-p (char line pos))
                                      (find (char-upcase (char line pos)) "ABCDEF"))
                                  (digit-char-p (char line pos))))
                   do (incf pos))
             (push (list :type :number :value (parse-integer (subseq line start pos) :radix radix)) result)))
          (read-operator ()
            (let ((start pos)
                  (c (char line pos)))
              ;; Handle two-character operators
              (cond
                ((and (< (1+ pos) len)
                      (or (string= (subseq line pos (+ pos 2)) "<<")
                          (string= (subseq line pos (+ pos 2)) ">>")
                          (string= (subseq line pos (+ pos 2)) "||")
                          (string= (subseq line pos (+ pos 2)) "&&")
                          (string= (subseq line pos (+ pos 2)) "==")
                          (string= (subseq line pos (+ pos 2)) "~=")))
                 (incf pos 2)
                 (push (list :type :symbol :value (subseq line start pos)) result))
                ;; Single-character operators
                ((find c '(#\+ #\- #\* #\/ #\% #\# #\< #\> #\= #\~ #\& #\| #\^ #\. #\?))
                 (incf pos)
                 (push (list :type :symbol :value (string c)) result))
                (t (incf pos)))))
         (read-identifier ()
           (let ((start pos))
             (loop while (and (< pos len)
                              (or (alphanumericp (char line pos))
                                  (char= (char line pos) #\_)))
                   do (incf pos))
             (push (list :type (if (lua-keyword-p (subseq line start pos)) :keyword :symbol)
                         :value (subseq line start pos))
                   result))))
       (loop while (< pos len) do
         (skip-whitespace)
         (when (>= pos len) (return))
         (let ((c (char line pos)))
           (cond
             ((or (char= c #\") (char= c #\')) (read-string))
             ((and (char= c #\-) (< (1+ pos) len) (char= (char line (1+ pos)) #\-))
              (loop while (and (< pos len) (not (char= (char line pos) #\Newline)))
                    do (incf pos)))
             ((digit-char-p c) (read-number))
             ((alpha-char-p c) (read-identifier))
             ((char= c #\_) (read-identifier))
             (t (read-operator)))))
      (nreverse result))))

(defun lua-lex-source (source)
  "Lex complete Lua SOURCE string into token stream."
  (let ((all-tokens '()))
    (dolist (line (split-sequence:split-sequence #\Newline source))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\; (char trimmed 0))
                    (and (>= (length trimmed) 2) (char= (char trimmed 0) #\-) (char= (char trimmed 1) #\-)))
          (setf all-tokens (append all-tokens (lua-lex-line trimmed))))))
    all-tokens))

(defun tokenize-lua (source)
  "Alias for lua-lex-source — canonical name expected by lua-parser.lisp."
  (lua-lex-source source))

(defun lua-lex-token ()
  "Read next token from *standard-input*."
  (declare (special *lua-lex-token-buffer*))
  (cond
    ((and (boundp '*lua-lex-token-buffer*) *lua-lex-token-buffer*)
     (prog1 (first *lua-lex-token-buffer*)
       (setf *lua-lex-token-buffer* (rest *lua-lex-token-buffer*))))
    (t
     (let ((line (read-line *standard-input* nil nil)))
       (when line
         (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
           (when (and (plusp (length trimmed)) (char/= #\; (char trimmed 0)))
             (setf *lua-lex-token-buffer* (lua-lex-line trimmed))
             (lua-lex-token))))))))

(defun lua-token-list ()
  "Return a thunk that reads tokens from *standard-input* using lua-lex-token."
  (lambda ()
    (let ((token (lua-lex-token)))
      (when token
        (cons token (funcall (lua-token-list)))))))

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
                   "dialogue" "print" "input" "dialog"
                   "goto" "self" "debug_break" "log_fault"
                   "blt" "string")
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
