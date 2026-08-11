;; src/frontend-forth/parser.lisp — Forth → EIGHTBOL AST compiler
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Forth AST Node Constructors

(defun make-forth-push-literal (value)
  "Create a stack push for a literal VALUE.
Returns: (:forth-push-literal :value value)"
  (list :forth-push-literal :value value))

(defun make-forth-push-identifier (name)
  "Create a stack push for an identifier (variable fetch).
Returns: (:forth-push-identifier :name name)"
  (list :forth-push-identifier :name name))

(defun make-forth-stack-op (op-name &key args)
  "Create a stack operation node.
Returns: (:forth-stack-op :name op-name :args args)"
  (list :forth-stack-op :name op-name :args args))

(defun make-forth-arithmetic (op &key left right result-name)
  "Create an arithmetic operation node.
Returns: (:forth-arithmetic :op op :left left :right right :result-name result-name)"
  (list :forth-arithmetic :op op :left left :right right :result-name result-name))

(defun make-forth-comparison (op &key left right)
  "Create a comparison operation node.
Returns: (:forth-comparison :op op :left left :right right)"
  (list :forth-comparison :op op :left left :right right))

(defun make-forth-conditional (condition then-body else-body)
  "Create a conditional (IF/THEN/ELSE) node.
Returns: (:forth-conditional :condition condition :then then-body :else else-body)"
  (list :forth-conditional :condition condition :then then-body :else else-body))

(defun make-forth-loop (loop-type init-body condition-body body)
  "Create a loop node.
LOOP-TYPE: :begin-until, :begin-while, :do-loop
Returns: (:forth-loop :type loop-type :init init-body :condition condition-body :body body)"
  (list :forth-loop :type loop-type :init init-body :condition condition-body :body body))

(defun make-forth-variable-def (name &optional initial-value)
  "Create a variable definition node.
Returns: (:forth-variable :name name :initial-value initial-value)"
  (list :forth-variable :name name :initial-value initial-value))

(defun make-forth-fetch (address)
  "Create a memory fetch node (@).
Returns: (:forth-fetch :address address)"
  (list :forth-fetch :address address))

(defun make-forth-store (address value)
  "Create a memory store node (!).
Returns: (:forth-store :address address :value value)"
  (list :forth-store :address address :value value))

(defun make-forth-print-value ()
  "Create a print-top-of-stack node (.).
Returns: (:forth-print-value)"
  '(:forth-print-value))

(defun make-forth-print-string (string)
  "Create a print-string node (.\").
Returns: (:forth-print-string :string string)"
  (list :forth-print-string :string string))

(defun make-forth-print-char ()
  "Create an emit character from TOS node (EMIT).
Returns: (:forth-emit)"
  '(:forth-emit))

(defun make-forth-print-cr ()
  "Create a carriage return / newline node (CR).
Returns: (:forth-print-cr)"
  '(:forth-print-cr))

(defun make-forth-print-space ()
  "Create a space output node (SPACE or SPACES).
Returns: (:forth-print-space :count count-or-nil)"
  '(:forth-print-space))

(defun make-forth-input-key ()
  "Create a single-key input node (KEY).
Returns: (:forth-input-key)"
  '(:forth-input-key))

(defun make-forth-input-line (max-length)
  "Create a line input node (ACCEPT).
Returns: (:forth-input-line :max-length max-length)"
  (list :forth-input-line :max-length max-length))

(defun make-forth-dialogue (dialogue-type &key speaker text options)
  "Create a dialogue node for narrative output.
DIALOGUE-TYPE: :say, :narrate, :choice, :show, :hide, :fade, :music, :sound
Returns: (:forth-dialogue :type dialogue-type :speaker speaker :text text :options options)"
  (list :forth-dialogue :type dialogue-type :speaker speaker :text text :options options))

(defun make-forth-word-def (name params body)
  "Create a word (function) definition node.
Returns: (:forth-word-def :name name :params params :body body)"
  (list :forth-word-def :name name :params params :body body))

(defun make-forth-constant-def (name value)
  "Create a constant definition node.
Returns: (:forth-constant :name name :value value)"
  (list :forth-constant :name name :value value))

(defun make-forth-program (words)
  "Create a complete Forth program node.
Returns: (:forth-program :definitions words)"
  (list :forth-program :definitions words))

;;; Parser state and context

(defstruct forth-parse-context
  "Parser context for Forth compilation."
  (tokens-per-line '())           ; ((token token ...) ...)
  (current-line-index 0)             ; current line number
  (current-token-index 0)            ; current token in line
  (word-definitions (make-hash-table :test #'equal)) ; user-defined words
  (stack-depth 0))                 ; estimated stack depth

(defun forth-parse-tokens (tokens-per-line)
  "Parse tokenized Forth code into an AST.
INPUT:  tokens-per-line — output from forth-tokenize-source
OUTPUT: (:forth-program :definitions [...AST nodes...])"
  (let ((context (make-forth-parse-context :tokens-per-line tokens-per-line)))
    (let ((definitions '()))
      (loop while (< (forth-parse-context-current-line-index context)
                     (length tokens-per-line))
            do (let ((tokens (nth (forth-parse-context-current-line-index context)
                                  tokens-per-line)))
                 (when tokens
                   (setf (forth-parse-context-current-token-index context) 0)
                   (multiple-value-bind (statements consumed)
                       (forth-parse-line context tokens)
                     (dolist (stmt statements)
                       (push stmt definitions))))
                 (incf (forth-parse-context-current-line-index context))))
      (make-forth-program (nreverse definitions)))))

(defun forth-parse-line (context tokens)
  "Parse a single line of tokens.
Returns (values statements-list tokens-consumed)"
  (let ((statements '())
        (pos 0))
    (loop while (< pos (length tokens))
          do (let ((token (nth pos tokens)))
               (multiple-value-bind (stmt new-pos)
                   (forth-parse-token context token pos tokens)
                 (when stmt
                   (push stmt statements))
                 (setq pos new-pos))))
    (values (nreverse statements) pos)))

(defun forth-parse-token (context token index tokens)
  "Parse a single token in context.
Returns (values statement new-index) or (values nil new-index) if no statement."
  (destructuring-bind (type . value) token
    (cond
      ;; Word definitions
      ((eq type :word)
       (let ((normalized (forth-normalize-identifier value)))
         (cond
           ;; Word definition start
           ((string= normalized ":")
            (let ((name-token (nth (1+ index) tokens)))
              (destructuring-bind (name-type . name-val) name-token
                (when (eq name-type :word)
                  (let* ((body-start (+ index 2))
                         (body-end (position-if (lambda (token)
                                                  (and (eq (car token) :word)
                                                       (string= (forth-normalize-identifier (cdr token)) ";")))
                                                tokens :start body-start)))
                    (when body-end
                      (let ((body-tokens (subseq tokens body-start body-end)))
                        (values (make-forth-word-def name-val nil body-tokens)
                                (1+ body-end)))))))))
           ;; Print word
           ((string= normalized ".")
            (values (make-forth-print-value) (1+ index)))
           ;; Print string
           ((string= normalized ".\"")
            (when (< (1+ index) (length tokens))
              (let ((str-token (nth (1+ index) tokens)))
                (when (eq (car str-token) :string)
                  (values (make-forth-print-string (cdr str-token)) (+ index 2))))))
           ;; Stack operations
           ((string= normalized "DUP")
            (values (make-forth-stack-op :DUP) (1+ index)))
           ((string= normalized "SWAP")
            (values (make-forth-stack-op :SWAP) (1+ index)))
           ((string= normalized "DROP")
            (values (make-forth-stack-op :DROP) (1+ index)))
           ((string= normalized "OVER")
            (values (make-forth-stack-op :OVER) (1+ index)))
           ((string= normalized "ROT")
            (values (make-forth-stack-op :ROT) (1+ index)))
           ((string= normalized "TWODUP")
            (values (make-forth-stack-op :TWODUP) (1+ index)))
           ((string= normalized "TWOSWAP")
            (values (make-forth-stack-op :TWOSWAP) (1+ index)))
           ((string= normalized "TWODROP")
            (values (make-forth-stack-op :TWODROP) (1+ index)))
           ((string= normalized "NIP")
            (values (make-forth-stack-op :NIP) (1+ index)))
           ((string= normalized "TUCK")
            (values (make-forth-stack-op :TUCK) (1+ index)))
           ;; Arithmetic
           ((string= normalized "+")
            (values (make-forth-arithmetic :ADD) (1+ index)))
           ((string= normalized "-")
            (values (make-forth-arithmetic :SUB) (1+ index)))
           ((string= normalized "*")
            (values (make-forth-arithmetic :MUL) (1+ index)))
           ((string= normalized "/")
            (values (make-forth-arithmetic :DIV) (1+ index)))
           ((string= normalized "MOD")
            (values (make-forth-arithmetic :MOD) (1+ index)))
           ((string= normalized "ABS")
            (values (make-forth-stack-op :ABS) (1+ index)))
           ((string= normalized "NEGATE")
            (values (make-forth-stack-op :NEGATE) (1+ index)))
           ;; Bitwise
           ((string= normalized "AND")
            (values (make-forth-arithmetic :AND) (1+ index)))
           ((string= normalized "OR")
            (values (make-forth-arithmetic :OR) (1+ index)))
           ((string= normalized "XOR")
            (values (make-forth-arithmetic :XOR) (1+ index)))
           ((string= normalized "NOT")
            (values (make-forth-stack-op :NOT) (1+ index)))
           ((string= normalized "<<")
            (values (make-forth-arithmetic :LSHIFT) (1+ index)))
           ((string= normalized ">>")
            (values (make-forth-arithmetic :RSHIFT) (1+ index)))
           ;; Comparison
           ((string= normalized "=")
            (values (make-forth-comparison :EQ) (1+ index)))
           ((string= normalized "<")
            (values (make-forth-comparison :LT) (1+ index)))
           ((string= normalized ">")
            (values (make-forth-comparison :GT) (1+ index)))
           ((string= normalized "<=")
            (values (make-forth-comparison :LE) (1+ index)))
           ((string= normalized ">=")
            (values (make-forth-comparison :GE) (1+ index)))
           ((string= normalized "<>")
            (values (make-forth-comparison :NE) (1+ index)))
           ((string= normalized "0=")
            (values (make-forth-stack-op :ZEQUAL) (1+ index)))
           ((string= normalized "0<")
            (values (make-forth-stack-op :ZLT) (1+ index)))
           ((string= normalized "0>")
            (values (make-forth-stack-op :ZGT) (1+ index)))
           ;; Memory operations
           ((string= normalized "@")
            (values (make-forth-fetch nil) (1+ index)))
           ((string= normalized "!")
            (values (make-forth-store nil nil) (1+ index)))
           ((string= normalized "+!")
            (values (make-forth-stack-op :PLUSSTORE) (1+ index)))
           ((string= normalized "C@")
            (values (make-forth-stack-op :CFETCH) (1+ index)))
           ((string= normalized "C!")
            (values (make-forth-stack-op :CSTORE) (1+ index)))
           ;; Return stack
           ((string= normalized ">R")
            (values (make-forth-stack-op :TORET) (1+ index)))
           ((string= normalized "R>")
            (values (make-forth-stack-op :FROMRET) (1+ index)))
           ((string= normalized "R@")
            (values (make-forth-stack-op :RATFETCH) (1+ index)))
           ;; I/O and Output
           ((string= normalized "EMIT")
            (values (make-forth-print-char) (1+ index)))
           ((string= normalized "CR")
            (values (make-forth-print-cr) (1+ index)))
           ((string= normalized "SPACE")
            (values (make-forth-print-space) (1+ index)))
           ((string= normalized "SPACES")
            (values (make-forth-stack-op :SPACES) (1+ index)))
           ((string= normalized "PAGE")
            (values (make-forth-stack-op :PAGE) (1+ index)))
           ;; Input
           ((string= normalized "KEY")
            (values (make-forth-input-key) (1+ index)))
           ((string= normalized "ACCEPT")
            (values (make-forth-input-line 255) (1+ index)))
           ;; Variable definitions
           ((string= normalized "VARIABLE")
            (when (< (1+ index) (length tokens))
              (let ((var-token (nth (1+ index) tokens)))
                (when (eq (car var-token) :word)
                  (values (make-forth-variable-def (cdr var-token)) (+ index 2))))))
           ((string= normalized "CONSTANT")
            (when (< (1+ index) (length tokens))
              (let ((const-token (nth (1+ index) tokens)))
                (when (eq (car const-token) :word)
                  (values (make-forth-constant-def (cdr const-token) nil) (+ index 2))))))
           ;; Dialogue words
           ((or (string= normalized "SAY") (string= normalized "NARRATE"))
            (when (< (1+ index) (length tokens))
              (let ((text-token (nth (1+ index) tokens)))
                (when (eq (car text-token) :string)
                  (let ((dialogue-type (if (string= normalized "SAY") :say :narrate)))
                    (values (make-forth-dialogue dialogue-type :text (cdr text-token))
                            (+ index 2)))))))
           ((string= normalized "SHOW")
            (values (make-forth-dialogue :show) (1+ index)))
           ((string= normalized "HIDE")
            (values (make-forth-dialogue :hide) (1+ index)))
           ((string= normalized "FADE")
            (values (make-forth-dialogue :fade) (1+ index)))
           ((string= normalized "MUSIC")
            (values (make-forth-dialogue :music) (1+ index)))
           ((string= normalized "SOUND")
            (values (make-forth-dialogue :sound) (1+ index)))
           ;; Control flow
           ((string= normalized "EXIT")
            (values '(:forth-exit) (1+ index)))
           ((string= normalized "QUIT")
            (values '(:forth-quit) (1+ index)))
           ((string= normalized "BYE")
            (values '(:forth-bye) (1+ index)))
           ;; Default: user-defined or unknown
           (t (values (make-forth-push-identifier value) (1+ index))))))
      ;; Numeric literals
      ((eq type :number)
       (values (make-forth-push-literal value) (1+ index)))
      ((eq type :hex-number)
       (values (make-forth-push-literal value) (1+ index)))
      ((eq type :octal-number)
       (values (make-forth-push-literal value) (1+ index)))
      ((eq type :binary-number)
       (values (make-forth-push-literal value) (1+ index)))
      ((eq type :dword-literal)
       (values (make-forth-push-literal value) (1+ index)))
      ;; String literals
      ((eq type :string)
       (values (make-forth-push-literal value) (1+ index)))
      ;; Parentheses (grouping)
      ((eq type :lparen)
       (values nil (1+ index)))
      ((eq type :rparen)
       (values nil (1+ index)))
      ;; Unknown
      (t (values nil (1+ index))))))

;;; High-level interface

(defun forth-compile-source (source-string)
  "Compile Forth source code to EIGHTBOL AST.
INPUT:  source-string — Forth source code
OUTPUT: (:forth-program :definitions [...])
        or error if parsing fails"
  (let ((token-lists (forth-tokenize-source source-string)))
    (forth-parse-tokens token-lists)))

(defun forth-compile-file (filename)
  "Compile a .forth file to AST.
INPUT:  filename — path to .forth source file
OUTPUT: (:forth-program :definitions [...]) or error"
  (let ((source (read-file-into-string filename)))
    (forth-compile-source source)))
