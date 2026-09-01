;; src/frontend-forth/forth-parser.lisp — Forth → EIGHTBOL AST parser
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;
;; Recursive-descent Forth parser that produces canonical EIGHTBOL AST.
;; Distinguishes interpretation mode (top-level, immediate) from
;; compilation mode (: ... ;, deferred). All front-ends must produce
;; the same AST consumed by all back-ends.
(in-package :eightbol)

;;; Word classification — built-in roles

(declaim (ftype (function (string) keyword) forth-classify-word))

(defun forth-classify-word (normalized)
  "Classify NORMALIZED Forth word into a role keyword.
Returns: :interpreted-prim, :compiled-prim, :control, :immediate,
:colon-def, :semicolon, :variable, :constant, :unknown"
  (let ((role (gethash normalized *forth-core-words-alist*)))
    (if role
        ;; Look up the role from the core-word role table
        (cdr (assoc normalized *forth-word-roles* :test #'string-equal))
        :unknown)))

(defvar *forth-word-roles*
  '(;; Compilation-mode primitives — compiled into word body
    ("DUP"     . :compiled-prim)
    ("SWAP"    . :compiled-prim)
    ("DROP"    . :compiled-prim)
    ("OVER"    . :compiled-prim)
    ("ROT"     . :compiled-prim)
    ("NIP"     . :compiled-prim)
    ("TUCK"    . :compiled-prim)
    ("2DUP"    . :compiled-prim)
    ("2SWAP"   . :compiled-prim)
    ("2DROP"   . :compiled-prim)
    ("PICK"    . :compiled-prim)
    ("ROLL"    . :compiled-prim)
    ("+"       . :compiled-prim)
    ("-"       . :compiled-prim)
    ("*"       . :compiled-prim)
    ("/"       . :compiled-prim)
    ("MOD"     . :compiled-prim)
    ("ABS"     . :compiled-prim)
    ("NEGATE"  . :compiled-prim)
    ("AND"     . :compiled-prim)
    ("OR"      . :compiled-prim)
    ("XOR"     . :compiled-prim)
    ("NOT"     . :compiled-prim)
    ("<<"      . :compiled-prim)
    (">>"      . :compiled-prim)
    ("="       . :compiled-prim)
    ("<"       . :compiled-prim)
    (">"       . :compiled-prim)
    ("<="      . :compiled-prim)
    (">="      . :compiled-prim)
    ("<>"      . :compiled-prim)
    ("0="      . :compiled-prim)
    ("0<"      . :compiled-prim)
    ("0>"      . :compiled-prim)
    ("@"       . :compiled-prim)
    ("!"       . :compiled-prim)
    ("+!"      . :compiled-prim)
    ("C@"      . :compiled-prim)
    ("C!"      . :compiled-prim)
    (">R"      . :compiled-prim)
    ("R>"      . :compiled-prim)
    ("R@"      . :compiled-prim)
    ;; Interpretation-mode primitives — executed immediately at top level
    ("."       . :interpreted-prim)
    ("EMIT"    . :interpreted-prim)
    ("CR"      . :interpreted-prim)
    ("SPACE"   . :interpreted-prim)
    ("SPACES"  . :interpreted-prim)
    ("PAGE"    . :interpreted-prim)
    ("KEY"     . :interpreted-prim)
    ("ACCEPT"  . :interpreted-prim)
    ("EXECUTE" . :interpreted-prim)
    ;; Control flow — only valid inside compilation mode
    ("IF"      . :control)
    ("ELSE"    . :control)
    ("THEN"    . :control)
    ("BEGIN"   . :control)
    ("UNTIL"   . :control)
    ("WHILE"   . :control)
    ("REPEAT"  . :control)
    ("DO"      . :control)
    ("LOOP"    . :control)
    ("+LOOP"   . :control)
    ("LEAVE"   . :control)
    ;; Definition scope
    (":"       . :colon-def)
    (";"       . :semicolon)
    ("VARIABLE" . :variable)
    ("CONSTANT" . :constant)
    ;; Exit / flow control
    ("EXIT"    . :compiled-prim)
    ("BYE"     . :interpreted-prim)
    ("CALL"    . :compiled-prim)
    ("GOBACK"  . :compiled-prim)
    ;; Include
    ("INCLUDE" . :interpreted-prim))
  "Maps normalized Forth word names to their compilation role.")

;;; Parser state

(defstruct forth-parse-state
  "Recursive-descent parser state for Forth source."
  (tokens nil :type list)
  (pos 0 :type fixnum)
  (vocab (make-hash-table :test #'equal) :type hash-table)
  (compiling nil :type boolean)
  (def-name nil :type (or null string))
  (depth 0 :type fixnum))

(defun state-peek (state)
  "Return current token or NIL at end."
  (when (< (forth-parse-state-pos state)
           (length (forth-parse-state-tokens state)))
    (nth (forth-parse-state-pos state)
         (forth-parse-state-tokens state))))

(defun state-next (state)
  "Consume and return current token, or NIL at end."
  (let ((tok (state-peek state)))
    (when tok
      (incf (forth-parse-state-pos state)))
    tok))

(defun state-at-end-p (state)
  (>= (forth-parse-state-pos state)
      (length (forth-parse-state-tokens state))))

(defun token-type (token) (car token))
(defun token-value (token) (cdr token))

(defun classify-token (token state)
  "Classify TOKEN: returns role keyword or :unknown/:number/:string/:hex-number etc."
  (let ((type (token-type token))
        (val  (token-value token)))
    (cond
      ((member type '(:number :hex-number :octal-number :binary-number :dword-literal))
       type)
      ((eq type :string) :string)
      ((eq type :word)
       (let ((norm (forth-normalize-identifier val)))
         (cond
           ;; User-defined words from vocabulary
           ((gethash norm (forth-parse-state-vocab state)) :user-word)
           ;; Known built-in
           (t (let ((role (cdr (assoc norm *forth-word-roles*
                                       :test #'string-equal))))
                (or role :unknown))))))
      (t :unknown))))

;;; Token list flattening

(defun flatten-tokens (tokens-per-line)
  "Flatten TOKENS-PER-LINE into a single token list, inserting :newline
sentinels at line boundaries so the parser can track source locations."
  (let ((result '()))
    (dolist (line-tokens tokens-per-line)
      (dolist (tok line-tokens)
        (push tok result))
      (push (cons :newline nil) result))
    (nreverse result)))

;;; Colon definition parser  (: name ... ;)

(defun parse-colon-definition (state)
  "Parse : name body-statement* ;
Registers NAME in vocab and returns (:forth-word-def :name N :body (...))."
  (state-next state)                    ; consume :
  (let ((name-tok (state-next state)))
    (unless (and name-tok (eq (token-type name-tok) :word))
      (error "FORTH: expected word name after :"))
    (let* ((raw-name (token-value name-tok))
           (norm (forth-normalize-identifier raw-name)))
      ;; Register in vocab NOW so recursive references resolve
      (setf (gethash norm (forth-parse-state-vocab state)) t)
      ;; Enter compilation mode
      (let ((old-compiling (forth-parse-state-compiling state))
            (old-def-name  (forth-parse-state-def-name state))
            (old-depth     (forth-parse-state-depth state)))
        (setf (forth-parse-state-compiling state) t
              (forth-parse-state-def-name state)  norm
              (forth-parse-state-depth state)     (1+ old-depth))
        ;; Parse body until matching ;
        (let ((body (parse-compilation-body state)))
          ;; Restore outer mode
          (setf (forth-parse-state-compiling state) old-compiling
                (forth-parse-state-def-name state)  old-def-name
                (forth-parse-state-depth state)     old-depth)
          (list (list :forth-word-def :name norm :body body)))))))

(defun parse-compilation-body (state)
  "Parse tokens in compilation mode until the matching ;.
Returns a list of canonical AST nodes for the word body."
  (let ((stmts '()))
    (loop for tok = (state-peek state)
          while tok
          do (let ((role (classify-token tok state)))
               (case role
                 (:semicolon
                  (state-next state)   ; consume ;
                  (return (nreverse stmts)))
                 (:colon-def
                  ;; Nested : — parse as sub-definition inside body
                  (push (car (parse-colon-definition state)) stmts))
                 (:control
                  (let ((node (parse-control-flow state)))
                    (when node (push node stmts))))
                 (:number :hex-number :octal-number :binary-number :dword-literal
                  (state-next state)
                  (push (list :forth-push :value (token-value tok)) stmts))
                 (:string
                  (state-next state)
                  (push (list :forth-push :value (token-value tok)) stmts))
                 (:variable
                  (state-next state)
                  (let ((n-tok (state-next state)))
                    (if (and n-tok (eq (token-type n-tok) :word))
                        (let ((nm (forth-normalize-identifier (token-value n-tok))))
                          (setf (gethash nm (forth-parse-state-vocab state)) t)
                          (push (list :forth-var-def :name nm) stmts))
                        (push (list :forth-error :message "VARIABLE missing name") stmts))))
                 (:constant
                  (state-next state)
                  (let ((n-tok (state-next state)))
                    (if (and n-tok (eq (token-type n-tok) :word))
                        (let ((nm (forth-normalize-identifier (token-value n-tok))))
                          (setf (gethash nm (forth-parse-state-vocab state)) t)
                          (push (list :forth-const-def :name nm) stmts))
                        (push (list :forth-error :message "CONSTANT missing name") stmts))))
                 (:user-word
                  (state-next state)
                  (push (list :call :target (forth-normalize-identifier
                                             (token-value tok)))
                        stmts))
                 (:compiled-prim
                  (state-next state)
                  (push (list :forth-primitive :name role) stmts))
                 (:interpreted-prim
                  ;; In compilation mode, immediate words still execute now
                  ;; (they are "immediate" words in ANS Forth terminology)
                  (state-next state)
                  (push (list :forth-primitive :name role) stmts))
                 (:unknown
                  (state-next state)
                  (push (list :call :target (forth-normalize-identifier
                                             (token-value tok)))
                        stmts))
                 (:newline
                  (state-next state))
                 (otherwise
                  (state-next state)))))
    (nreverse stmts)))

;;; Control-flow parsing (IF/THEN/ELSE, BEGIN/UNTIL/WHILE/REPEAT, DO/LOOP)

(defun parse-control-flow (state)
  "Parse a control-flow construct and return a canonical AST node."
  (let ((tok (state-peek state)))
    (unless tok (return-from parse-control-flow nil))
    (ecase (classify-token tok state)
      (:if    (parse-if state))
      (:begin (parse-begin state))
      (:do    (parse-do-loop state)))))

(defun parse-if (state)
  "Parse IF [ELSE] THEN.  Condition is the top-of-stack value (already there).
Returns (:if :condition (:forth-test) :then (...) :else (...))."
  (state-next state)                    ; consume IF
  (let ((then-body '())
        (else-body '())
        (in-else nil))
    (loop for tok = (state-peek state) while tok
          do (let ((role (classify-token tok state)))
               (case role
                 (:then
                  (state-next state)
                  (return))
                 (:else
                  (state-next state)
                  (setf in-else t))
                 (:newline
                  (state-next state))
                 (:control
                  (let ((node (parse-control-flow state)))
                    (when node
                      (if in-else
                          (push node else-body)
                          (push node then-body)))))
                 (otherwise
                  (let ((stmt (parse-compiled-statement state)))
                    (when stmt
                      (if in-else
                          (push stmt else-body)
                          (push stmt then-body))))))))
    (make-if-node '(:forth-test)
                  (nreverse then-body)
                  (nreverse else-body))))

(defun parse-begin (state)
  "Parse BEGIN [condition] UNTIL  or  BEGIN [condition] WHILE body REPEAT.
Forth semantics:
  BEGIN ... cond UNTIL   → loops while cond is FALSE, exits when TRUE.
  BEGIN ... cond WHILE body REPEAT → loops while cond is TRUE.
Returns (:perform :procedure nil :until COND :body (...))."
  (state-next state)                    ; consume BEGIN
  (let ((pre-body '()))
    (loop for tok = (state-peek state) while tok
          do (let ((role (classify-token tok state)))
               (case role
                 (:until
                  (state-next state)    ; consume UNTIL
                  ;; BEGIN ... cond UNTIL
                  ;; Pre-body includes the condition computation at the end.
                  ;; The last statement is the flag to test: exit when TRUE.
                  ;; So :until = the last statement, body = everything before it.
                  (let ((cond-expr (car (last pre-body)))
                        (body-stmts (butlast pre-body)))
                    (return (make-perform-node
                             nil
                             :until (or cond-expr '(:forth-test))
                             :body body-stmts))))
                 (:while
                  (state-next state)   ; consume WHILE
                  ;; BEGIN ... cond WHILE ... REPEAT
                  ;; Condition is on the stack; WHILE exits when FALSE.
                  ;; `:until` = exit condition = NOT cond.
                  (let ((cond-expr (car (last pre-body)))
                        (body-stmts '()))
                    ;; Parse body until REPEAT
                    (loop for ctok = (state-peek state) while ctok
                          do (if (eq (classify-token ctok state) :repeat)
                                 (progn (state-next state) (return))
                                 (let ((stmt (parse-compiled-statement state)))
                                   (when stmt (push stmt body-stmts)))))
                    ;; Exit when cond is FALSE = (NOT cond)
                    (return (make-perform-node
                             nil
                             :until (list :not (or cond-expr '(:forth-test)))
                             :body (nreverse body-stmts)))))
                 (:repeat
                  ;; Bare BEGIN ... REPEAT — infinite loop
                  (state-next state)
                  (return (make-perform-node nil :until :self
                                             :body (nreverse pre-body))))
                 (:newline
                  (state-next state))
                 (:control
                  (let ((node (parse-control-flow state)))
                    (when node (push node pre-body))))
                 (otherwise
                  (let ((stmt (parse-compiled-statement state)))
                    (when stmt (push stmt pre-body)))))))
    ;; Fallback if no UNTIL/WHILE/REPEAT found — infinite loop
    (make-perform-node nil :until '(:forth-test) :body (nreverse pre-body))))

(defun parse-do-loop (state)
  "Parse DO [index limit] ... LOOP.  Bounds are on the stack before DO.
Returns (:perform :procedure nil :varying I :from 0 :by 1 :body (...))."
  (state-next state)                    ; consume DO
  (let ((body '()))
    (loop for tok = (state-peek state) while tok
          do (let ((role (classify-token tok state)))
               (case role
                 ((:loop :plus-loop)
                  (state-next state)
                  (return))
                 (:newline
                  (state-next state))
                 (:control
                  (let ((node (parse-control-flow state)))
                    (when node (push node body))))
                 (otherwise
                  (let ((stmt (parse-compiled-statement state)))
                    (when stmt (push stmt body)))))))
    (make-perform-node nil
                       :varying "I" :from 0 :by 1
                       :body (nreverse body))))

;;; Compiled-mode statement (inside : ... ;)

(defun parse-compiled-statement (state)
  "Parse a single compiled-mode statement and return one AST node."
  (let ((tok (state-peek state)))
    (unless tok (return-from parse-compiled-statement nil))
    (let ((role (classify-token tok state)))
      (case role
        ((:number :hex-number :octal-number :binary-number :dword-literal)
         (state-next state)
         (list :forth-push :value (token-value tok)))
        (:string
         (state-next state)
         (list :forth-push :value (token-value tok)))
        (:user-word
         (state-next state)
         (list :call :target (forth-normalize-identifier (token-value tok))))
        (:compiled-prim
         (state-next state)
         (list :forth-primitive :name role))
        (:interpreted-prim
         (state-next state)
         (list :forth-primitive :name role))
        (:unknown
         (state-next state)
         (list :call :target (forth-normalize-identifier (token-value tok))))
        (:newline
         (state-next state)
         nil)
        (:control
         (parse-control-flow state))
        (otherwise
         (state-next state)
         nil)))))

;;; Interpretation-mode statement (top-level)

(defun parse-interpreted-statement (state)
  "Parse a single top-level (interpretation-mode) statement.
At the top level, colon definitions and INCLUDE are processed; everything
else generates :call nodes."
  (let ((tok (state-peek state)))
    (unless tok (return-from parse-interpreted-statement nil))
    (let ((role (classify-token tok state)))
      (case role
        (:colon-def
         (parse-colon-definition state))
        (:semicolon
         (state-next state)             ; stray ; outside definition — skip
         nil)
        (:variable
         (state-next state)
         (let ((n-tok (state-next state)))
           (if (and n-tok (eq (token-type n-tok) :word))
               (let ((nm (forth-normalize-identifier (token-value n-tok))))
                 (setf (gethash nm (forth-parse-state-vocab state)) t)
                 (list (list :forth-var-def :name nm)))
               (list (list :forth-error :message "VARIABLE missing name")))))
        (:constant
         (state-next state)
         (let ((n-tok (state-next state)))
           (if (and n-tok (eq (token-type n-tok) :word))
               (let ((nm (forth-normalize-identifier (token-value n-tok))))
                 (setf (gethash nm (forth-parse-state-vocab state)) t)
                 (list (list :forth-const-def :name nm)))
               (list (list :forth-error :message "CONSTANT missing name")))))
        (:interpreted-prim
         (state-next state)
         (list (list :forth-primitive :name role)))
        ((:number :hex-number :octal-number :binary-number :dword-literal)
         (state-next state)
         (list (list :forth-push :value (token-value tok))))
        (:string
         (state-next state)
         (list (list :forth-push :value (token-value tok))))
        (:user-word
         (state-next state)
         (list (list :call :target (forth-normalize-identifier (token-value tok)))))
        (:unknown
         (state-next state)
         (list (list :call :target (forth-normalize-identifier (token-value tok)))))
        (:newline
         (state-next state)
         nil)
        (otherwise
         (state-next state)
         nil)))))

;;; Top-level program parsing

(defun forth-parse-tokens (tokens-per-line &optional (program-name "Script"))
  "Parse TOKENS-PER-LINE into a canonical EIGHTBOL AST.
Two passes: first scan for all colon definitions (populate vocab),
then parse bodies with full vocabulary available.

Returns (:program :class-id PROGRAM-NAME :methods (...))."
  (let* ((all-tokens (flatten-tokens tokens-per-line))
         (state (make-forth-parse-state :tokens all-tokens)))
    ;; ---- First pass: scan for : word names to populate vocab ----
    (let ((scan-pos 0))
      (loop for tok in all-tokens
            when (and (eq (car tok) :word)
                      (string= (cdr tok) ":"))
              do (let ((next (nth (1+ scan-pos) all-tokens)))
                   (when (and next (eq (car next) :word))
                     (setf (gethash (forth-normalize-identifier (cdr next))
                                    (forth-parse-state-vocab state))
                           t)))
            do (incf scan-pos)))
    ;; ---- Second pass: full parse ----
    (setf (forth-parse-state-pos state) 0)
    (let ((top-level '())
          (methods '()))
      (loop for tok = (state-peek state)
            while (and tok (not (state-at-end-p state)))
            do (let ((result (parse-interpreted-statement state)))
                 (dolist (node result)
                   (cond
                     ;; Colon definitions become separate :method nodes
                     ((and (listp node)
                           (eq (first node) :forth-word-def))
                      (let ((name (getf (rest node) :name))
                            (body (getf (rest node) :body)))
                        (push (make-method-node name :statements body)
                              methods)))
                     ;; Everything else accumulates in the top-level method
                     (t
                      (push node top-level))))))
      ;; Build program node
      (let ((all-methods (nreverse methods)))
        ;; Prepend the top-level interpretation method if non-empty
        (when top-level
          (setf all-methods
                (cons (make-method-node "TopLevel"
                                        :statements (nreverse top-level))
                      all-methods)))
        (make-program-node program-name :methods all-methods)))))

;;; High-level interface

(defun forth-compile-source (source-string &optional (program-name "Script"))
  "Compile Forth SOURCE-STRING to canonical EIGHTBOL AST.
Returns (:program :class-id PROGRAM-NAME :methods (...))."
  (let ((token-lists (forth-tokenize-source source-string)))
    (forth-parse-tokens token-lists program-name)))

(defun forth-compile-file (filename)
  "Compile a .forth file to canonical EIGHTBOL AST."
  (let* ((source (uiop:read-file-string filename))
         (program-name (pathname-name filename)))
    (forth-compile-source source program-name)))
