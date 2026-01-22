;; src/parser.lisp
(in-package :eightbol)

;;; ---------------------------------------------------------------
;;; Source-location tracking for error reporting
;;; ---------------------------------------------------------------

(defvar *current-token-location* nil
  "Plist (:source-file :source-line :source-sequence) of the token most
recently consumed by the YACC lexer thunk.  Set by STREAM-CODE.")

;;; source-error is defined in conditions.lisp

;;; ---------------------------------------------------------------
;;; Token list — every terminal used anywhere in the grammar must
;;; appear here so the lexer produces the right token type.
;;; ---------------------------------------------------------------
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun token-list ()
    '(|(| |)| |:| |,| + - * / |.| /= < <= = > >= ≠ ≤ ≥
      add address after all alphabet alphabetic also and any are
      argument ascii atascii at author
      bank before binary bit-and bit-not bit-or bit-xor blank break by
      call cancel characters class class-id comment compute
      configuration constant continue converting copy count
      data date date-compiled date-written debug decimal
      delimited delimiter depending display divide division down
      else end end-evaluate end-if end-method end-perform end-search
      environment equal evaluate exit external
      false fault filler for format from function
      giving go goback greater
      id identification if imperative in indent inherits installation
      inspect into invoke is
      just justified
      leading length less library linkage log
      method method-id minifont
      move multiply native negative next not null nulls numeric
      object object-computer occurs of offset on or other outdent
      packed-decimal petscii pic picture pointer positive process
      procedure procedure-pointer program
      redefines reference remainder renames replacing returning right run
      search section security self sentence sentences service set shift-left shift-right sign signed size
      subtract
      source-computer special-names stop string super symbol
      sync synchronized
      tallying test than then through thru times title to trailing true
      unicode unsigned until up usage
      value values varying
      when with working-storage
      zero zeroes)))

;;; ---------------------------------------------------------------
;;; Parser action functions
;;; ---------------------------------------------------------------

(defun parse/class-file (c1* id c2*
                         env c2a*
                         class-data c3*
                         _object _object_dot c4*
                         data c5*
                         proc c6*
                         _end_object _object_end _end_object_dot c6a*
                         _end _class end-class-name _dot c7*)
  (declare (ignore _end _class _dot _object _object_dot
                   _end_object _object_end _end_object_dot))
  (let* ((class-id (getf id :class-id)))
    (assert (string-equal class-id end-class-name)
            () "Class-ID mismatch: ~a vs ~a" class-id end-class-name)
    (list :program
          :comments (list :before-id c1*
                          :before-end c2*
                          :before-class-data c2a*
                          :before-object c3*
                          :before-object-data c4*
                          :before-procedure c5*
                          :before-end-object c6*
                          :before-end-class c6a*
                          :trailer c7*)
          :class-id class-id
          :identification id
          :environment env
          :working-storage (ensure-list class-data)
          :data (ensure-list data)
          :methods (or (extract-method-list proc) '()))))

(defun extract-method-list (proc)
  "Extract flat list of method AST nodes from procedure-division result."
  (when proc
    (if (and (listp proc) (eq (first proc) :methods))
        (rest proc)
        proc)))

(defun parse/method-defs-append (defs method)
  "Append one method definition to the method definitions list."
  (append defs (list method)))

(defun parse/stmt-sequence-append (seq item)
  "Build a flat list of statements by appending one item to the sequence."
  (append (ensure-list seq) (list item)))

(defun parse/stmt-item-with-dot (stmt _dot)
  "stmt-item: statement followed by period — discard the period."
  (declare (ignore _dot))
  stmt)

(defun parse/eightbol-program (struct _comments)
  "Top-level program rule: ignore trailing comments, return the program plist."
  (declare (ignore _comments))
  struct)

(defun parse/class-id (_id _div _stop1
                       _cid  _stop2 class-id _stop3
                       body)
  (declare (ignore _id _div _cid _stop1 _stop2 _stop3))
  (append (ensure-list body) (list :class-id class-id)))

(defun parse/id-field (field-name _stop field-value _stop2)
  (declare (ignore _stop _stop2))
  (list (intern (string-upcase field-name) :keyword)
        (princ-to-string field-value)))

(defun parse/inherits (_inherits _dot parent _dot2)
  (declare (ignore _inherits _dot _dot2))
  (list :inherits (princ-to-string parent)))

(defun parse/method-block (id-div proc _end _method name _dot)
  "Action for: method-identification-division procedure-statements END METHOD method-name |.|"
  (declare (ignore id-div _end _method _dot))
  (let ((statements (if (and (listp proc) (eq (first proc) :proc-stmts))
                        (rest proc)
                        (ensure-list proc))))
    (list :method
          :method-id name
          :statements statements)))

(defun parse/method-block-em (id-div proc _end-method name _dot)
  "Action for: method-identification-division procedure-statements END-METHOD method-name |.|"
  (declare (ignore id-div _end-method _dot))
  (let ((statements (if (and (listp proc) (eq (first proc) :proc-stmts))
                        (rest proc)
                        (ensure-list proc))))
    (list :method
          :method-id name
          :statements statements)))

(defun parse/method-id (_id _div _stop1
                        _mid _stop2 method-id _stop3
                        body)
  (declare (ignore _id _div _stop1 _mid _stop2 _stop3))
  (cons body (list :method-id method-id)))

(defun parse/object-procedure-division (_proc _div _dot methods)
  (declare (ignore _proc _div _dot))
  (cons :methods (ensure-list methods)))

(defun parse/procedure-statements (_proc _div _dot stmts)
  (declare (ignore _proc _div _dot))
  (cons :proc-stmts (ensure-list stmts)))

(defun parse/blank-when-zero ()
  (list :blank-when-zero t))

(defun parse/occurs (_occurs expression &optional _times)
  (declare (ignore _occurs _times))
  (list :occurs expression))

(defun parse/occurs-depending (_occurs min _to max &optional _times _depending _on dep-name)
  "OCCURS min TO max [TIMES] DEPENDING ON dep-name."
  (declare (ignore _occurs _to _times _depending _on))
  (list :occurs (list :min min :max max :depending-on dep-name)))

(defun parse/identifier-subscript (rp-val sub lp-val name)
  "Action for: data-name |(| subscript |)| — subscripted identifier.
   YACC passes vals in reverse stack order: (|)| subscript |(| data-name)."
  (declare (ignore rp-val lp-val))
  (list :subscript name sub))

(defun parse/picture-sequence-repeat (pic-seq _lp n _rp)
  "Action for: picture-sequence |(| number |)| — e.g. X(64) for 64 X characters."
  (declare (ignore _lp _rp))
  (format nil "~a(~a)" (if (stringp pic-seq) pic-seq (princ-to-string pic-seq)) n))

(defun parse/move (_move from _to identifier)
  (declare (ignore _move _to))
  (list :move :from from :to identifier))

(defun parse/add-to (_add expr _to identifier)
  (declare (ignore _add _to))
  (list :add :from expr :to identifier))

(defun parse/add-giving (_add expr1 _to expr2 _giving identifier)
  (declare (ignore _add _to _giving))
  (list :add :from expr1 :to expr2 :giving identifier))

(defun parse/subtract-from (_sub expr _from identifier)
  (declare (ignore _sub _from))
  (list :subtract :from expr :from-target identifier))

(defun parse/subtract-giving (_sub expr1 _from expr2 _giving identifier)
  (declare (ignore _sub _from _giving))
  (list :subtract :from expr1 :from-target expr2 :giving identifier))

(defun parse/compute-eq (_compute identifier _eq expr)
  (declare (ignore _compute _eq))
  (list :compute :target identifier :expression expr))

(defun parse/expr-add (e1 _op e2)
  (declare (ignore _op))
  (list :add-expr e1 e2))

(defun parse/expr-subtract (e1 _op e2)
  (declare (ignore _op))
  (list :subtract-expr e1 e2))

(defun parse/expr-multiply (e1 _op e2)
  (declare (ignore _op))
  (list :multiply-expr e1 e2))

(defun parse/expr-divide (e1 _op e2)
  (declare (ignore _op))
  (list :divide-expr e1 e2))

(defun parse/shift-left (expr _op n)
  (declare (ignore _op))
  (list :shift-left expr n))

(defun parse/shift-right (expr _op n)
  (declare (ignore _op))
  (list :shift-right expr n))

(defun parse/bit-and (e1 _op e2)
  (declare (ignore _op))
  (list :bit-and e1 e2))

(defun parse/cond-and (cond1 _op cond2)
  (declare (ignore _op))
  (list :and cond1 cond2))

(defun parse/cond-or (cond1 _op cond2)
  (declare (ignore _op))
  (list :or cond1 cond2))

(defun parse/cond-is-null (expr _is _null)
  (declare (ignore _is _null))
  (list '= expr :null))

(defun parse/cond-is-not-null (expr _is _not _null)
  (declare (ignore _is _not _null))
  (list :not (list '= expr :null)))

(defun parse/cond-eq (expr1 _op expr2 &optional expr3)
  (declare (ignore _op))
  (list '= expr1 (or expr3 expr2)))

(defun parse/bit-or (e1 _op e2)
  (declare (ignore _op))
  (list :bit-or e1 e2))

(defun parse/bit-xor (e1 _op e2)
  (declare (ignore _op))
  (list :bit-xor e1 e2))

(defun parse/bit-not (_bit_not expr)
  (declare (ignore _bit_not))
  (list :bit-not expr))

(defun parse/invoke (_invoke obj method)
  (declare (ignore _invoke))
  (list :invoke :object obj :method method))

(defun parse/invoke-returning (_invoke obj method _returning result)
  (declare (ignore _invoke _returning))
  (list :invoke :object obj :method method :returning result))

(defun parse/call (_call target)
  (declare (ignore _call))
  (list :call :target target :bank nil))

(defun parse/call-service (_call _service target)
  "CALL SERVICE target.  — service-dispatch call; bank must be specified at link time."
  (declare (ignore _call _service))
  (list :call :service target :bank nil))

(defun parse/call-in-service (_call target _in _service bank)
  "CALL target IN SERVICE bank.  — service-dispatch call with explicit bank."
  (declare (ignore _call _in _service))
  (list :call :service target :bank bank))

(defun parse/call-in-bank (_call target _in _bank bank)
  "CALL target IN BANK bank.  — far call to an explicit bank."
  (declare (ignore _call _in _bank))
  (list :call :target target :bank bank))

(defun parse/call-in-library (_call target _in _library _name)
  "CALL target IN LIBRARY name.
Library routines live in LastBank which is always resident at $c000.
Generates a near jsr (no bank switch) regardless of the library name."
  (declare (ignore _call _in _library _name))
  (list :call :target target :bank nil :library t))

(defun parse/if-then (_if condition _then stmts _end_if)
  (declare (ignore _if _then _end_if))
  (list :if :condition condition :then (or stmts '()) :else '()))

(defun parse/if-then-else (_if condition _then then-stmts _else else-stmts _end_if)
  (declare (ignore _if _then _else _end_if))
  (list :if :condition condition
        :then (or then-stmts '())
        :else (or else-stmts '())))

(defun parse/goback () (list :goback))
(defun parse/exit-method (_exit _method) (declare (ignore _exit _method)) (list :exit-method))
(defun parse/exit-program (_exit _program) (declare (ignore _exit _program)) (list :exit-program))

(defun parse/log-fault (_log _fault code)
  (declare (ignore _log _fault))
  (list :log-fault :code code))

(defun parse/debug-break (_debug _break code)
  (declare (ignore _debug _break))
  (list :debug-break :code code))

(defun parse/copy (_copy name _dot)
  (declare (ignore _copy _dot))
  (list :copy :name name))

(defun parse/copy-of (_copy name _of library _dot)
  (declare (ignore _copy _of _dot))
  (list :copy :name name :library library))

(defun parse/copy-in (_copy name _in library _dot)
  (declare (ignore _copy _in _dot))
  (list :copy :name name :library library))

(defun parse/perform-proc (_perform name)
  (declare (ignore _perform))
  (list :perform :procedure name))

(defun parse/perform-times (_perform name expr _times)
  (declare (ignore _perform _times))
  (list :perform :procedure name :times expr))

(defun parse/perform-until (_perform name _until cond)
  (declare (ignore _perform _until))
  (list :perform :procedure name :until cond))

(defun parse/perform-varying (_perform name _varying id _from start _by step _until cond)
  (declare (ignore _perform _varying _from _by _until))
  (list :perform :procedure name :varying id :from start :by step :until cond))

(defun parse/set-to (_set identifier _to expr)
  (declare (ignore _set _to))
  (list :set :target identifier :value expr))

(defun parse/stop-run (_stop _run)
  (declare (ignore _stop _run))
  (list :stop-run))

;;; ---------------------------------------------------------------
;;; Unsupported-statement: signal compile-time error
;;;
;;; Statement forms that parse but are not implemented signal
;;; EIGHTBOL-SOURCE-ERROR at compile time.
;;;
;;; INSPECT — all 3 forms unsupported:
;;;   • INSPECT id TALLYING expr FOR CHARACTERS
;;;   • INSPECT id CONVERTING expr TO expr
;;;   • INSPECT id REPLACING CHARACTERS BY expr
;;;
;;; GOTO — all 4 forms unsupported:
;;;   • GO TO procedure-name
;;;   • GO procedure-name
;;;   • GO TO procedure-name DEPENDING ON expression
;;;   • GO procedure-name DEPENDING ON expression
;;;
;;; EVALUATE — unsupported (entire statement):
;;;   • EVALUATE subject WHEN phrases stmts [WHEN OTHER stmts] [END-EVALUATE]
;;;   All eval-subject, when-clause, and evaluate-phrases variants.
;;;
;;; SET — implemented: SET identifier TO expression only.
;;;   Unsupported forms:
;;;   • SET identifier UP BY expression
;;;   • SET identifier DOWN BY expression
;;;   • SET condition-name TO TRUE
;;;   • SET identifier TO ADDRESS OF identifier
;;;   • SET identifier TO NULL
;;;   • SET identifier TO NULLS
;;;   • SET identifier TO SELF
;;; ---------------------------------------------------------------

(defun unsupported-statement (message)
  "Signal EIGHTBOL-SOURCE-ERROR for an unsupported statement at current token location."
  (error 'source-error
         :source-file     (getf *current-token-location* :source-file)
         :source-line     (getf *current-token-location* :source-line)
         :source-sequence (getf *current-token-location* :source-sequence)
         :message         message))

;;; DIVIDE — all 5 forms unsupported
(defun parse/divide-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "DIVIDE is not supported"))

;;; MULTIPLY — both forms unsupported
(defun parse/multiply-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "MULTIPLY is not supported"))

;;; STRING — BLT form (DELIMITED BY SIZE) supported; character delimiter unsupported
(defun parse/string-blt (_str source _del _by _size _into dest)
  (declare (ignore _str _del _by _size _into))
  (list :string-blt :source source :dest dest))

(defun parse/string-blt-length (_str source _del _by _size _into dest _len length)
  (declare (ignore _str _del _by _size _into _len))
  (list :string-blt :source source :dest dest :length length))

(defun parse/string-blt-or-unsupported (_str source _del _by delim _into dest)
  (declare (ignore _str _del _by _into))
  (if (or (eq delim 'eightbol::size) (string-equal (princ-to-string delim) "SIZE"))
      (list :string-blt :source source :dest dest)
      (unsupported-statement "STRING with character delimiter is not supported; use DELIMITED BY SIZE for BLT")))

(defun parse/string-blt-length-or-unsupported (_str source _del _by delim _into dest _len length)
  (declare (ignore _str _del _by _into _len))
  (if (or (eq delim 'eightbol::size) (string-equal (princ-to-string delim) "SIZE"))
      (list :string-blt :source source :dest dest :length length)
      (unsupported-statement "STRING with character delimiter is not supported; use DELIMITED BY SIZE for BLT")))

(defun parse/string-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "STRING with character delimiter is not supported; use DELIMITED BY SIZE for BLT"))

(defun parse/identifier-refmod (name _lp start _colon length _rp)
  (declare (ignore _lp _colon _rp))
  (list :refmod :base name :start start :length length))

;;; UNSTRING — unsupported
(defun parse/unstring-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "UNSTRING is not supported"))

;;; INSPECT — implemented
(defun parse/inspect-tallying (_insp id _tally expr _for _chars)
  (declare (ignore _insp _tally _for _chars))
  (list :inspect :target id :tallying expr :for :characters))

(defun parse/inspect-converting (_insp id _conv from _to to)
  (declare (ignore _insp _conv _to))
  (list :inspect :target id :converting from :to to))

(defun parse/inspect-replacing (_insp id _repl _chars _by repl)
  (declare (ignore _insp _repl _chars _by))
  (list :inspect :target id :replacing :characters :by repl))

;;; GOTO — implemented
(defun parse/paragraph (name _dot)
  (declare (ignore _dot))
  (list :paragraph (princ-to-string name)))

(defun parse/goto (_go target)
  (declare (ignore _go))
  (list :goto :target target))

(defun parse/procedure-name-list-append (list _comma name)
  (declare (ignore _comma))
  (append (ensure-list list) (list name)))

(defun parse/goto-depending (_go target _depending _on expr)
  (declare (ignore _go _depending _on))
  (list :goto :target target :depending-on expr))

(defun parse/goto-depending-multi (_go targets _depending _on expr)
  (declare (ignore _go _depending _on))
  (list :goto :targets (ensure-list targets) :depending-on expr))

;;; EVALUATE — implemented
(defun parse/when-clause (_when phrases stmts)
  (declare (ignore _when))
  (list :when phrases (if (listp stmts) stmts (list stmts))))

(defun parse/when-other-clause (_when _other stmts)
  (declare (ignore _when _other))
  (list :when-other (if (listp stmts) stmts (list stmts))))

(defun parse/when-clauses-append (clauses clause)
  (append (ensure-list clauses) (list clause)))

(defun parse/evaluate (subject when-clauses _end)
  (declare (ignore _end))
  (list :evaluate :subject subject :when-clauses (ensure-list when-clauses)))

;;; SET — unsupported forms (SET identifier TO expression is implemented)
(defun parse/set-up-by-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "SET ... UP BY ... is not supported"))

(defun parse/set-down-by-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "SET ... DOWN BY ... is not supported"))

(defun parse/set-condition-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "SET condition-name TO TRUE is not supported"))

(defun parse/set-address-of-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "SET ... TO ADDRESS OF ... is not supported"))

(defun parse/set-null (_set id _to _null)
  (declare (ignore _set _to _null))
  (list :set :target id :value :null))

(defun parse/set-nulls-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "SET ... TO NULLS is not supported"))

(defun parse/set-self-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "SET ... TO SELF is not supported"))

;;; ---------------------------------------------------------------
;;; YACC grammar definition
;;; ---------------------------------------------------------------
(eval `(yacc:define-parser *eightbol-parser*
         (:start-symbol eightbol-program)
         (:terminals (,@(token-list) number string symbol bareword picture-sequence))
         (:precedence ((:left * /) (:left + -)))
         (:muffle-conflicts :some)

         ;; Top-level: a class file
         (eightbol-program
          (eightbol-class-definition-structure comments* #'parse/eightbol-program)
          eightbol-class-definition-structure)

         (eightbol-class-definition-structure
          (comments* class-identification-division
           comments* environment-division
           comments* class-data-division
           comments* object |.|
           comments* data-division
           comments* object-procedure-division
           comments* end object |.|
           comments* end class end-class-name |.|
           comments*
           #'parse/class-file))

         ;; Class-level DATA DIVISION (WORKING-STORAGE) — globals, not object instance data
         (class-data-division
          ()
          (data division |.| working-storage section |.| data-item-description-entries))

         ;; Optional terminals (noise words / repetitions)
         ;; NOTE: ebol-comment is a non-terminal wrapping the COMMENT terminal.
         ;; It is named ebol-comment (not comment) to avoid a T/NT symbol collision
         ;; with the COMMENT keyword terminal in the :terminals list, which would
         ;; corrupt the LALR GOTO table.
         (comments* () ebol-comment (comments* ebol-comment))
         (ebol-comment (ebol-comment bareword) (ebol-comment symbol) comment)

         ;; ID / IDENTIFICATION interchangeable
         (id* (id (constantly 'identification)) (identification #'identity))

         ;; Literals and names
         (literal number string)
         ;; data-name accepts SYMBOL tokens and also SELF, which is tokenized as
         ;; a keyword but is legitimately used as a data item name (e.g. 01 Self
         ;; OBJECT REFERENCE) and as a slot qualifier (e.g. HP OF Self).
         (data-name symbol self)
         (procedure-name symbol)
         (procedure-name-list
          procedure-name
          (procedure-name-list |,| procedure-name #'parse/procedure-name-list-append))
         (section-name symbol)
         (paragraph-name symbol)
         (library-name symbol)
         (bank-identifier symbol number)
         (condition-name symbol)
         (alphabet-name minifont ascii petscii atascii unicode)
         ;; obj-ref-class: class name in OBJECT REFERENCE. end-class-name: at END CLASS.
         ;; Named to avoid T/NT collision with terminal CLASS-NAME (corrupts LALR table).
         (obj-ref-class symbol)
         (end-class-name symbol)
         (expr-class obj-ref-class)
         ;; Method names must be quoted string literals (e.g. METHOD-ID. "Test".)
         (method-name string)
         ;; picture-string: PICTURE-SEQUENCE (from lexer when after PIC/PICTURE),
         ;; or symbol, number, symbol(number), or picture-sequence(number) e.g. X(64)
         (picture-string
          picture-sequence
          symbol
          number
          (symbol |(| number |)|)
          (picture-sequence |(| number |)| #'parse/picture-sequence-repeat))

         ;; IDENTIFICATION DIVISION
         (class-identification-division
          (id* division |.|
               class-id |.| symbol |.|
               identification-division-clauses
               #'parse/class-id))

         (identification-division-clauses
          (identification-division-clauses identification-division-clause)
          identification-division-clause
          ())

         (identification-division-clause
          (inherits |.| symbol |.| #'parse/inherits)
          (author |.| comment-entry |.| #'parse/id-field)
          (installation |.| comment-entry |.| #'parse/id-field)
          (date-written |.| comment-entry |.| #'parse/id-field)
          (date-compiled |.| comment-entry |.| #'parse/id-field)
          (security |.| comment-entry |.| #'parse/id-field)
          ebol-comment)

         (comment-entry
          literal symbol bareword
          (comment-entry literal)
          (comment-entry symbol)
          (comment-entry bareword)
          (comment-entry |,|))

         ;; ENVIRONMENT DIVISION
         (environment-division
          ()
          (environment division |.|)
          (environment division |.| configuration-section))

         (configuration-section
          (configuration section |.|
                         source-computer-paragraph
                         object-computer-paragraph
                         special-names-paragraph
                         repository-paragraph))

         (source-computer-paragraph
          ()
          (source-computer |.| symbol |.| #'parse/id-field))

         (object-computer-paragraph
          ()
          (object-computer |.| object-computer-tag |.| #'parse/id-field))

         (object-computer-tag literal symbol)

         (special-names-paragraph
          ()
          (special-names |.| special-names-clauses))

         (special-names-clauses
          ()
          (special-names-clause special-names-clauses))

         (special-names-clause (alphabet alphabet-name))

         (repository-paragraph ())

         ;; DATA DIVISION
         (data-division
          ()
          (data division |.| data-division-sections))

         (data-division-sections
          (data-division-sections data-division-section)
          data-division-section
          ())

         (data-division-section
          (working-storage section |.| data-item-description-entries)
          (linkage section |.| data-item-description-entries)
          ;; OOP COBOL OBJECT DATA DIVISION: items may appear without a section header
          ;; (e.g. COPY-expanded copybooks in DATA DIVISION.).  Requires at least one
          ;; entry to stay unambiguous with data-division-sections → ().
          (data-item-description-entry data-item-description-entries))

         (data-item-description-entries
          (data-item-description-entries data-item-description-entry)
          data-item-description-entry
          ())

         ;; pic/usage/occurs: canonical order is PIC [USAGE] [OCCURS [min TO max] TIMES [DEPENDING ON x]]
         ;; e.g. 10 My-Variable PIC 9999 USAGE BINARY OCCURS 0 TO 15 TIMES DEPENDING ON My-Length
         ;; Parser accepts flexible order for compatibility with existing copybooks.
         (data-item-description-entry
          (level-number data-name
                        redefines-clause external-clause global-clause
                        justified-clause picture-clause occurs-clause
                        sign-clause synchronized-clause usage-clause
                        value-clause date-format-clause |.|)
          (level-number data-name
                        redefines-clause external-clause global-clause
                        justified-clause occurs-clause picture-clause
                        sign-clause synchronized-clause usage-clause
                        value-clause date-format-clause |.|)
          (level-number data-name
                        redefines-clause external-clause global-clause
                        justified-clause picture-clause usage-clause occurs-clause
                        sign-clause synchronized-clause
                        value-clause date-format-clause |.|)
          (level-number filler picture-clause sign-clause |.|)
          copy-statement
          ebol-comment)

         ;; level-number covers the full EIGHTBOL range: 01-49, 66, 77, 78, 88.
         ;; Special-purpose levels (66=RENAMES, 77=independent, 78=constant, 88=condition)
         ;; are distinguished by the action function of each data-item rule, not by
         ;; separate grammar non-terminals (which cause reduce/reduce conflicts with
         ;; the literal → number pathway).
         (level-number (number (lambda (level-number)
                                 (assert (and (integerp level-number)
                                              (or (< 0 level-number 50)
                                                  (member level-number '(66 77 78 88)))))
                                 level-number)))

         (external-clause (external) ())
         (global-clause ())

         (blank-when-zero-clause
          (blank when zero #'parse/blank-when-zero)
          (blank when zeroes)
          (blank zero)
          (blank zeroes)
          ())

         (date-format-clause
          (date format is picture-string)
          (date format picture-string)
          ())

         (justified-clause
          (justified right (constantly (list :justify :right)))
          (just right (constantly (list :justify :right)))
          ())

         (occurs-clause
          (occurs expression times #'parse/occurs)
          (occurs expression #'parse/occurs)
          (occurs number to number times depending on data-name #'parse/occurs-depending)
          (occurs number to number depending on data-name #'parse/occurs-depending)
          ())

         (picture-clause
          (picture is picture-string
                   (lambda (_p _i pic) (declare (ignore _p _i)) (list :pic pic)))
          (picture picture-string
                   (lambda (_p pic) (declare (ignore _p)) (list :pic pic)))
          (pic is picture-string
               (lambda (_p _i pic) (declare (ignore _p _i)) (list :pic pic)))
          (pic picture-string
               (lambda (_p pic) (declare (ignore _p)) (list :pic pic)))
          ())

         (redefines-clause (redefines data-name) ())

         (sign-clause
          (sign is leading) (sign is trailing)
          (sign leading) (sign trailing)
          (unsigned) (signed)
          ())

         (synchronized-clause (synchronized) (sync) ())

         (usage-clause
          (usage is binary (lambda (&rest _) (declare (ignore _)) (list :usage :binary)))
          (usage binary    (lambda (&rest _) (declare (ignore _)) (list :usage :binary)))
          ;; USAGE BINARY n  (explicit byte-count extension)
          (usage is binary number
                 (lambda (_u _is _b n) (declare (ignore _u _is _b)) (list :usage :binary :size n)))
          (usage binary number
                 (lambda (_u _b n) (declare (ignore _u _b)) (list :usage :binary :size n)))
          (usage is native  (lambda (&rest _) (declare (ignore _)) (list :usage :native)))
          (usage native     (lambda (&rest _) (declare (ignore _)) (list :usage :native)))
          (usage is display (lambda (&rest _) (declare (ignore _)) (list :usage :display)))
          (usage display    (lambda (&rest _) (declare (ignore _)) (list :usage :display)))
          (usage is packed-decimal (lambda (&rest _) (declare (ignore _)) (list :usage :bcd)))
          (usage packed-decimal    (lambda (&rest _) (declare (ignore _)) (list :usage :bcd)))
          (usage is decimal       (lambda (&rest _) (declare (ignore _)) (list :usage :bcd)))
          (usage decimal          (lambda (&rest _) (declare (ignore _)) (list :usage :bcd)))
          (usage is pointer        (lambda (&rest _) (declare (ignore _)) (list :usage :pointer)))
          (usage pointer           (lambda (&rest _) (declare (ignore _)) (list :usage :pointer)))
          (usage is procedure-pointer
                 (lambda (&rest _) (declare (ignore _)) (list :usage :procedure-pointer)))
          (usage procedure-pointer
                 (lambda (&rest _) (declare (ignore _)) (list :usage :procedure-pointer)))
          (usage object reference symbol
                 (lambda (_u _o _r cn) (declare (ignore _u _o _r)) (list :usage :object-ref :class cn)))
          (usage object reference string
                 (lambda (_u _o _r cn) (declare (ignore _u _o _r)) (list :usage :object-ref :class cn)))
          (object reference symbol
                  (lambda (_o _r cn) (declare (ignore _o _r)) (list :usage :object-ref :class cn)))
          (object reference string
                  (lambda (_o _r cn) (declare (ignore _o _r)) (list :usage :object-ref :class cn)))
          ;; OBJECT REFERENCE VALUE NULL — untyped null pointer (e.g. Self)
          (object reference value null
                  (lambda (_o _r _v _n) (declare (ignore _o _r _v _n))
                    (list :usage :object-ref :class nil)))
          ())

         (value-clause
          (value is literal)
          (value literal)
          (value is literal through literal)
          (value literal through literal)
          (values are literal through literal)
          (values literal through literal)
          (value is null)
          (value is nulls)
          ())

         ;; OBJECT PROCEDURE DIVISION containing method definitions
         (object-procedure-division
          (procedure division |.| method-definitions
                     #'parse/object-procedure-division))

         (method-definitions
          (method-definitions method-definition #'parse/method-defs-append)
          (method-definition (lambda (m) (list m)))
          ())

         ;; Each method: IDENTIFICATION DIVISION. METHOD-ID. "Name". PROCEDURE DIVISION. … END METHOD "Name".
         ;; Two forms: "END METHOD name." (two tokens) and "END-METHOD name." (one hyphenated token).
         (method-definition
          (method-identification-division
           procedure-statements
           end method method-name |.|
           #'parse/method-block)
          (method-identification-division
           procedure-statements
           |END-METHOD| method-name |.|
           #'parse/method-block-em))

         (method-identification-division
          (id* division |.|
               method-id |.| method-name |.|
               identification-division-clauses))

         ;; PROCEDURE DIVISION inside a method
         (procedure-statements
          (procedure division |.| stmt-sequence
                     #'parse/procedure-statements))

         ;; stmt-sequence builds a flat list of statement AST nodes.
         (stmt-sequence
          (stmt-sequence stmt-item #'parse/stmt-sequence-append)
          (stmt-item (lambda (item) (list item)))
          ())

         (stmt-item
          (statement |.| #'parse/stmt-item-with-dot)
          (statement (lambda (s) s))
          (ebol-comment (lambda (c) (declare (ignore c)) nil)))

         ;; Identifiers and expressions
         (identifier
          data-name
          (data-name of data-name)
          (data-name |(| subscript |)| #'parse/identifier-subscript))

         (subscript
          integer all data-name
          (data-name of data-name)
          (data-name + integer)
          (data-name - integer))

         (integer number)

         (pointer-value-expression
          self identifier
          (address of identifier)
          null nulls)

         (object-expression self identifier super)

         (expression
          literal
          identifier
          function-identifier
          (|(| expression |)|)
          (expression + expression #'parse/expr-add)
          (expression - expression #'parse/expr-subtract)
          (expression * expression #'parse/expr-multiply)
          (expression / expression #'parse/expr-divide)
          (expression shift-left expression #'parse/shift-left)
          (expression shift-right expression #'parse/shift-right)
          (expression bit-and expression #'parse/bit-and)
          (expression bit-or expression #'parse/bit-or)
          (expression bit-xor expression #'parse/bit-xor)
          (bit-not expression #'parse/bit-not))

         (function-identifier
          (function symbol |(| argument-list |)|)
          (function symbol))

         (argument-list
          (argument-list expression)
          expression
          ())

         ;; Conditions
         ;; pointer-condition is omitted: its rules are all covered by relation-condition
         ;; since pointer-value-expression derives from expression.
         (condition
          relation-condition
          class-condition
          sign-condition
          negated-condition
          combined-condition
          identifier)

         (negated-condition (not condition))
         (combined-condition
          (condition and condition #'parse/cond-and)
          (condition or condition #'parse/cond-or))

         ;; Use expression (not identifier) so the parser reduces to expression first,
         ;; then the token after IS determines which condition type applies.
         (class-condition
          (expression is numeric)
          (expression is not numeric)
          (expression is alphabetic)
          (expression is not alphabetic)
          (expression is zero)
          (expression is not zero)
          (expression is null #'parse/cond-is-null)
          (expression is not null #'parse/cond-is-not-null)
          (expression is expr-class)
          (expression is not expr-class))

         (sign-condition
          (expression is positive)
          (expression is not positive)
          (expression is negative)
          (expression is not negative)
          (expression is zero)
          (expression is not zero))


         (relation-condition
          (expression is greater than expression)
          (expression greater than expression)
          (expression is > expression)
          (expression > expression)
          (expression is not greater than expression)
          (expression not greater than expression)
          (expression is not > expression)
          (expression not > expression)
          (expression is <= expression)
          (expression <= expression)
          (expression is ≤ expression)
          (expression ≤ expression)
          (expression is less than expression)
          (expression less than expression)
          (expression is < expression)
          (expression < expression)
          (expression is not less than expression)
          (expression not less than expression)
          (expression is not < expression)
          (expression not < expression)
          (expression is >= expression)
          (expression >= expression)
          (expression is ≥ expression)
          (expression ≥ expression)
          (expression is equal to expression)
          (expression equal to expression)
          (expression is = expression #'parse/cond-eq)
          (expression = expression #'parse/cond-eq)
          (expression is not equal to expression)
          (expression not equal to expression)
          (expression is not = expression)
          (expression not = expression)
          (expression /= expression)
          (expression ≠ expression)
          (expression is greater than or equal to expression)
          (expression is greater or equal to expression)
          (expression is less than or equal to expression)
          (expression is less or equal to expression))

         ;; Statements
         (statement
          add-statement call-statement cancel-statement
          compute-statement copy-statement
          debug-break-statement
          divide-statement
          evaluate-statement
          exit-method-statement exit-program-statement exit-statement
          goback-statement goto-statement
          if-statement inspect-statement invoke-statement
          log-fault-statement
          move-statement multiply-statement
          paragraph-statement
          perform-statement
          search-statement set-statement stop-statement
          string-statement subtract-statement
          unstring-statement)

         (paragraph-statement
          (paragraph-name |.| #'parse/paragraph))

         (add-statement
          (add expression to expression giving identifier #'parse/add-giving)
          (add expression to identifier #'parse/add-to))

         (call-statement
          ;; CALL SERVICE Target.  — service dispatch, bank resolved at link time
          (call service call-target #'parse/call-service)
          ;; CALL Target IN SERVICE BankName.  — service dispatch with explicit bank (.FarCall)
          (call call-target in service bank-identifier #'parse/call-in-service)
          ;; CALL Target IN BANK BankName.  — far call to explicit bank (.FarJSR)
          (call call-target in bank bank-identifier #'parse/call-in-bank)
          ;; CALL Target IN LIBRARY LibraryName.  — near jsr (LastBank is always resident)
          (call call-target in library bank-identifier #'parse/call-in-library)
          ;; CALL Target.  — local jsr
          (call call-target #'parse/call))

         (call-target identifier literal symbol)
         (cancel-statement (cancel identifier) (cancel literal))

         (compute-statement
          (compute identifier = expression #'parse/compute-eq)
          (compute identifier equal expression #'parse/compute-eq)
          (compute expression giving identifier))

         (copy-statement
          (copy symbol |.| #'parse/copy)
          (copy symbol of library-name |.| #'parse/copy-of)
          (copy symbol in library-name |.| #'parse/copy-in))

         (debug-break-statement
          (debug break expression #'parse/debug-break))

         (divide-statement
          (divide expression into identifier #'parse/divide-unsupported)
          (divide expression into expression giving identifier #'parse/divide-unsupported)
          (divide expression by expression giving identifier #'parse/divide-unsupported)
          (divide expression into expression giving identifier remainder identifier #'parse/divide-unsupported)
          (divide expression by expression giving identifier remainder identifier #'parse/divide-unsupported))

         (evaluate-statement
          (evaluate eval-subject when-clauses end-evaluate #'parse/evaluate))

         (end-evaluate
          (|END-EVALUATE| (constantly nil))
          ())

         (eval-subject
          expression true false
          (eval-subject also eval-subject))

         (when-clauses
          (when-clauses when-clause #'parse/when-clauses-append)
          when-clause)

         (when-clause
          (when evaluate-phrases imperative-statements #'parse/when-clause)
          (when other imperative-statements #'parse/when-other-clause))

         (evaluate-phrases
          any condition true false
          expression
          (not expression)
          (expression through expression)
          (expression thru expression)
          (evaluate-phrases also evaluate-phrases))

         ;; imperative-statements allows each statement to have an optional trailing period.
         ;; Uses the same list-building pattern as stmt-sequence to produce a flat list.
         (imperative-statements
          (imperative-statements stmt-item #'parse/stmt-sequence-append)
          (stmt-item (lambda (item) (list item))))

         (exit-statement (exit (constantly (list :exit))))
         (exit-method-statement  (exit method  #'parse/exit-method))
         (exit-program-statement (exit program #'parse/exit-program))
         (goback-statement  (goback  (constantly (list :goback))))
         (stop-statement    (stop run #'parse/stop-run))

         (goto-statement
          (go to procedure-name #'parse/goto)
          (go procedure-name #'parse/goto)
          (go to procedure-name-list depending on expression #'parse/goto-depending-multi)
          (go procedure-name-list depending on expression #'parse/goto-depending-multi)
          (go to procedure-name depending on expression #'parse/goto-depending)
          (go procedure-name depending on expression #'parse/goto-depending))

         (if-statement
          (if condition then imperative-statements end-if
              #'parse/if-then)
          (if condition then imperative-statements
              else imperative-statements end-if
              #'parse/if-then-else)
          (if condition then next sentence end-if))

         (end-if
          (|END-IF| (constantly nil))
          ())

         (inspect-statement
          (inspect identifier tallying expression for characters #'parse/inspect-tallying)
          (inspect identifier converting expression to expression #'parse/inspect-converting)
          (inspect identifier replacing characters by expression #'parse/inspect-replacing))

         (invoke-statement
          (invoke object-expression method-name returning identifier
                  #'parse/invoke-returning)
          (invoke object-expression method-name #'parse/invoke))

         (log-fault-statement
          (log fault expression #'parse/log-fault))

         (move-statement
          (move expression to identifier #'parse/move))

         (multiply-statement
          (multiply expression by identifier #'parse/multiply-unsupported)
          (multiply expression by expression giving identifier #'parse/multiply-unsupported))

         (perform-statement
          (perform procedure-name expression times #'parse/perform-times)
          (perform procedure-name through procedure-name)
          (perform procedure-name thru procedure-name)
          (perform procedure-name until condition #'parse/perform-until)
          (perform procedure-name with test before until condition)
          (perform procedure-name with test after until condition)
          (perform procedure-name varying identifier from expression by expression until condition #'parse/perform-varying)
          (perform procedure-name #'parse/perform-proc)
          (perform imperative-statements end-perform))

         (end-perform
          (|END-PERFORM| (constantly nil))
          ())

         (search-statement
          (search identifier when condition imperative-statements end-search))

         (end-search
          (|END-SEARCH| (constantly nil))
          ())

         (set-statement
          (set identifier to expression #'parse/set-to)
          (set identifier up by expression #'parse/set-up-by-unsupported)
          (set identifier down by expression #'parse/set-down-by-unsupported)
          (set condition-name to true #'parse/set-condition-unsupported)
          (set identifier to address of identifier #'parse/set-address-of-unsupported)
          (set identifier to null #'parse/set-null)
          (set identifier to nulls #'parse/set-nulls-unsupported)
          (set identifier to self #'parse/set-self-unsupported))

         ;; string-operand: identifier, literal, or identifier with reference modification (start:length)
         (string-operand
          identifier
          literal
          (data-name |(| expression |:| expression |)| #'parse/identifier-refmod))
         ;; string-delimiter: SIZE (BLT) or expression (char delimiter — unsupported)
         (string-delimiter size expression)
         (string-statement
          (string string-operand delimited by string-delimiter into string-operand #'parse/string-blt-or-unsupported)
          (string string-operand delimited by string-delimiter into string-operand length expression #'parse/string-blt-length-or-unsupported))

         (subtract-statement
          (subtract expression from expression giving identifier #'parse/subtract-giving)
          (subtract expression from identifier #'parse/subtract-from))

         (unstring-statement
          (unstring identifier delimited by expression
                    into identifier #'parse/unstring-unsupported))))

;;; ---------------------------------------------------------------
;;; Lexer → token stream adapter
;;; ---------------------------------------------------------------
(defun stream-code (lexer-tokens)
  "Return a YACC lexer thunk that pops tokens from LEXER-TOKENS list.
The lexval passed to parser action functions is (second token), i.e. the raw
parsed value (string, number, etc.) — NOT the full token plist.
As a side effect, each consumed token's source location is stored in
*CURRENT-TOKEN-LOCATION* for use in error reporting."
  (lambda ()
    (loop
      (unless lexer-tokens (return (values nil nil)))
      (let ((token (pop lexer-tokens)))
        (when token
          (setf *current-token-location*
                (list :source-file     (getf token :source-file)
                      :source-line     (getf token :source-line)
                      :source-sequence (getf token :source-sequence)))
          (return (values (first token) (second token))))))))

(defun parse-eightbol-string (string &optional (pathname "<String>"))
  (with-input-from-string (stream string)
    (let ((*source-file-pathname* pathname))
      (parse-eightbol stream))))

(defun parse-eightbol (stream)
  "Lex STREAM (with COPY expansion) and parse with YACC, returning AST plist.
Parse errors are caught and re-signalled as EIGHTBOL-SOURCE-ERROR conditions
that include the source file, line number, and sequence number."
  (let ((tokens (lex-with-copy-expansion stream))
        (*current-token-location* nil))
    (handler-case
        (yacc:parse-with-lexer
         (stream-code tokens)
         *eightbol-parser*)
      (yacc:yacc-parse-error (e)
        (let* ((loc   *current-token-location*)
               (term  (yacc:yacc-parse-error-terminal e))
               (val   (yacc:yacc-parse-error-value e))
               (exp   (yacc:yacc-parse-error-expected-terminals e))
               (msg   (format nil
                        "Unexpected token ~a~@[ (~s)~].~%~10tExpected one of: ~{~a~^, ~}"
                        term val exp)))
          (error 'source-error
                 :source-file     (getf loc :source-file)
                 :source-line     (getf loc :source-line)
                 :source-sequence (getf loc :source-sequence)
                 :terminal        term
                 :token-value     val
                 :expected        exp
                 :message         msg))))))
