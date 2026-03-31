;; src/parser.lisp
(in-package :eightbol)

;;; Source-location tracking for error reporting

(defvar *current-token-location* nil
  "Plist (:source-file :source-line :source-sequence) of the token most
recently consumed by the YACC lexer thunk. Set by STREAM-CODE.")

;;; source-error is defined in conditions.lisp

(defun safe-getf (plist key)
  "Return (getf plist key) when PLIST is a plausible plist; otherwise nil. 

Avoids type-error on (nil) or tails like (\"Name\" :source-file …) from @code{:paragraph}."
  (when (and plist (listp plist) (evenp (length plist)))
    (handler-case (getf plist key)
      (type-error () nil))))

;;; Token list — every terminal used anywhere in the grammar must
;;; appear here so the lexer produces the right token type.
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun token-list ()
    '(|(| |)| |:| |,| + - * / × ÷ |.| /= < <= = > >= ≠ ≤ ≥
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
      packed-decimal perform petscii pic picture pointer positive process
      procedure procedure-pointer program
      redefines reference remainder renames replacing returning right run
      search section security self sentence sentences service set
      shift-left shift-right sign signed size subtract
      source-computer special-names stop string super symbol
      tallying test than then through thru times title to trailing true
      unicode unsigned until up usage
      value values varying
      when with working-storage
      zero zeroes)))

;;; Parser action functions

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
  (let* ((class-id (safe-getf id :class-id)))
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

(defun stmt-with-source-location (stmt)
  "Append @code{:source-file}, @code{:source-line}, @code{:source-sequence}, and
@code{:source-text} (trimmed COBOL line from the lexer) from
@code{*current-token-location*} to STMT so backends can emit assembly comments."
  (unless (and (listp stmt) (first stmt))
    (return-from stmt-with-source-location stmt))
  (let ((loc *current-token-location*))
    (if (null loc)
        stmt
        (let ((suffix
               (append (when (safe-getf loc :source-file)
                         (list :source-file (safe-getf loc :source-file)))
                       (when (safe-getf loc :source-line)
                         (list :source-line (safe-getf loc :source-line)))
                       (when (safe-getf loc :source-sequence)
                         (list :source-sequence (safe-getf loc :source-sequence)))
                       (when (safe-getf loc :source-line-text)
                         (list :source-text (safe-getf loc :source-line-text))))))
          ;; (:paragraph \"Name\") — second slot is the name, not a plist tail; inserting
          ;; keys after @code{append} would make @code{(rest stmt)} a malformed plist for @code{getf}.
          (if (and (eq (first stmt) :paragraph) (>= (length stmt) 2))
              (list* :paragraph (second stmt) (append suffix (cddr stmt)))
              (append stmt suffix))))))

(defun parse/stmt-item-with-dot (stmt _dot)
  "stmt-item: statement followed by period — discard the period."
  (declare (ignore _dot))
  (stmt-with-source-location stmt))

(defun parse/stmt-item-no-dot (stmt)
  "stmt-item: statement without optional period (same grammar as COBOL allows)."
  (stmt-with-source-location stmt))

(defun parse/eightbol-program (struct _comments)
  "Top-level program rule: ignore trailing comments, return the program plist."
  (declare (ignore _comments))
  struct)

(defun parse/class-id (_id _div _stop1
                       _cid _stop2 class-id _stop3
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

(defun parse/data-name-id (_id)
  "Data name ID (keyword token) — return string \"ID\" for AST/copybook consistency."
  (declare (ignore _id))
  "ID")

(defun parse/method-name-from-symbol (sym)
  "METHOD-ID. identifier (unquoted) — return string for AST (e.g. Energize → \"Energize\")."
  (string-capitalize (princ-to-string sym)))

(defun parse/identifier-subscript (name _lp sub _rp)
  "Action for: data-name |(| subscript |)| — subscripted identifier.
 Receives $1=data-name, $2=|(|, $3=subscript, $4=|)|."
  (declare (ignore _lp _rp))
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

(defun parse/cond-is-zero (expr _is _zero)
  (declare (ignore _is _zero))
  (list :is-zero expr))

(defun parse/cond-is-not-zero (expr _is _not _zero)
  (declare (ignore _is _not _zero))
  (list :is-not-zero expr))

(defun parse/cond-eq (expr1 _op expr2 &optional expr3)
  (declare (ignore _op))
  (list '= expr1 (or expr3 expr2)))

(defun parse/cond-rel-equal-is (e1 _is _equal _to e2)
  "Relation @code{(= E1 E2)} from @samp{E1 IS EQUAL TO E2}."
  (declare (ignore _is _equal _to))
  (list '= e1 e2))

(defun parse/cond-rel-equal-omitted (e1 _equal _to e2)
  "Relation @code{(= E1 E2)} from @samp{E1 EQUAL TO E2}."
  (declare (ignore _equal _to))
  (list '= e1 e2))

(defun parse/cond-rel-less-is (e1 _is _less _than e2)
  "Relation @code{(< E1 E2)} from @samp{E1 IS LESS THAN E2}."
  (declare (ignore _is _less _than))
  (list '< e1 e2))

(defun parse/cond-rel-less-omitted (e1 _less _than e2)
  "Relation @code{(< E1 E2)} from @samp{E1 LESS THAN E2}."
  (declare (ignore _less _than))
  (list '< e1 e2))

(defun parse/cond-rel-greater-is (e1 _is _greater _than e2)
  "Relation @code{(> E1 E2)} from @samp{E1 IS GREATER THAN E2}."
  (declare (ignore _is _greater _than))
  (list '> e1 e2))

(defun parse/cond-rel-greater-omitted (e1 _greater _than e2)
  "Relation @code{(> E1 E2)} from @samp{E1 GREATER THAN E2}."
  (declare (ignore _greater _than))
  (list '> e1 e2))

(defun parse/bit-or (e1 _op e2)
  (declare (ignore _op))
  (list :bit-or e1 e2))

(defun parse/bit-xor (e1 _op e2)
  (declare (ignore _op))
  (list :bit-xor e1 e2))

(defun parse/bit-not (_bit_not expr)
  (declare (ignore _bit_not))
  (list :bit-not expr))

(defun parse/expr-zero (_zero)
  "ZERO as expression (78-level constant) — yields literal 0."
  (declare (ignore _zero))
  '(:literal 0))

(defun parse/expr-zeroes (_zeroes)
  "ZEROES as expression (78-level constant) — yields literal 0."
  (declare (ignore _zeroes))
  '(:literal 0))

(defun parse/expr-null (_null)
  "NULL as pointer / aggregate source in MOVE, COMPUTE, etc."
  (declare (ignore _null))
  :null)

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
  "CALL SERVICE target. — service-dispatch call; bank must be specified at link time."
  (declare (ignore _call _service))
  (list :call :service target :bank nil))

(defun parse/call-in-service (_call target _in _service bank)
  "CALL target IN SERVICE bank. — service-dispatch call with explicit bank."
  (declare (ignore _call _in _service))
  (list :call :service target :bank bank))

(defun parse/call-in-bank (_call target _in _bank bank)
  "CALL target IN BANK bank. — far call to an explicit bank."
  (declare (ignore _call _in _bank))
  (list :call :target target :bank bank))

(defun parse/call-in-library (_call target _in _library _name)
  "CALL target IN LIBRARY name.
Library routines live in LastBank which is always resident at $c000.
Generates a near jsr (no bank switch) regardless of the library name."
  (declare (ignore _call _in _library _name))
  (list :call :target target :bank nil :library t))

(defun parse/call-in-library-omit-name (_call target _in _library)
  "CALL target IN LIBRARY. — library name omitted (LastBank implied)."
  (declare (ignore _call _in _library))
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

(defun parse/perform-cobol-times (_perform name _times expr)
  "Build @code{(:perform :procedure NAME :times EXPR)} for PERFORM NAME TIMES EXPR.

_INPUTS_: PERFORM token, procedure NAME, TIMES keyword, count EXPR.
_OUTPUT_: perform AST plist."
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

;;; Unsupported-statement: signal compile-time error
;;;
;;; Statement forms that parse but are not implemented signal
;;; EIGHTBOL-SOURCE-ERROR at compile time.
;;;
;;; INSPECT — implemented: TALLYING … FOR CHARACTERS, CONVERTING … TO …,
;;; REPLACING CHARACTERS BY … (see parse/inspect-* below).
;;;
;;; GOTO — implemented: GO/GO TO procedure-name, GO TO id DEPENDING ON expr.
;;;
;;; EVALUATE — implemented: EVALUATE subject WHEN ... [WHEN OTHER ...] END-EVALUATE.
;;;
;;; SET — implemented: TO expression, TO NULL, UP BY, DOWN BY, TO ADDRESS OF, TO SELF.
;;; Unsupported: SET condition-name TO TRUE; SET identifier TO NULLS.

(defun unsupported-statement (message)
  "Signal EIGHTBOL-SOURCE-ERROR for an unsupported statement at current token location."
  (error 'source-error
         :source-file (safe-getf *current-token-location* :source-file)
         :source-line (safe-getf *current-token-location* :source-line)
         :source-sequence (safe-getf *current-token-location* :source-sequence)
         :message message))

;;; DIVIDE — supported only when divisor is constant power-of-two (1, 2, 4, 8, ...).
;;; INTO id, INTO expr GIVING id, BY expr GIVING id. Remainder forms unsupported.
(defun parse/divide-into-id (_div expr _into id)
  (declare (ignore _div _into))
  (list :divide :into id :divisor expr))

(defun parse/divide-into-giving (_div divisor _into dividend _giving id)
  (declare (ignore _div _into _giving))
  (list :divide :into dividend :giving id :divisor divisor))

(defun parse/divide-by-giving (_div divisor _by dividend _giving id)
  (declare (ignore _div _by _giving))
  (list :divide :divisor divisor :by dividend :giving id))

(defun parse/divide-into-remainder-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "DIVIDE ... REMAINDER ... is not supported"))

;;; MULTIPLY — supported only when multiplier is constant power-of-two.
(defun parse/multiply-by-id (_mul expr _by id)
  (declare (ignore _mul _by))
  (list :multiply :by id :multiplier expr))

(defun parse/multiply-by-giving (_mul mult _by op _giving id)
  (declare (ignore _mul _by _giving))
  (list :multiply :by op :giving id :multiplier mult))

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

(defun parse/goto (go token-or-to &optional target)
  "Return (:goto :target …). @code{GO TO name} passes three values (@code{go}, @code{to}, NAME); @code{GO name} passes two (@code{go}, NAME)."
  (declare (ignore go))
  (if target
      (list :goto :target target)
      (list :goto :target token-or-to)))

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

(defun parse/evaluate (_evaluate subject when-clauses _end-evaluate)
  "Return (:evaluate :subject SUBJECT :when-clauses …). YACC passes four values (EVALUATE token, subject, clauses, end)."
  (declare (ignore _evaluate _end-evaluate))
  (let ((wc when-clauses))
    (list :evaluate :subject subject
          :when-clauses (cond
                          ((null wc) nil)
                          ;; One clause: (:when …) or (:when-other …) — wrap as list of clauses.
                          ((and (listp wc) (symbolp (first wc))
                                (member (first wc) '(:when :when-other)))
                           (list wc))
                          (t (ensure-list wc))))))

;;; SET — UP BY, DOWN BY, TO ADDRESS OF, TO SELF (TO expr and TO NULL implemented above)
(defun parse/set-up-by (_set identifier _up _by expr)
  (declare (ignore _set _up _by))
  (list :set :up-by identifier :by expr))

(defun parse/set-down-by (_set identifier _down _by expr)
  (declare (ignore _set _down _by))
  (list :set :down-by identifier :by expr))

(defun parse/set-condition-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "SET condition-name TO TRUE is not supported"))

(defun parse/set-address-of (_set target-id _to _address _of source-id)
  (declare (ignore _set _to _address _of))
  (list :set :target target-id :address-of source-id))

(defun parse/set-null (_set id _to _null)
  (declare (ignore _set _to _null))
  (list :set :target id :value :null))

(defun parse/set-nulls-unsupported (&rest _) (declare (ignore _))
  (unsupported-statement "SET ... TO NULLS is not supported"))

(defun parse/set-self (_set identifier _to _self)
  (declare (ignore _set _to _self))
  (list :set :to-self identifier))

;;; YACC grammar definition
(eval `(yacc:define-parser *eightbol-parser*
         (:start-symbol eightbol-program)
         (:terminals (,@(token-list) number string symbol bareword picture-sequence))
         (:precedence ((:left * / × ÷) (:left + -)))
         (:muffle-conflicts :some)

         ;; Top-level: a class file
         (eightbol-program
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

         ;; Class-level DATA  DIVISION (WORKING-STORAGE) —  globals, not
         ;; object instance data
         (class-data-division
          ()
          (data division |.| working-storage section |.| data-item-description-entries))
         
         ;; Optional  terminals   (noise  words  /   repetitions)  NOTE:
         ;; eightbol-comment   is  a   non-terminal  wrapping   the  COMMENT
         ;; terminal. It  is named  eightbol-comment (not comment)  to avoid
         ;; a T/NT symbol collision with the COMMENT keyword terminal in
         ;; the   :terminals  list,   which  would   corrupt  the   LALR
         ;; GOTO table.
         (comments* () eightbol-comment (comments* eightbol-comment))
         (eightbol-comment (eightbol-comment bareword) (eightbol-comment symbol) comment)
         
         ;; ID / IDENTIFICATION interchangeable
         (id* (id (constantly 'identification)) (identification #'identity))

         ;; Literals and names
         (literal number string)
         ;; data-name accepts SYMBOL tokens and also SELF, ID (keyword but valid
         ;; data names in COBOL, e.g. 01 Self OBJECT REFERENCE, 05 ID PIC 99).
         (data-name symbol self (id #'parse/data-name-id))
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
         ;; Method names: quoted string (METHOD-ID. "Think".) or identifier (METHOD-ID. Energize.)
         (method-name string (symbol #'parse/method-name-from-symbol))
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
          eightbol-comment)

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
          ;; (e.g. COPY-expanded copybooks in DATA DIVISION.). Requires at least one
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
                        sign-clause usage-clause
                        value-clause date-format-clause |.|)
          (level-number data-name
                        redefines-clause external-clause global-clause
                        justified-clause occurs-clause picture-clause
                        sign-clause usage-clause
                        value-clause date-format-clause |.|)
          (level-number data-name
                        redefines-clause external-clause global-clause
                        justified-clause picture-clause usage-clause occurs-clause
                        sign-clause
                        value-clause date-format-clause |.|)
          (level-number filler picture-clause sign-clause |.|)
          copy-statement
          eightbol-comment)

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

         (usage-clause
          (usage is binary (lambda (&rest _) (declare (ignore _)) (list :usage :binary)))
          (usage binary (lambda (&rest _) (declare (ignore _)) (list :usage :binary)))
          ;; USAGE BINARY n (explicit byte-count extension)
          (usage is binary number
                 (lambda (_u _is _b n) (declare (ignore _u _is _b)) (list :usage :binary :size n)))
          (usage binary number
                 (lambda (_u _b n) (declare (ignore _u _b)) (list :usage :binary :size n)))
          (usage is native (lambda (&rest _) (declare (ignore _)) (list :usage :native)))
          (usage native (lambda (&rest _) (declare (ignore _)) (list :usage :native)))
          (usage is display (lambda (&rest _) (declare (ignore _)) (list :usage :display)))
          (usage display (lambda (&rest _) (declare (ignore _)) (list :usage :display)))
          (usage is packed-decimal (lambda (&rest _) (declare (ignore _)) (list :usage :bcd)))
          (usage packed-decimal (lambda (&rest _) (declare (ignore _)) (list :usage :bcd)))
          (usage is decimal (lambda (&rest _) (declare (ignore _)) (list :usage :bcd)))
          (usage decimal (lambda (&rest _) (declare (ignore _)) (list :usage :bcd)))
          (usage is pointer (lambda (&rest _) (declare (ignore _)) (list :usage :pointer)))
          (usage pointer (lambda (&rest _) (declare (ignore _)) (list :usage :pointer)))
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
          (statement #'parse/stmt-item-no-dot)
          (eightbol-comment (lambda (c) (declare (ignore c)) nil)))

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
          (expression × expression #'parse/expr-multiply)
          (expression / expression #'parse/expr-divide)
          (expression ÷ expression #'parse/expr-divide)
          (expression shift-left expression #'parse/shift-left)
          (expression shift-right expression #'parse/shift-right)
          (expression bit-and expression #'parse/bit-and)
          (expression bit-or expression #'parse/bit-or)
          (expression bit-xor expression #'parse/bit-xor)
          (bit-not expression #'parse/bit-not)
          (zero #'parse/expr-zero)
          (zeroes #'parse/expr-zeroes)
          (null #'parse/expr-null)
          )

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
          (expression is zero #'parse/cond-is-zero)
          (expression is not zero #'parse/cond-is-not-zero)
          (expression is null #'parse/cond-is-null)
          (expression is not null #'parse/cond-is-not-null)
          (expression is expr-class)
          (expression is not expr-class))

         (sign-condition
          (expression is positive)
          (expression is not positive)
          (expression is negative)
          (expression is not negative)
          (expression is zero #'parse/cond-is-zero)
          (expression is not zero #'parse/cond-is-not-zero))


         (relation-condition
          (expression is greater than expression #'parse/cond-rel-greater-is)
          (expression greater than expression #'parse/cond-rel-greater-omitted)
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
          (expression is less than expression #'parse/cond-rel-less-is)
          (expression less than expression #'parse/cond-rel-less-omitted)
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
          (expression is equal to expression #'parse/cond-rel-equal-is)
          (expression equal to expression #'parse/cond-rel-equal-omitted)
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
          ;; CALL SERVICE Target. — service dispatch, bank resolved at link time
          (call service call-target #'parse/call-service)
          ;; CALL Target IN SERVICE BankName. — service dispatch with explicit bank (.FarCall)
          (call call-target in service bank-identifier #'parse/call-in-service)
          ;; CALL Target IN BANK BankName. — far call to explicit bank (.FarJSR)
          (call call-target in bank bank-identifier #'parse/call-in-bank)
          ;; CALL Target IN LIBRARY [LibraryName]. — near jsr (LastBank is always resident)
          (call call-target in library bank-identifier #'parse/call-in-library)
          (call call-target in library #'parse/call-in-library-omit-name)
          ;; CALL Target. — local jsr
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
          (divide expression into identifier #'parse/divide-into-id)
          (divide expression into expression giving identifier #'parse/divide-into-giving)
          (divide expression by expression giving identifier #'parse/divide-by-giving)
          (divide expression into expression giving identifier remainder identifier #'parse/divide-into-remainder-unsupported)
          (divide expression by expression giving identifier remainder identifier #'parse/divide-into-remainder-unsupported))

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
         (exit-method-statement (exit method #'parse/exit-method))
         (exit-program-statement (exit program #'parse/exit-program))
         (goback-statement (goback (constantly (list :goback))))
         (stop-statement (stop run #'parse/stop-run))

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
          (multiply expression by identifier #'parse/multiply-by-id)
          (multiply expression by expression giving identifier #'parse/multiply-by-giving))

         (perform-statement
          (perform procedure-name times expression #'parse/perform-cobol-times)
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
          (set identifier up by expression #'parse/set-up-by)
          (set identifier down by expression #'parse/set-down-by)
          (set condition-name to true #'parse/set-condition-unsupported)
          (set identifier to address of identifier #'parse/set-address-of)
          (set identifier to null #'parse/set-null)
          (set identifier to nulls #'parse/set-nulls-unsupported)
          (set identifier to self #'parse/set-self))

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

;;; Lexer → token stream adapter
(defun stream-code (lexer-tokens)
  "Return a YACC lexer thunk that pops tokens from LEXER-TOKENS list.
The lexval passed to parser action functions is (second token), i.e. the raw
parsed value (string, number, etc.) — NOT the full token plist.
As a side effect, each consumed token's source location is stored in
*CURRENT-TOKEN-LOCATION* for use in error reporting and assembly comments."
  (lambda ()
    (loop
       (unless lexer-tokens (return (values nil nil)))
       (let ((token (pop lexer-tokens)))
         (when token
           (setf *current-token-location*
                 (list :source-file (safe-getf token :source-file)
                       :source-line (safe-getf token :source-line)
                       :source-sequence (safe-getf token :source-sequence)
                       :source-line-text (safe-getf token :source-line-text)))
           (return (values (first token) (second token))))))))

(defun parse-eightbol-string (string &optional (pathname "<String>"))
  (with-input-from-string (stream string)
    (let ((*source-file-pathname* pathname))
      (parse-eightbol stream))))

(defun parse-eightbol (stream)
  "Lex STREAM (with COPY expansion) and parse with YACC, returning AST plist.
Parse  errors  are  caught  and  re-signalled  as  EIGHTBOL-SOURCE-ERROR
conditions   that   include   the   source  file,   line   number,   and
sequence number."
  (let ((tokens (lex-with-copy-expansion stream)))
    (handler-case
        (yacc:parse-with-lexer
         (stream-code tokens)
         *eightbol-parser*)
      (yacc:yacc-parse-error (e)
        (error 'source-error
               :source-file (safe-getf *current-token-location* :source-file)
               :source-line (safe-getf *current-token-location* :source-line)
               :source-sequence (safe-getf *current-token-location* :source-sequence)
               :terminal (yacc:yacc-parse-error-terminal e)
               :token-value (yacc:yacc-parse-error-value e)
               :expected (yacc:yacc-parse-error-expected-terminals e)
               :message (format nil
                                "Unexpected token ~s~@[ (~s)~].~%~10tExpected one of: ~{~s~^, ~}"
                                term val exp))))))
