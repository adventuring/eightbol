;; src/backend.lisp — generic code-generation interface
(in-package :eightbol)

;;; Public generic function

(defgeneric compile-to-assembly (ast cpu output-stream)
  (:documentation
   "Compile a parsed EIGHTBOL AST to assembly source text for CPU,
writing output to OUTPUT-STREAM.
AST must be a plist of the form (:program :class-id NAME :methods (...)).
CPU is a keyword such as :6502, :65c02, :65c816, :cp1610, :Z80, :m68k.
The default method signals an error for unsupported CPUs.")
  (:method (ast cpu output-stream)
    (declare (ignore ast output-stream))
    (error "EIGHTBOL: CPU ~s is not supported" cpu)))

;;; Copybook file parsing for slot/type/constant resolution
;;;
;;; The canonical copybook format (from make-eightbol-copybooks) is:
;;;   * Inherited from X:
;;;    05 SlotName PIC ... .
;;;   * Own slots (X):
;;;    05 SlotName OBJECT REFERENCE ClassName.
;;;
;;; Phantasia-Globals.cpy adds:
;;;    01 SECTION EXTERNAL.
;;;    05 VarName PIC ... .
;;;    77 ConstName PIC ... VALUE n.
;;;    78 EnumName  PIC ... VALUE n.

(defun class-id-to-copybook-filename (class-id)
  "Convert CLASS-ID (PascalCase) to copybook filename base in Title-And-Hyphens form.
   Character → Character, NonPlayerCharacter → Non-Player-Character"
  (header-case class-id))

(defun cobol-strip-class-copybook-prefix (name)
  "If NAME begins with the Title-And-Hyphens spelling of @code{*CLASS-ID*} and a hyphen, return the suffix; else NIL.

Used so qualified identifiers (e.g. @code{Character-Song--Heal--ID}) resolve like unqualified
copybook names (@code{Song--Heal--ID}) for @code{*CONST-TABLE*} and @code{*SLOT-TABLE*}.

@table @asis
@item NAME
COBOL data name (string).
@end table

@subsection Outputs
Unqualified suffix string, or NIL."
  (when (and *class-id* name (stringp name))
    (let ((pref (format nil "~a-" (class-id-to-copybook-filename *class-id*))))
      (when (and (> (length name) (length pref))
                 (string-equal pref name :end2 (length pref)))
        (subseq name (length pref))))))

(defvar *eightbol-root-directory* nil
  "Root directory of the project; bound during compile-eightbol-class.
Used by backends to locate generated copybook files.")

;;; Dynamic bindings for copybook tables (bound during compilation)
(defvar *output-stream* nil "Assembly output stream; bound during compile-to-assembly.")
(defvar *class-id* nil)

(define-constant +object-reference-storage-width+ 2
  :test 'eql
  :documentation
  "Byte width for an OBJECT REFERENCE operand when no PIC row exists in @code{*pic-width-table*}.")
(defvar *method-id* nil "Current method being compiled; used for paragraph labels.")
(defvar *slot-table* (make-hash-table :test 'equal)
  "Slot name → origin class string or global section label (e.g. Phantasia-Globals).
Bound to a hash table with @code{:test equalp} so copybook uppercase names match
parser mixed-case identifiers (see @code{load-copybook-tables}).")
(defvar *type-table* (make-hash-table :test 'equal))
(defvar *const-table* (make-hash-table :test 'equal)
  "77/78 constant name → integer. Bound to a hash table with @code{:test equalp}
(case-insensitive string keys; see @code{const-table-name-key}).")
(defvar *service-bank-table* (make-hash-table :test 'equal))
(defvar *usage-table* (make-hash-table :test 'equal))
(defvar *sign-table* (make-hash-table :test 'equal))
(defvar *pic-size-table* (make-hash-table :test 'equal))
(defvar *pic-nybble-table* (make-hash-table :test 'equal))
(defvar *pic-width-table* (make-hash-table :test 'equal))
(defvar *pic-frac-bits-table* (make-hash-table :test 'equal)
  "Variable-name → fractional bits (0..n). From USAGE BINARY WITH nVm BITS (m = fractional).")
(defvar *pic-nybble-semantics-table* (make-hash-table :test 'equal)
  "Variable-name → T when copybook PIC is single-digit @code{9}/@code{S9} (logical @code{#x0}--@code{#xF});
@code{NIL} in table means @code{PIC 99} or wider numeric picture. Storage is still byte-aligned.")

(defun cobol-slot-table-name-key (name)
  "Normalize COBOL identifier for @code{*slot-table*} and related gethash keys.

NAME is trimmed and uppercased so mixed-case sources match copybook rows."
  (string-upcase (string-trim " " (if (stringp name) name (format nil "~a" name)))))

(defun operand-nybble-semantics-p (name)
  "True when NAME is listed in @code{*pic-nybble-semantics-table*} (single-digit @code{PIC 9} row)."
  (and *pic-nybble-semantics-table*
       (let ((n (if (stringp name) name (format nil "~a" name))))
         (or (gethash (cobol-slot-table-name-key n) *pic-nybble-semantics-table*)
             (gethash n *pic-nybble-semantics-table*)))))

;;; In-memory service→bank LUT for CALL SERVICE resolution (no copybook file).
;;; Populated by build-service-bank-lut-from-banks when scanning Source/Code/{machine}/Banks.
(defvar *service-bank-lut* (make-hash-table :test 'equal)
  "In-memory LUT: service name (string) → bank symbol (string). Used for CALL SERVICE target.")

(defun build-service-bank-lut-from-banks (&optional (root-directory nil) (machine-directory nil))
  "Scan Source/Code/{MACHINE-DIR}/Banks/**/*.s and Common/Enums.s.
Populate *service-bank-lut* with service→bank mappings. MACHINE-DIR is e.g. \"7800\".

When ROOT-DIRECTORY is non-NIL (unit tests), paths are merged from it instead of
@code{*eightbol-root-directory*}. When MACHINE-DIRECTORY is non-NIL, it overrides
@code{infer-machine-from-copybook-paths}."
  (clrhash *service-bank-lut*)
  (let* ((m (or machine-directory (machine-directory-name)))
         (root (or root-directory *eightbol-root-directory* (truename "."))))
    (unless m
      (return-from build-service-bank-lut-from-banks *service-bank-lut*))
    (let* ((common (merge-pathnames
                    (make-pathname :directory `(:relative "Source" "Code" ,m "Common"))
                    root))
           (enums-path (merge-pathnames "Enums.s" common))
           (banks-dir (merge-pathnames
                       (make-pathname :directory `(:relative "Source" "Code" ,m "Banks"))
                       root))
           (bank-num->symbol ()))
      ;; Parse Enums.s for BankXxx = $YY
      (when (probe-file enums-path)
        (with-open-file (stream enums-path :direction :input)
          (loop for line = (read-line stream nil nil) while line do
            (cl-ppcre:register-groups-bind (sym hex)
                ("^\\s*\\b(Bank[-A-Za-z0-9_]+)\\s*=\\s*\\$([0-9a-fA-F]+)\\b" line)
              (when (and sym hex)
                (push (cons (parse-integer hex :radix 16) sym) bank-num->symbol))))))
      ;; Scan bank .s files for BANK = $NN and .Dispatch ServiceXxx, Label
      (when (and (probe-file banks-dir) bank-num->symbol)
        (setf bank-num->symbol (nreverse bank-num->symbol))
        (let ((bank-num->symbol-alist bank-num->symbol))
          (loop for n from 0 to 31
                for subdir = (merge-pathnames
                              (make-pathname :directory `(:relative ,(format nil "Bank~2,'0x" n)))
                              banks-dir)
                when (probe-file subdir)
                  do (dolist (path (directory (merge-pathnames
                                               (make-pathname :name :wild :type "s")
                                               subdir)))
                       (let ((bank-num nil)
                             (bank-sym nil))
                         (ignore-errors
                          (with-open-file (stream path :direction :input)
                            (loop for line = (read-line stream nil nil) while line do
                              (cl-ppcre:register-groups-bind (hex)
                                  ("^\\s*BANK\\s*=\\s*\\$([0-9a-fA-F]+)\\b" line)
                                (when hex
                                  (setf bank-num (parse-integer hex :radix 16))
                                  (setf bank-sym (cdr (assoc bank-num bank-num->symbol-alist)))))
                              (cl-ppcre:register-groups-bind (service)
                                  ("^\\s*\\.Dispatch\\s+(Service[-A-Za-z0-9_]+)\\s*," line)
                                (when (and service bank-sym)
                                  (setf (gethash service *service-bank-lut*) bank-sym)))))))))))
      (when (and (zerop (hash-table-count *service-bank-lut*))
                 (probe-file banks-dir))
        (error "EIGHTBOL: empty service:bank LUT for machine"))
      *service-bank-lut*)))

(defun machine-directory-name ()
  (infer-machine-from-copybook-paths))

(defun infer-machine-from-copybook-paths ()
  "Infer machine directory (e.g. 7800) from *copybook-paths*.
Looks for .../Generated/NNNN/Classes or .../7800/Classes in paths."
  (when *copybook-paths*
    (dolist (dir *copybook-paths*)
      (let* ((p (if (pathnamep dir) dir (pathname dir)))
             (path (pathname-directory (merge-pathnames p (or *eightbol-root-directory* (truename "."))))))
        (when (listp path)
          (let ((pos (position "Generated" path :test #'equal)))
            (when (and pos (< (1+ pos) (length path)))
              (let ((cand (nth (1+ pos) path)))
                (when (and (stringp cand) (plusp (length cand))
                           (every (lambda (c) (or (digit-char-p c) (char-equal c #\.))) cand))
                  (return-from infer-machine-from-copybook-paths cand)))))))))
  nil)

(defun pic-nybble-semantics-p (pic-rest)
  "True when PIC-REST describes a single decimal digit picture (@code{PIC 9} or @code{PIC S9}),
so logical values are @code{#x0}--@code{#xF}. @code{PIC 99}, @code{PIC 9(n)}, @code{PIC X}, and
@code{PIC 9(1)}-style repetition use full-byte or multi-byte rules instead.

@table @asis
@item PIC-REST
Copybook fragment containing @code{PIC} / @code{PICTURE} through @code{USAGE} or end of clause.
@end table

@subsection Outputs
Boolean."
  (when (and (stringp pic-rest)
             (not (cl-ppcre:scan "(?i)(?:PIC|PICTURE)\\s+S?X" pic-rest))
             (not (cl-ppcre:scan "(?i)(?:PIC|PICTURE)\\s+S?9\\s*\\(" pic-rest)))
    (cl-ppcre:register-groups-bind (sign run)
        ("(?i)(?:PIC|PICTURE)\\s+(S?)(9+)" pic-rest)
      (declare (ignore sign))
      (and run (= (length run) 1)))))

(defun %pic-strip-leading-picture-keyword (pic-str)
  "Strip leading @code{PIC}/@code{PICTURE} keyword from PIC-STR when present; trim spaces."
  (if (not (stringp pic-str))
      ""
      (string-trim " " (cl-ppcre:regex-replace "(?i)^(PIC|PICTURE)\\s+" pic-str ""))))

(defun %pic-count-9-x-slots (segment)
  "Count display digit positions in SEGMENT: each @code{9} or @code{X} counts 1; @code{9(n)} / @code{X(n)} count N.
Ignores @code{.}, @code{V}, sign @code{S}, and other COBOL picture punctuation."
  (let ((i 0)
        (n (length segment))
        (total 0))
    (loop while (< i n)
          do (let ((c (char-upcase (char segment i))))
               (cond
                 ((or (char= c #\9) (char= c #\X))
                  (if (and (< (1+ i) n) (char= (char segment (1+ i)) #\())
                      (let ((close (position #\) segment :start (+ i 2))))
                        (unless close
                          (return-from %pic-count-9-x-slots total))
                        (incf total (parse-integer (subseq segment (+ i 2) close)))
                        (setf i (1+ close)))
                      (progn
                        (incf i)
                        (incf total 1))))
                 (t
                  (incf i)))))
    total))

(defun pic-fractional-decimal-digits (pic-str)
  "Return count of digit positions (@code{9}/@code{X} and @code{9(n)}) right of the first @code{V} or @code{.}.

Used for implied-decimal alignment on ADD/SUBTRACT/COMPUTE: packed integers are scaled by
@code{10^-k} where @code{k} is this count. When no radix appears, return 0.

@table @asis
@item PIC-STR
Copybook @code{PIC} clause or parser picture string (with or without @code{PIC} keyword).
@end table

@subsection Outputs
Non-negative integer."
  (unless (stringp pic-str)
    (return-from pic-fractional-decimal-digits 0))
  (let ((s (string-upcase (%pic-strip-leading-picture-keyword pic-str))))
    (let ((radix (or (position #\V s) (position #\. s))))
      (if radix
          (%pic-count-9-x-slots (subseq s (1+ radix)))
          0))))

(defun fixed-pic-decimal-aligned-sum-integer (from-int from-frac-digits to-int to-frac-digits result-frac-digits)
  "Return integer sum aligned to RESULT-FRAC-DIGITS implied decimal places.

Computes @code{from-int * 10^(result-from) + to-int * 10^(result-to)} with non-negative
exponents (operands widened to the result scale). FROM-INT and TO-INT are packed digit
integers at their respective PICTURE scales.

@table @asis
@item FROM-INT, TO-INT
Unsigned digit concatenations (e.g. @code{PIC 99.9999} max @code{999999}).
@item FROM-FRAC-DIGITS, TO-FRAC-DIGITS, RESULT-FRAC-DIGITS
Fractional decimal digit counts from @code{pic-fractional-decimal-digits}.
@end table

@subsection Outputs
Integer sum before any final modulo by the result field width (caller must clip/MOVE)."
  (let ((dr result-frac-digits)
        (df from-frac-digits)
        (dt to-frac-digits))
    (+ (* from-int (expt 10 (- dr df)))
       (* to-int (expt 10 (- dr dt))))))

(defun fixed-pic-decimal-aligned-diff-integer (from-int from-frac sub-int sub-frac result-frac)
  "Return integer @code{FROM - SUB} at RESULT-FRAC implied decimal places.

Each packed integer is at its PICTURE scale; widened to RESULT-FRAC before subtract.

@table @asis
@item FROM-INT, SUB-INT
Unsigned digit concatenations at their respective PICTURE scales.
@item FROM-FRAC, SUB-FRAC, RESULT-FRAC
Fractional digit counts from @code{pic-fractional-decimal-digits}.
@end table

@subsection Outputs
Integer difference before any modulo by the result field width."
  (let ((dr result-frac)
        (df from-frac)
        (ds sub-frac))
    (- (* from-int (expt 10 (- dr df)))
       (* sub-int (expt 10 (- dr ds))))))

(defun pic-digits-to-width (pic-rest)
  "From PIC clause rest, return byte width (1–8) for packed storage in RAM.
Examples: 1 byte = S9/99 USAGE DECIMAL or BINARY, S99 USAGE BINARY;
2 bytes = 9999/9(4)/S9(4); up to 8 bytes = 9(8)/S9(8) USAGE BINARY or DECIMAL.
Formula: max(1, ceiling(digits/2)). For BCD, S9 = 1 byte (sign nybble + 1 digit).
Single-digit @code{PIC 9} and two-digit @code{PIC 99} both allocate one byte in
generated copybooks; runtime nybble packing (where used) is documented in Phantasia
class design notes."
  (or
   ;; Dotted or V implied decimal first: @code{(\\d+)} would otherwise match only digits
   ;; before @code{.} (e.g. @code{PIC 999999.999999} → 3 bytes instead of 6).
   (when (and (stringp pic-rest)
              (let ((s (%pic-strip-leading-picture-keyword pic-rest)))
                (or (find #\. s) (find #\V s))))
     (max 1 (ceiling (%pic-count-9-x-slots (%pic-strip-leading-picture-keyword pic-rest)) 2)))
   (cl-ppcre:register-groups-bind (digits n)
       ("(?i)(?:PIC|PICTURE)\\s+(?:(\\d+)|9\\s*\\(\\s*(\\d+)\\s*\\))" pic-rest)
     (let ((d (or (when digits (length digits)) (when n (parse-integer n)))))
       (when d (max 1 (ceiling d 2)))))
   ;; PIC(99) / PICTURE(9) parenthesized digit count (AssetIDs.cpy style)
   (cl-ppcre:register-groups-bind (n)
       ("(?i)(?:PIC|PICTURE)\\s*\\(\\s*(\\d+)\\s*\\)" pic-rest)
     (let ((d (parse-integer n)))
       (max 1 (ceiling d 2))))
   ;; S9, S99, S9(n) — signed; for BCD, S9 = 1 byte (1 digit + sign nybble)
   (cl-ppcre:register-groups-bind (sdigits sn)
       ("(?i)(?:PIC|PICTURE)\\s+S(?:(\\d+)|9\\s*\\(\\s*(\\d+)\\s*\\))" pic-rest)
     (let ((d (or (when sdigits (length sdigits)) (when sn (parse-integer sn)))))
       (when d (max 1 (ceiling d 2)))))))

(defun oops-class-of (symbol)
  (if (string-equal "Self" symbol)
      *class-id*
      (let ((var (gethash symbol *working-storage*)))
        (case (getf var :usage)
          ((:object :object-ref)
           (getf var :class))
          (otherwise (error "Unable to find reference class for ~a: ~s" symbol var))))))

(defun slot-table-origin-lookup (name symbol)
  "Resolve copybook origin for NAME using canonical key, raw NAME, then class-qualified strip.

When the parser emits @code{Class-Global-Name} and the copybook row is @code{Global-Name}
under Phantasia-Globals, stripping the @code{*CLASS-ID*} prefix finds the same row."
  (slot-class symbol name))

(defun service-bank-table-lookup (name)
  "Resolve bank symbol for service NAME from @code{*service-bank-table*}.
Tries NAME, then @code{cobol-slot-table-name-key} (matches copybook / LUT key forms),
then paired @code{ServiceMoveDecalX} / @code{ServiceMoveDecalY} bank.

@table @asis
@item NAME
Assembly-style service label (e.g. @code{ServiceMoveDecalY}) or COBOL form.
@end table

@subsection Outputs
Bank symbol string (e.g. @code{BankAnimation}), or NIL."
  (when (zerop (hash-table-count *service-bank-lut*))
    (build-service-bank-lut-from-banks))
  (gethash name *service-bank-lut*))

(defun cobol-constant-to-assembly-symbol (name)
  "Map COBOL 77/78 constant name to assembly (no class prefix).

Double hyphen @code{--} separates groups; groups are joined with underscore @code{_}.
Within each group, single hyphens separate word parts (see @code{cobol-hyphenated-to-pascal-concat}).
Examples: Song--Hurt--ID → Song_Hurt_ID; Hurt-H-P → HurtHP.

@table @asis
@item NAME
Constant name as in the copybook / source.
@end table

@subsection Outputs
Assembly symbol string suitable after @code{# } in immediates or as an equate label."
  (let ((s (string-trim " " (if (stringp name) name (format nil "~a" name)))))
    (when (zerop (length s))
      (return-from cobol-constant-to-assembly-symbol ""))
    (let ((groups (remove-if (lambda (g) (zerop (length (string-trim " " g))))
                             (cl-ppcre:split "--" s))))
      (if (null groups)
          (pascal-case s)
          (format nil "~{~a~^_~}"
                  (mapcar #'pascal-case groups))))))

(defun cobol-global-data-name-to-assembly-symbol (name)
  "Map bare COBOL data name (globals, WORKING-STORAGE) to assembly without class prefix.

Instance slots use @code{slot-symbol} only in @code{OF} contexts (e.g. H-P OF Self →
offset @code{CharacterHP}); bare identifiers here are globals like Next-Song → NextSong.

@table @asis
@item NAME
Data name from source.
@end table

@subsection Outputs
Single concatenated symbol (no @code{--} → underscore rule)."
  (pascal-case name))

(defun cobol-hyphenated-to-pascal-concat (name)
  "Join COBOL hyphen-separated words into one PascalCase identifier (no hyphens).

When NAME contains no hyphens, return @code{pascal-case} of NAME; otherwise split on
hyphens, apply @code{pascal-case} to each segment, and concatenate (e.g. Decal-Animation-On-Tick
→ DecalAnimationOnTick).

@table @asis
@item NAME
COBOL identifier or already-camel token string.
@end table

@subsection Outputs
Single assembly-safe symbol string."
  (let ((s (string-trim " " (if (stringp name) name (format nil "~a" name)))))
    (if (find #\- s)
        (apply #'concatenate 'string
               (mapcar #'pascal-case (split-sequence #\- s :remove-empty-subseqs t)))
        (pascal-case s))))

(defun implicit-instance-slot-p (slot-name class-id)
  "True when SLOT-NAME is an instance field of CLASS-ID per @code{*slot-table*} origin.

Globals (e.g. Phantasia-Globals) yield NIL when origin does not match CLASS-ID.

@table @asis
@item SLOT-NAME
COBOL data name (hyphenated spelling).
@item CLASS-ID
Declaring class id string (e.g. @code{\"TestClass\"}).
@end table

@subsection Outputs
Boolean."
  (when (and *slot-table* slot-name class-id)
    (let ((origin (gethash (cobol-slot-table-name-key slot-name) *slot-table*)))
      (and origin (string-equal origin class-id)))))

(defun slot-origin-global-data-p (origin)
  "True if copybook ORIGIN names global/section data (e.g. Phantasia-Globals), not an OOPS class slot block.

@table @asis
@item ORIGIN
String from slot table (copybook section), or NIL.
@end table

@subsection Outputs
Boolean; NIL means treat as class layout origin (Character, Entity, …).
NIL origin means “unknown” — not global; see @code{bare-data-assembly-symbol}."
  (let ((s (format nil "~a" origin)))
    (or (search "Global" s :test #'char-equal)
        (search "SECTION" s :test #'char-equal)
        (string-equal s "EXTERNAL"))))

(defun slot-origin-class (slot-name object &optional (class-id (if (string-equal "Self" object)
                                                                   *class-id*
                                                                   object)))
  "Return the origin class for SLOT-NAME."
  (slot-class object slot-name class-id))

(defun slot-name-to-assembly-segment (name)
  (pascal-case name))

(defun slot-symbol (slot-name object-name
                    &optional
                      (cast-class (slot-class object-name slot-name)))
  "Return assembly copybook offset symbol for SLOT-NAME. Uses *SLOT-TABLE*
for origin class."
  (let* ((origin cast-class))
    (when (and (listp slot-name) (eql :subscript (first slot-name)))
      (setf slot-name (second slot-name)))
    (concatenate 'string (pascal-case origin) (pascal-case slot-name))))

(defun bare-data-assembly-symbol (name class-id)
  "Assembly symbol for bare data NAME in compilation context of CLASS-ID.

Uses @code{slot-symbol} for implicit instance slots; otherwise @code{cobol-global-data-name-to-assembly-symbol}.

@table @asis
@item NAME
COBOL data name.
@item CLASS-ID
Current class id (binds @code{*class-id*} for @code{slot-symbol} of Self).
@end table

@subsection Outputs
Identifier string without leading class prefix when data is global / external."
  (let ((*class-id* class-id))
    (if (implicit-instance-slot-p name class-id)
        (slot-symbol name "Self")
        (cobol-global-data-name-to-assembly-symbol name))))

(defun cobol-double-hyphen-grouped-name-p (name)
  "True when NAME uses COBOL 77/78 grouped spelling with @code{--} (e.g. @code{Song--Hurt--ID}).

These are manifest constants in Phantasia, never per-instance OOPS slots; they must not use
@code{ldy #Class…} / @code{lda (Self),y} when @code{*CONST-TABLE*} lacks a row (e.g. partial compile)."
  (and (stringp name)
       (search "--" name)))

(defun const-table-name-key (name)
  "Trimmed identifier for @code{*const-table*} @code{gethash}/@code{setf}.

@code{*const-table*} uses @code{equalp}, so string keys match case-insensitively;
only surrounding space is normalized."
  (string-trim " " (if (stringp name) name (format nil "~a" name))))

(defun var-class (var-name)
  "Return the OBJECT REFERENCE class for VAR-NAME, or NIL. Uses *TYPE-TABLE* then Phantasia fallbacks."
  (oops-class-of var-name))

(defun %const-table-resolve (name)
  "Return @code{(values integer-value cobol-name-for-assembly)} or @code{(values nil nil)}.

Looks up @code{*CONST-TABLE*} by trimmed name, canonical slot key, and by stripping a leading
@code{Class-} prefix when @code{*CLASS-ID*} is bound (matches unqualified copybook 77/78 names)."
  (getf (gethash name *working-storage*) :value))

(defun constant-value (name)
  "Return the integer value of constant NAME, or NIL. Uses *CONST-TABLE*."
  (%const-table-resolve name))

(defun constant-p (name)
  "True if NAME is a known compile-time constant."
  (constantp name))

(defun constant-cobol-name-for-assembly (name)
  "Return the COBOL identifier to pass to @code{cobol-constant-to-assembly-symbol} for NAME.

When NAME is class-qualified (e.g. @code{Character-Song--Heal--ID}) and the table holds
@code{Song--Heal--ID}, returns the unqualified form so @code{--} groups map to @code{Song_Heal_ID}.

@table @asis
@item NAME
Bare constant name as in the AST (must satisfy @code{constant-p}).
@end table

@subsection Outputs
String suitable for @code{cobol-constant-to-assembly-symbol}."
  (multiple-value-bind (val sym) (%const-table-resolve name)
    (declare (ignore val))
    (or sym name)))

(defun expression-slot-of-slot-name (expression)
  "Return slot data name for @code{*pic-width-table*} when EXPRESSION is slot OF object.

Parser @code{(data-name of data-name)} yields @code{(\"HP\" \"OF\" \"Self\")}; backend
prefers @code{(:of \"HP\" :self)}. Both must resolve the same PIC width (e.g. 16-bit HP).

@table @asis
@item EXPRESSION
Expression AST leaf or nested form.
@end table

@subsection Outputs
Slot name string (e.g. @code{\"Max-HP\"}), or NIL if not a slot-OF form."
  (when (listp expression) (eql :of (first expression))
        (second expression)))

(defun expression-to-width-name (expression)
  "Return variable name for pic-width lookup. NIL for literals/constants."
  (cond
    ((stringp expression) (format nil "~a" expression))
    ((and (listp expression) (eq (first expression) :subscript))
     (format nil "~a" (second expression)))
    (t (expression-slot-of-slot-name expression))))

(defun object-reference-storage-width ()
  "Return pointer width for OBJECT REFERENCE; matches @code{+object-reference-storage-width+}."
  +object-reference-storage-width+)

(defun operand-signed-p (expression)
  (cond
    ((numberp expression)
     (cond
       ((minusp expression) t)
       (t nil)))
    ((eql :null expression) nil)
    ((eql :zero expression) nil)
    (t (let ((token (slot-token expression)))
         (when-let (var (gethash token *working-storage*))
           (getf var :signed nil))))))

(defun operand-bcd-p (expression)
  (cond
    ((numberp expression) nil)
    ((eql :zero expression) nil)
    (t (let ((token (slot-token expression)))
         (when-let (var (gethash token *working-storage*))
           (eql :decimal (getf var :usage nil)))))))

(defun usage-bcd-p (expression) (operand-bcd-p expression))

(defun pic-length-bytes (pic)
  (ceiling (if (find #\( pic)
               (cl-ppcre:register-groups-bind (count) ("[9X]\\(([0-9]+)\\)" pic)
                 (parse-integer count))
               (count-if (lambda (ch) (find ch "9X")) pic))
           2))

(defun slot-token (expression)
  (cond ((and (listp expression) (eql :of (first expression)))
         (list :of (second expression)
               (if (< 3 (length expression))
                   (fourth expression)
                   (slot-class (third expression) (second expression)))))
        ((and (listp expression) (eql :on (first expression)))
         (list :of (second expression)
               (if (> 3 (length expression))
                   (fourth expression)
                   (slot-class (third expression) (second expression)))))
        ((and (listp expression) (eql :subscript (first expression)))
         (slot-token (second expression)))
        (t expression)))

(defun %operand-pic-fractional-decimal-digits (expression)
  "Fractional decimal digit count from EXPRESSION's @code{:pic} in @code{*working-storage*}, or 0.

Literals and unresolved names yield 0."
  (handler-case
      (pic-fractional-decimal-digits
       (or (when-let (tok (slot-token expression))
             (when-let (var (gethash tok *working-storage*))
               (getf var :pic)))
           ""))
    (error () 0)))

(defun add-picture-decimal-scales-mismatch-p (giving result from to-op)
  "True when ADD mixes implied decimal (@code{V}/@code{.}) fractional digit counts.

With @code{GIVING}, the result field's fractional digit count differs from an operand (each
operand must be scaled to the result picture). Without @code{GIVING}, the addend and target
differ in fractional digit count."
  (let ((dr (%operand-pic-fractional-decimal-digits result))
        (df (%operand-pic-fractional-decimal-digits from))
        (dt (%operand-pic-fractional-decimal-digits to-op)))
    (if giving
        (or (/= dr df) (/= dr dt))
        (/= df dt))))

(defun subtract-picture-decimal-scales-mismatch-p (giving result subtrahend minuend)
  "True when SUBTRACT mixes implied decimal (@code{V}/@code{.}) fractional digit counts.

With @code{GIVING}, the result's fractional digit count differs from an operand. Without
@code{GIVING}, the minuend and subtrahend differ in fractional digit count.

@table @asis
@item MINUEND
The value @emph{after} @code{FROM} in @code{SUBTRACT @dots{} FROM @dots{}} (parser @code{:from}, or
legacy @code{:from-target} when @code{:subtrahend} is absent).
@item SUBTRAHEND
The value subtracted (parser @code{:subtrahend}, or legacy @code{:from} when @code{:subtrahend}
is absent).
@end table"
  (let ((dr (%operand-pic-fractional-decimal-digits result))
        (ds (%operand-pic-fractional-decimal-digits subtrahend))
        (dm (%operand-pic-fractional-decimal-digits minuend)))
    (if giving
        (or (/= dr ds) (/= dr dm))
        (/= ds dm))))

(defun subtract-statement-minuend-and-subtrahend (stmt)
  "Return @code{(values MINUEND SUBTRAHEND)} for a @code{:subtract} statement plist.

Supports parser output (@code{:subtrahend} and @code{:from}) and legacy tests that use only
@code{:from} (subtrahend) and @code{:from-target} (minuend).

@table @asis
@item STMT
@code{(:subtract @dots{})} plist.
@end table"
  (let ((plist (rest stmt))
        (s (getf plist :subtrahend))
        (f (getf plist :from))
        (ft (getf plist :from-target)))
    (if s
        (values (or f ft) s)
        (values ft f))))

(defun pic-decimal-binary-add-scaling-supported-p (giving from to-op)
  "True when misaligned implied-decimal ADD can use widen-only binary scaling (multiply by @code{10^n}).

@code{n} is bounded by 12 per operand path. Narrowing (divide by @code{10^n}) is not supported.

@table @asis
@item GIVING
When non-NIL, result scale @code{dr} must be @code{>=} each operand scale (@code{dr-df},
@code{dr-dt} in @code{0..12}). When NIL, target scale @code{dt} must be @code{>=} addend
scale @code{df} with @code{dt-df} in @code{0..12}.
@item FROM, TO-OP
ADD addend and target (from @code{*working-storage*} @code{:pic} strings).
@end table

@subsection Outputs
Generalized Boolean."
  (let ((dr (%operand-pic-fractional-decimal-digits (if giving (or giving to-op) to-op)))
        (df (%operand-pic-fractional-decimal-digits from))
        (dt (%operand-pic-fractional-decimal-digits to-op)))
    (if giving
        (and (>= dr df) (>= dr dt)
             (<= (- dr df) 12)
             (<= (- dr dt) 12))
        (and (>= dt df) (<= (- dt df) 12)))))

(defun pic-decimal-binary-subtract-scaling-supported-p (giving minuend subtrahend)
  "True when misaligned implied-decimal SUBTRACT can use widen-only binary scaling.

Same @code{10^n} bound (12) as @code{pic-decimal-binary-add-scaling-supported-p}.

@table @asis
@item GIVING
When non-NIL, widen minuend and subtrahend to result fractional digit count. When NIL, widen
subtrahend to minuend scale only.
@item MINUEND, SUBTRAHEND
Packed values at their respective PICTURE scales.
@end table

@subsection Outputs
Generalized Boolean."
  (let ((dr (%operand-pic-fractional-decimal-digits (if giving
                                                        (or giving minuend)
                                                        minuend)))
        (dm (%operand-pic-fractional-decimal-digits minuend))
        (ds (%operand-pic-fractional-decimal-digits subtrahend)))
    (if giving
        (and (>= dr dm) (>= dr ds)
             (<= (- dr dm) 12)
             (<= (- dr ds) 12))
        (and (>= dm ds) (<= (- dm ds) 12)))))

(defun %cpu-has-6502-pic-decimal-scaling-p (cpu)
  "True when CPU shares the 6502-family backend that implements widen-only PIC decimal ADD/SUBTRACT."
  (member cpu '(:6502 :rp2a03 :65c02 :65c816 :huc6280) :test #'eq))

(defun pic-decimal-add-scales-uncompiled-on-this-cpu-p (cpu giving from to-op)
  "True when ADD has mismatched implied-decimal scales and CPU cannot emit correct widen-only code.

6502-family with USAGE BINARY and @code{pic-decimal-binary-add-scaling-supported-p} is compiled;
other CPUs signal @code{backend-error}. USAGE DECIMAL misaligned is rejected on 6502 as well."
  (let ((result (or giving to-op)))
    (and (add-picture-decimal-scales-mismatch-p giving result from to-op)
         (not (and (%cpu-has-6502-pic-decimal-scaling-p cpu)
                   (not (operand-bcd-p from))
                   (not (operand-bcd-p to-op))
                   (not (operand-bcd-p result))
                   (pic-decimal-binary-add-scaling-supported-p giving from to-op))))))

(defun pic-decimal-subtract-scales-uncompiled-on-this-cpu-p (cpu giving minuend subtrahend)
  "True when SUBTRACT has mismatched implied-decimal scales and CPU cannot emit correct widen-only code."
  (let ((result (or giving minuend)))
    (and (subtract-picture-decimal-scales-mismatch-p giving result subtrahend minuend)
         (not (and (%cpu-has-6502-pic-decimal-scaling-p cpu)
                   (not (operand-bcd-p minuend))
                   (not (operand-bcd-p subtrahend))
                   (not (operand-bcd-p result))
                   (pic-decimal-binary-subtract-scaling-supported-p giving minuend subtrahend))))))

(defun assert-pic-decimal-add-compiled (cpu stmt)
  "Signal @code{backend-error} when STMT is ADD with misaligned PIC decimals this CPU cannot compile.

@table @asis
@item CPU
Backend keyword (e.g. @code{:z80}, @code{:6502}).
@item STMT
Statement plist whose @code{car} is @code{:add}.
@end table"
  (let ((from (getf (rest stmt) :from))
        (to-op (getf (rest stmt) :to))
        (giving (getf (rest stmt) :giving)))
    (when (pic-decimal-add-scales-uncompiled-on-this-cpu-p cpu giving from to-op)
      (error 'backend-error
             :message
             "ADD mixes implied decimal (PIC V/.) fractional scales; widen-only scaling is implemented only for 6502-family USAGE BINARY (or use matching fractional digit counts)"
             :cpu cpu
             :detail (list :add stmt :from from :to to-op :giving giving)))))

(defun assert-pic-decimal-subtract-compiled (cpu stmt)
  "Signal @code{backend-error} when STMT is SUBTRACT with misaligned PIC decimals this CPU cannot compile."
  (multiple-value-bind (minuend subtrahend) (subtract-statement-minuend-and-subtrahend stmt)
    (let ((giving (getf (rest stmt) :giving)))
      (when (pic-decimal-subtract-scales-uncompiled-on-this-cpu-p cpu giving minuend subtrahend)
        (error 'backend-error
               :message
               "SUBTRACT mixes implied decimal (PIC V/.) fractional scales; widen-only scaling is implemented only for 6502-family USAGE BINARY (or use matching fractional digit counts)"
               :cpu cpu
               :detail (list :subtract stmt :minuend minuend :subtrahend subtrahend :giving giving))))))

(defun operand-width (expression &optional (pic-width-table *pic-width-table*))
  "Return byte width (1–8) for EXPRESSION."
  (cond
    ((numberp expression) (cond
                            ((zerop expression) 1)
                            ((minusp expression) (ceiling (log (1+ (abs expression)) 2) 8))
                            (t (ceiling (1+ (log expression 2)) 8))))
    ((eql :null expression) 1)
    ((eql :zero expression) 1)
    ((eql :self expression) 2)
    ((equalp '(:literal :self) expression) 2)
    ((and (listp expression) (member (first expression) '(:low :high))) 1)
    ((and (listp expression) (member (first expression) '(:address-of))) 2)
    ((and (listp expression)
          (string= "(" (first expression))
          (string= ")" (lastcar expression)))
     (if (= 3 (length expression))
         (second expression)
         (loop for el in (subseq expression 1 (1- (length expression)))
               maximize (operand-width el pic-width-table))))
    ((and (listp expression) (member (first expression) '(:bit-and :bit-or :bit-xor)))
     (loop for el in (subseq expression 1)
           maximize (operand-width el pic-width-table)))
    (t (let ((token (slot-token expression)))
         (if-let (var (gethash token *working-storage*))
           (ecase (getf var :usage)
             (:binary
              (pic-length-bytes (getf var :pic)))
             (:decimal
              (pic-length-bytes (getf var :pic)))
             (:pointer 2)
             (:procedure-pointer 2)
             (:object-ref 2))
           (cond
             ((and *type-table* (gethash token *type-table*))
              +object-reference-storage-width+)
             (t 1)))))))

(defun expression-operand-width (expression &optional (pic-width-table *pic-width-table*))
  "Return max byte width (1–8) for EXPRESSION and its leaves.

For composite arithmetic (@code{:add-expression}, @code{:subtract-expression}, @dots{}) returns
the maximum of recursive leaf widths. For other forms, @code{operand-width}.
Use when MOVE/ADD/SUB sources may be expressions so width matches the widest operand.

@table @asis
@item EXPRESSION
Expression AST or bare identifier.
@item PIC-WIDTH-TABLE
Optional width table (defaults to @code{*pic-width-table*}).
@end table

@subsection Outputs
Integer byte count at least 1."
  (labels ((rec (e)
             (cond
               ((not (listp e)) (operand-width e))
               ((eql :literal (first e))
                (expression-operand-width (second e)))
               ((eql :add (first e))
                (max (rec (getf (rest e) :from)) (rec (getf (rest e) :to))))
               ((eql :subtract (first e))
                (max (rec (getf (rest e) :from)) (rec (getf (rest e) :subtrahend))))
               ((eql :mulitply (first e))
                (max (rec (getf (rest e) :multiplier)) (rec (getf (rest e) :by))))
               ((eql :divide (first e))
                (max (rec (getf (rest e) :numerator)) (rec (getf (rest e) :denominator))))
               ((member (first e)
                        '(:shift-left :shift-right :bit-and :bit-or :bit-xor))
                (max (rec (second e)) (rec (third e))))
               ((and (string= "(" (first e))
                     (string= ")" (lastcar e)))
                (loop for el in (subseq e 1 (1- (length e)))
                      maximize (rec el)))
               ((eq (first e) :bit-not)
                (rec (second e)))
               (t (operand-width e pic-width-table)))))
    (max 1 (rec expression))))

(defun expression-operand-signed-p (expression)
  "Return max byte width (1–8) for EXPRESSION and its leaves.

For composite arithmetic (@code{:add-expression}, @code{:subtract-expression}, @dots{}) returns
the maximum of recursive leaf widths. For other forms, @code{operand-width}.
Use when MOVE/ADD/SUB sources may be expressions so width matches the widest operand.

@table @asis
@item EXPRESSION
Expression AST or bare identifier.
@item PIC-WIDTH-TABLE
Optional width table (defaults to @code{*pic-width-table*}).
@end table

@subsection Outputs
Integer byte count at least 1."
  (labels ((rec (e)
             (cond
               ((not (listp e)) (operand-signed-p e))
               ((eql :literal (first e))
                (rec (second e)))
               ((member (first e)
                        '(:add :subtract :multiply :divide
                          :shift-left :shift-right :bit-and :bit-or :bit-xor)
                        :test #'eq)
                (or (rec (second e)) (rec (third e))))
               ((and (string= "(" (first e))
                     (string= ")" (lastcar e)))
                (some (lambda (el) (rec el)) (subseq e 1 (1- (length e)))))
               ((eq (first e) :bit-not)
                (rec (second e)))
               (t nil))))
    (rec expression)))

(defun pic-frac-bits (name)
  "Return fractional bit count for NAME from @code{*pic-frac-bits-table*}, or NIL.
NAME is a string or symbol; keys match copybook canonical names."
  (and *pic-frac-bits-table*
       (let ((n (if (stringp name) name (format nil "~a" name))))
         (or (gethash (cobol-slot-table-name-key n) *pic-frac-bits-table*)
             (gethash n *pic-frac-bits-table*)))))

(defun %string-blt-refmod-length (operand)
  "Return @code{:length} from STRING BLT operand: @code{(:refmod …)} plist, else plain plist, else NIL."
  (when (listp operand)
    (if (eq (first operand) :refmod)
        (getf (rest operand) :length)
        (getf operand :length))))

(defun backend-unsupported-operand-width (cpu-name op-name width max-supported)
  "Signal an error when WIDTH exceeds MAX-SUPPORTED for CPU-NAME on OP-NAME (multi-byte codegen)."
  (when (> width max-supported)
    (error "EIGHTBOL/~a: ~a operand width ~d bytes not supported (max ~d)"
           cpu-name op-name width max-supported)))

(defun %sanitize-cobol-source-for-assembly-comment (text)
  "Return TEXT safe to embed after @code{;} in 64tass-style assembly.

Collapses line breaks to spaces"
  (cl-ppcre:regex-replace-all "[[:space:]]" text " "))

(defun emit-assembly-source-line-comment (stream plist)
  "Emit a semicolon comment mapping generated assembly to COBOL source when PLIST
contains @code{:source-text}, @code{:source-line}, @code{:source-sequence}, or
@code{:source-file} (from parser @code{stmt-with-source-location}).

@table @asis
@item STREAM
Where assembly text is written; comment uses @code{;} (64tass, Z80, et al.).
@item PLIST
Statement tail (e.g. @code{:from} … @code{:source-line} …), not the leading keyword.
@end table

@subsection Outputs
Writes zero or one comment line to STREAM. When @code{:source-text} is present, it is
appended after file / seq / line metadata (sanitized)."
  (when (and stream plist (listp plist))
    (let ((file (safe-getf plist :source-file))
          (line (safe-getf plist :source-line))
          (seq (safe-getf plist :source-sequence))
          (src-safe (and (safe-getf plist :source-text)
                         (%sanitize-cobol-source-for-assembly-comment
                          (safe-getf plist :source-text)))))
      (when (or line file seq src-safe)
        (let ((meta-parts
                (remove nil
                        (list (when file
                                (handler-case
                                    (if (pathnamep file)
                                        (namestring file)
                                        (princ-to-string file))
                                  (error () (princ-to-string file))))
                              (when seq (format nil "seq ~a" seq))
                              (when line (format nil "line ~a" line)))))
              (trimmed (when src-safe (string-trim " " src-safe))))
          (when (or meta-parts trimmed)
            (format stream "~&; ~{~a~^ ~}~@[ ~a~]~%"
                    (or meta-parts (list ""))
                    (when (and trimmed (plusp (length trimmed))) trimmed))))))))

(defun emit-statement-source-comment (stream stmt)
  "Call @code{emit-assembly-source-line-comment} on @code{(rest stmt)} for a full
statement node @code{(:keyword ...)}."
  (when (and stream (listp stmt) (rest stmt))
    (emit-assembly-source-line-comment stream (rest stmt))))

;;; Statement compilation — generic dispatch on CPU and AST node type
;;;
;;; (compile-statement cpu ast-node-symbol ast-node-data)
;;; Specializes on cpu and ast-node-symbol only. Uses *output-stream*.
;;; Each CPU defines brief methods for each statement type it supports.

(defgeneric compile-statement (cpu ast-node-symbol ast-node-data)
  (:documentation "Compile one statement. Dispatches on CPU and statement type. Uses *output-stream*."))

(defmethod compile-statement (cpu ast-node-symbol ast-node-data)
  (declare (ignore ast-node-data))
  (error "compile-statement: no method for CPU ~s statement ~s" cpu ast-node-symbol))

(defmethod compile-statement (cpu (stmt-type (eql :service-bank)) ast-node-data)
  "Reject @code{:service-bank} if it appears as a procedure statement (copybook metadata only).

@table @asis
@item CPU
Ignored; error applies to all backends.
@item STMT-TYPE
Always @code{:service-bank}.
@item AST-NODE-DATA
Ignored.
@end table

@subsection Outputs
Signals an error (no successful return)."
  (declare (ignore cpu ast-node-data))
  (error "EIGHTBOL: :service-bank is copybook metadata, not a procedure statement (corrupt AST)"))

;;; Legacy helpers kept for backwards compatibility

(defun build-slot-table (data-items)
  "Build slot-origin table from AST :data items (fallback when no copybook)."
  (let ((table (make-hash-table :test 'equalp))
        (current-origin nil))
    (dolist (item (ensure-list data-items))
      (when (and (listp item) (eq (first item) 'comment))
        (let ((text (if (stringp (second item)) (second item) "")))
          (cl-ppcre:register-groups-bind (c)
	    ("Inherited from (\\S+):" text)
	  (setf current-origin c))
          (cl-ppcre:register-groups-bind (c)
	    ("Own slots \\((\\S+)\\)" text)
	  (setf current-origin c))))
      (when (and (listp item) (numberp (first item))
                 (stringp (second item)))
        (setf (gethash (cobol-slot-table-name-key (second item)) table) current-origin)))
    table))

(defun build-type-table (data-items)
  "Build type table from AST :data items (fallback when no copybook)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (item (ensure-list data-items))
      (when (and (listp item) (numberp (first item)))
        (let ((name  (second item))
              (usage (find-if (lambda (x) (and (listp x) (eq (first x) :usage))) item)))
          (when (and usage (eq (second usage) :object-ref) (stringp name))
            (setf (gethash name table) (getf usage :class))))))
    table))
