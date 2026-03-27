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
  (title-case (param-case class-id)))

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
(defvar *method-id* nil "Current method being compiled; used for paragraph labels.")
(defvar *slot-table* nil
  "Slot name → origin class string or global section label (e.g. Phantasia-Globals).
Bound to a hash table with @code{:test equalp} so copybook uppercase names match
parser mixed-case identifiers (see @code{load-copybook-tables}).")
(defvar *type-table* nil)
(defvar *const-table* nil
  "77/78 constant name → integer. Bound to a hash table with @code{:test equalp}
(case-insensitive string keys; see @code{const-table-name-key}).")
(defvar *service-bank-table* nil)
(defvar *usage-table* nil)
(defvar *sign-table* nil)
(defvar *pic-size-table* nil)
(defvar *pic-width-table* nil)
(defvar *pic-frac-bits-table* nil
  "Variable-name → fractional bits (0..n). From USAGE BINARY WITH nVm BITS (m = fractional).")
(defvar *pic-nybble-semantics-table* nil
  "Variable-name → T when copybook PIC is single-digit @code{9}/@code{S9} (logical @code{#x0}--@code{#xF});
@code{NIL} in table means @code{PIC 99} or wider numeric picture. Storage is still byte-aligned.")

(defparameter +object-reference-storage-width+ 2
  "Byte width for OBJECT REFERENCE data items (OOPS pointer). Phantasia uses 16-bit references.
Used when @code{*type-table*} names a class but @code{*pic-width-table*} has no entry (copybook
lines often omit numeric PIC for @code{USAGE OBJECT REFERENCE @dots{}}).")

;;; In-memory service→bank LUT for CALL SERVICE resolution (no copybook file).
;;; Populated by build-service-bank-lut-from-banks when scanning Source/Code/{machine}/Banks.
(defvar *service-bank-lut* (make-hash-table :test 'equal)
  "In-memory LUT: service name (string) → bank symbol (string). Used for CALL SERVICE target.")

(defun build-service-bank-lut-from-banks (root-dir machine-dir)
  "Scan Source/Code/{MACHINE-DIR}/Banks/**/*.s and Common/Enums.s.
Populate *service-bank-lut* with service→bank mappings. MACHINE-DIR is e.g. \"7800\"."
  (clrhash *service-bank-lut*)
  (let* ((common (merge-pathnames
                  (make-pathname :directory `(:relative "Source" "Code" ,machine-dir "Common"))
                  root-dir))
         (enums-path (merge-pathnames "Enums.s" common))
         (banks-dir (merge-pathnames
                     (make-pathname :directory `(:relative "Source" "Code" ,machine-dir "Banks"))
                     root-dir))
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
                       ;; Swallow read/open errors per bank file (permissions, encoding, etc.).
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
                                     (setf (gethash service *service-bank-lut*) bank-sym))))))))))))
    ;; Temp trees (unit tests) often lack Source/Code/.../Banks — allow empty LUT there.
    ;; When Banks exists under ROOT-DIR but the LUT is still empty, Enums.s or .Dispatch scan failed.
    (when (and (zerop (hash-table-count *service-bank-lut*))
               (probe-file (merge-pathnames
                            (make-pathname :directory `(:relative "Source" "Code" ,machine-dir "Banks"))
                            root-dir)))
      (error "EIGHTBOL: empty service:bank LUT for machine ~a (~a); check Source/Code/~a/Banks dispatch labels and Common/Enums bank symbols"
             machine-dir root-dir machine-dir))
    *service-bank-lut*)

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

(defun %merge-cpy-stream-into-tables (stream slot-table type-table const-table
                                      service-bank-table usage-table sign-table
                                      pic-size-table pic-width-table pic-frac-bits-table
                                      pic-nybble-table)
  "Read copybook text from STREAM and merge entries into the ten hash tables.
SLOT-TABLE, TYPE-TABLE, CONST-TABLE, SERVICE-BANK-TABLE, USAGE-TABLE, SIGN-TABLE,
PIC-SIZE-TABLE, PIC-WIDTH-TABLE, PIC-FRAC-BITS-TABLE, and PIC-NYBBLE-TABLE are mutated.
Uses @code{parse-cpy-line} on each line."
  (declare (stream stream))
  (let ((current-origin nil))
    (loop for line = (read-line stream nil nil) while line do
          (let ((parsed (parse-cpy-line line)))
            (when parsed
              ;; Use COND (not ECASE) so :SERVICE-BANK cannot be mis-read as a stray form after a
              ;; paren mismatch (which would evaluate as (FUNCALL :SERVICE-BANK …) and signal
              ;; UNDEFINED-FUNCTION).
              (cond
                ((eql (first parsed) :section-comment)
                 (setf current-origin
                       (or (getf (rest parsed) :origin-class)
                           (getf (rest parsed) :own-class))))
                ((eql (first parsed) :data-item)
                 (let ((level (getf (rest parsed) :level))
                       (name (getf (rest parsed) :name))
                       (pic-rest (getf (rest parsed) :pic))
                       (type-cls (getf (rest parsed) :type-class))
                       (constp (getf (rest parsed) :constant-p))
                       (value (getf (rest parsed) :value))
                       (usage-bcd (getf (rest parsed) :usage-bcd))
                       (usage-signed (getf (rest parsed) :usage-signed))
                       (pic-size (getf (rest parsed) :pic-size))
                       (pic-width (getf (rest parsed) :pic-width)))
                   ;; Phantasia-Globals.cpy uses COBOL 01 … EXTERNAL section headers (no * Inherited * line).
                   ;; Without this, current-origin stays NIL and globals never enter SLOT-TABLE — bare names
                   ;; wrongly default to instance slots (e.g. Hurt-H-P → CharacterHurtHp + lda (Self),y).
                   (when (and (= level 1)
                              (stringp pic-rest)
                              (search "EXTERNAL" pic-rest :test #'char-equal))
                     (setf current-origin "Phantasia-Globals"))
                   ;; Gap subsections (05 CART-RAM-GAP-n) follow 01 CART-RAM EXTERNAL; keep global origin.
                   (when (and (= level 5)
                              (stringp name)
                              (cl-ppcre:scan "^CART-RAM-GAP" name))
                     (setf current-origin "Phantasia-Globals"))
                   (let ((skip-slot-hash
                          (or (and (= level 1)
                                   (stringp pic-rest)
                                   (search "EXTERNAL" pic-rest :test #'char-equal))
                              (and (= level 5)
                                   (stringp name)
                                   (cl-ppcre:scan "^CART-RAM-GAP" name))
                              ;; 77/78 are compile-time constants — not object layout slots.
                              constp)))
                     (when (and current-origin (not skip-slot-hash))
                       (setf (gethash (cobol-slot-table-name-key name) slot-table)
                             current-origin)))
                   (when type-cls
                     (setf (gethash (cobol-slot-table-name-key name) type-table) type-cls))
                   (when (and constp value)
                     (setf (gethash (const-table-name-key name) const-table) value))
                   (when usage-bcd
                     (setf (gethash (cobol-slot-table-name-key name) usage-table) :bcd))
                   (when usage-signed
                     (setf (gethash (cobol-slot-table-name-key name) sign-table) t))
                   (when pic-size
                     (setf (gethash (cobol-slot-table-name-key name) pic-size-table) pic-size))
                   (when pic-width
                     (setf (gethash (cobol-slot-table-name-key name) pic-width-table) pic-width))
                   (let ((frac (getf (rest parsed) :pic-frac-bits)))
                     (when frac
                       (setf (gethash (cobol-slot-table-name-key name) pic-frac-bits-table) frac)))
                   (when (and (stringp pic-rest) (pic-nybble-semantics-p pic-rest))
                     (setf (gethash (cobol-slot-table-name-key name) pic-nybble-table) t))))
                ((eql (first parsed) :service-bank)
                 (let ((service (getf (rest parsed) :service))
                       (bank (getf (rest parsed) :bank)))
                   (when (and service bank)
                     (setf (gethash service service-bank-table) bank))))
                (t
                 (error "EIGHTBOL: parse-cpy-line returned unknown type ~s" (first parsed)))))))))

(defun load-copybook-tables (class-id &key root-dir)
  "Load copybooks for CLASS-ID and return 10 tables: slot, type, const, service-bank,
usage, sign, pic-size, pic-width, pic-frac-bits, pic-nybble-semantics. Uses *copybook-paths* and
@code{*eightbol-root-directory*}. Merges @code{*service-bank-lut*} (built from bank files)
into service-bank-table so CALL SERVICE resolves without a Service-Banks copybook.
After @file{*-Globals.cpy}, merges @file{Source/Generated/{machine}/AssetIDs.cpy} when present so
77/78 asset IDs (e.g. @code{Song--Hurt--ID}) populate @code{*CONST-TABLE*}."
  (let* ((root (or root-dir *eightbol-root-directory* (truename ".")))
         (paths (or *copybook-paths* (list (merge-pathnames
                                           (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                                           root))))
         (slot-table (make-hash-table :test 'equalp))
         (type-table (make-hash-table :test 'equalp))
         (const-table (make-hash-table :test 'equalp))
         (service-bank-table (make-hash-table :test 'equal))
         (usage-table (make-hash-table :test 'equalp))
         (sign-table (make-hash-table :test 'equalp))
         (pic-size-table (make-hash-table :test 'equalp))
         (pic-width-table (make-hash-table :test 'equalp))
         (pic-frac-bits-table (make-hash-table :test 'equalp))
         (pic-nybble-table (make-hash-table :test 'equalp)))
    ;; Populate service-bank-table from in-memory LUT (scanned from bank files)
    (let ((machine (infer-machine-from-copybook-paths)))
      (when machine
        (build-service-bank-lut-from-banks root machine))
      (maphash (lambda (k v) (setf (gethash k service-bank-table) v)) *service-bank-lut*))
    ;; Load copybooks: Class-Slots and *-Globals
    (flet ((load-one-copybook (base-name)
             (let ((path (block find-path
                           (dolist (dir paths)
                             (let ((p (merge-pathnames
                                       (make-pathname :name base-name :type "cpy")
                                       (if (pathnamep dir) dir (pathname dir)))))
                               (when (probe-file p)
                                 (return-from find-path p))))
                           (return-from load-one-copybook nil))))
               (when path
                 (with-open-file (stream path :direction :input)
                   (%merge-cpy-stream-into-tables stream slot-table type-table const-table
                                                  service-bank-table usage-table sign-table
                                                  pic-size-table pic-width-table
                                                  pic-frac-bits-table pic-nybble-table))))))
      (load-one-copybook (format nil "~a-Slots" (class-id-to-copybook-filename class-id)))
      ;; Load first *-Globals.cpy found in paths
      (block load-globals
        (dolist (dir paths)
          (let ((dir-p (if (pathnamep dir) dir (pathname dir))))
            (when (probe-file dir-p)
              (dolist (f (directory (merge-pathnames
                                    (make-pathname :name :wild :type "cpy")
                                    dir-p)))
                (when (cl-ppcre:scan "-Globals$" (pathname-name f))
                  (load-one-copybook (pathname-name f))
                  (return-from load-globals)))))))
      ;; Merge Source/Generated/{machine}/AssetIDs.cpy (77/78 song & asset IDs). Not under *-Globals;
      ;; must be merged here so *CONST-TABLE* has Song--Hurt--ID etc. Otherwise bare MOVE Song--Hurt--ID
      ;; wrongly uses implicit instance slot (ldy #ClassSongHurtID / lda (Self),y) instead of lda # Song_Hurt_ID.
      (let ((machine (infer-machine-from-copybook-paths)))
        (when machine
          (let ((asset-ids (merge-pathnames
                            (make-pathname :name "AssetIDs" :type "cpy")
                            (merge-pathnames
                             (make-pathname :directory `(:relative "Source" "Generated" ,machine))
                             root))))
            (when (probe-file asset-ids)
              (with-open-file (stream asset-ids :direction :input)
                (%merge-cpy-stream-into-tables stream slot-table type-table const-table
                                                service-bank-table usage-table sign-table
                                                pic-size-table pic-width-table
                                                pic-frac-bits-table pic-nybble-table))))))
      (values slot-table type-table const-table service-bank-table
              usage-table sign-table pic-size-table pic-width-table pic-frac-bits-table
              pic-nybble-table))))

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

(defun pic-digits-to-width (pic-rest)
  "From PIC clause rest, return byte width (1–8) for packed storage in RAM.
Examples: 1 byte = S9/99 USAGE DECIMAL or BINARY, S99 USAGE BINARY;
2 bytes = 9999/9(4)/S9(4); up to 8 bytes = 9(8)/S9(8) USAGE BINARY or DECIMAL.
Formula: max(1, ceiling(digits/2)). For BCD, S9 = 1 byte (sign nybble + 1 digit).
Single-digit @code{PIC 9} and two-digit @code{PIC 99} both allocate one byte in
generated copybooks; runtime nybble packing (where used) is documented in Phantasia
class design notes."
  (or (cl-ppcre:register-groups-bind (digits n)
          ("(?i)(?:PIC|PICTURE)\\s+(?:(\\d+)|9\\s*\\(\\s*(\\d+)\\s*\\))" pic-rest)
        (let ((d (or (when digits (length digits)) (when n (parse-integer n)))))
          (when d (max 1 (ceiling d 2)))))
      ;; S9, S99, S9(n) — signed; for BCD, S9 = 1 byte (1 digit + sign nybble)
      (cl-ppcre:register-groups-bind (sdigits sn)
          ("(?i)(?:PIC|PICTURE)\\s+S(?:(\\d+)|9\\s*\\(\\s*(\\d+)\\s*\\))" pic-rest)
        (let ((d (or (when sdigits (length sdigits)) (when sn (parse-integer sn)))))
          (when d (max 1 (ceiling d 2)))))))

(defun parse-cpy-line (line)
  "Parse one line of a EIGHTBOL copybook (.cpy) file.
Returns one of:
  (:section-comment :origin-class CLASS-NAME-STRING)   — * Inherited from X:
  (:section-comment :own-class CLASS-NAME-STRING)       — * Own slots (X):
  (:data-item :level N :name NAME :pic PIC :type-class C) — 05/10/77/78 item
  NIL                                                    — uninteresting line"
  (let ((s (string-trim " " line)))
    (when (zerop (length s)) (return-from parse-cpy-line nil))
    ;; Comment line: NNNNNN* or * at start.  Cols 1-6 = seq, col 7 = *.
    (let ((text (cond ((and (>= (length s) 7) (char= #\* (char s 6)))
                       (string-trim " " (subseq s 7)))
                      ((char= #\* (char s 0))
                       (string-trim " " (subseq s 1)))
                      (t nil))))
      (when text
        (cl-ppcre:register-groups-bind (class)
            ("Inherited from (\\S+):" text)
          (return-from parse-cpy-line
            (list :section-comment :origin-class class)))
        (cl-ppcre:register-groups-bind (class)
            ("Own slots \\((\\S+)\\)" text)
          (return-from parse-cpy-line
            (list :section-comment :own-class class)))
        (cl-ppcre:register-groups-bind (service bank)
            ("^Service bank (\\S+)\\s+(\\S+)$" text)
          (when (and service bank)
            (return-from parse-cpy-line
              (list :service-bank :service service :bank bank))))))
    ;; Data item line:  NN Name PIC … [VALUE …]. Trailing period optional (Phantasia AssetIDs.cpy
    ;; lines omit it: @samp{77 Song--Hurt--ID … VALUE IS CONSTANT x'49'}).
    (let ((data (if (and (plusp (length s)) (char= #\. (char s (1- (length s)))))
                    (subseq s 0 (1- (length s)))
                    s)))
      (cl-ppcre:register-groups-bind (level-str name rest)
          ("^(\\d+)\\s+(\\S+)\\s+(.*)$" data)
        (let* ((level    (parse-integer level-str))
               (type-cls (cl-ppcre:register-groups-bind (c)
                             ("OBJECT REFERENCE (\\S+)" rest)
                           c))
               (is-const (or (= level 77) (= level 78)))
               (value    (when is-const
                           (or (cl-ppcre:register-groups-bind (v)
                                   ("(?i)VALUE\\s+(\\d+)" rest)
                                 (ignore-errors (parse-integer v)))
                               (cl-ppcre:register-groups-bind (hx)
                                   ("(?i)VALUE\\s+IS\\s+CONSTANT\\s+x'([0-9A-Fa-f]+)'" rest)
                                 (ignore-errors (parse-integer hx :radix 16))))))
               (usage-bcd (cl-ppcre:scan "(?i)USAGE\\s+(?:BCD|DECIMAL|PACKED\\s*[- ]?DECIMAL)" rest))
               (usage-signed (or (cl-ppcre:scan "(?i)(?:PIC|PICTURE)\\s+S\\d" rest)
                                 (cl-ppcre:scan "(?i)SIGN\\s+(?:IS\\s+)?(?:LEADING|TRAILING)" rest)
                                 (cl-ppcre:scan "(?i)\\bSIGNED\\b" rest)))
               (pic-size   (cl-ppcre:register-groups-bind (n)
                               ("(?i)(?:PIC|PICTURE)\\s+X\\s*\\(\\s*(\\d+)\\s*\\)" rest)
                             (when n (parse-integer n))))
               ;; Numeric PIC → digit-derived width; PIC X(n) → n bytes (pic-size).
               (pic-width  (or (pic-digits-to-width rest) pic-size))
               (pic-frac-bits (cl-ppcre:register-groups-bind (int-bits frac-bits)
                                  ("(?i)USAGE\\s+BINARY\\s+WITH\\s+(\\d+)V(\\d+)\\s+BITS" rest)
                                (when (and int-bits frac-bits)
                                  (parse-integer frac-bits)))))
          (return-from parse-cpy-line
            (list :data-item
                  :level     level
                  :name      name
                  :pic       rest
                  :pic-size  pic-size
                  :pic-width pic-width
                  :type-class type-cls
                  :constant-p is-const
                  :value     value
                  :usage-bcd  (and usage-bcd t)
                  :usage-signed (and usage-signed t)
                  :pic-frac-bits pic-frac-bits)))))
    nil))

(defun usage-bcd-p (name)
  "True if NAME is a BCD variable (USAGE BCD or PACKED-DECIMAL) in *USAGE-TABLE*."
  (and *usage-table*
       (let ((n (if (stringp name) name (format nil "~a" name))))
         (eq :bcd (or (gethash (cobol-slot-table-name-key n) *usage-table*)
                      (gethash n *usage-table*))))))

(defun operand-signed-p (expr)
  "True if EXPR is a variable/slot with signed PIC (PIC S99, SIGN LEADING/TRAILING, SIGNED).
Uses *SIGN-TABLE*."
  (and *sign-table*
       (let ((name (expr-to-width-name expr)))
         (and name
              (let ((n (if (stringp name) name (format nil "~a" name))))
                (or (gethash (cobol-slot-table-name-key n) *sign-table*)
                    (gethash n *sign-table*)))))))

;;; Accessor helpers (used by backends)

(defun cobol-hyphen-segment-to-pascal-fragment (segment)
  "One hyphen-delimited piece of a COBOL name → fragment for PascalCase concatenation.
2–3 letter all-uppercase segments (e.g. HP, ID) stay uppercase so assembly matches
@code{HurtHP} / @code{NextSong}; longer words use @code{string-capitalize}."
  (let ((s (string-trim " " segment)))
    (cond
      ((zerop (length s)) "")
      ((and (<= 2 (length s) 3)
            (every #'alpha-char-p s)
            (every #'upper-case-p s))
       s)
      (t (string-capitalize s)))))

(defun cobol-hyphenated-to-pascal-concat (name)
  "Split COBOL stabby-case NAME on single hyphens; map each segment; concatenate.

Segments use @code{cobol-hyphen-segment-to-pascal-fragment} (not raw @code{string-capitalize})
so @code{Hurt-HP} → @code{HurtHP} not @code{HurtHp}. Examples: Next-Song → NextSong,
Hurt-H-P → HurtHP.

When NAME has no hyphens but is a long alphabetic token (e.g. generated @code{DecalAnimationOnTick}),
@code{param-case} expands camelCase/PascalCase to hyphen-separated words so each word is
capitalized — avoiding @code{Decalanimationontick} from a single @code{string-capitalize}.

@table @asis
@item NAME
COBOL identifier (string or atom printed as string); surrounding space trimmed.
@end table

@subsection Outputs
Returns a single PascalCase-like concatenation with no separators (no class prefix)."
  (let ((s (string-trim " " (if (stringp name) name (format nil "~a" name)))))
    (when (zerop (length s))
      (return-from cobol-hyphenated-to-pascal-concat ""))
    (let* ((parts0 (split-sequence:split-sequence #\- s :remove-empty-subseqs t))
           (parts (if (and (= (length parts0) 1)
                           (> (length (first parts0)) 4)
                           (every #'alpha-char-p (first parts0))
                           (not (find #\- s)))
                      (let ((p (param-case (first parts0))))
                        (if (find #\- p)
                            (split-sequence:split-sequence #\- p :remove-empty-subseqs t)
                            parts0))
                      parts0)))
      (apply #'concatenate 'string
             (mapcar #'cobol-hyphen-segment-to-pascal-fragment parts)))))

(defun pascal-to-eightbol-name-core (pascal-name)
  "Insert hyphens before uppercase letters that follow lowercase (Skyline-Tool rule).
Used only to build @code{cobol-slot-table-name-key}."
  (with-output-to-string (out)
    (loop for i from 0 below (length pascal-name)
          for ch = (char pascal-name i)
          do (when (and (> i 0)
                        (upper-case-p ch)
                        (lower-case-p (char pascal-name (1- i))))
               (write-char #\- out))
             (write-char ch out))))

(defun cobol-slot-table-name-key (name)
  "Canonical key for @code{*slot-table*} / PIC-width / usage lookups.
Unifies @code{Hurt-H-P} and @code{Hurt-HP} (both map to assembly @code{HurtHP}) and
matches generated Globals copybook keys (uppercase stabby-case)."
  (let ((s (string-trim " " (if (stringp name) name (format nil "~a" name)))))
    (when (zerop (length s))
      (return-from cobol-slot-table-name-key ""))
    (string-upcase
     (pascal-to-eightbol-name-core
      (cobol-hyphenated-to-pascal-concat s)))))

(defun slot-table-origin-lookup (name)
  "Resolve copybook origin for NAME using canonical key, raw NAME, then class-qualified strip.

When the parser emits @code{Class-Global-Name} and the copybook row is @code{Global-Name}
under Phantasia-Globals, stripping the @code{*CLASS-ID*} prefix finds the same row."
  (when *slot-table*
    (let ((n (if (stringp name) name (format nil "~a" name))))
      (or (gethash (cobol-slot-table-name-key n) *slot-table*)
          (gethash n *slot-table*)
          (when *class-id*
            (let ((short (cobol-strip-class-copybook-prefix n)))
              (when short
                (or (gethash (cobol-slot-table-name-key short) *slot-table*)
                    (gethash short *slot-table*)))))))))

(defun %service-move-decal-paired-bank (n)
  "If N is @code{ServiceMoveDecalX} or @code{ServiceMoveDecalY}, return the other’s bank entry.

LastBank @code{MoveDecal.s} exposes both; bank LUT may only list one @code{.Dispatch} line.
Paired lookup keeps @code{CALL … IN LIBRARY} on both emitting @code{.FarCall} with the same bank.

@table @asis
@item N
Assembly service name string.
@end table

@subsection Outputs
Bank symbol string or @code{NIL}."
  (when (and *service-bank-table* n)
    (let ((other
            (cond
              ((string-equal n "ServiceMoveDecalX") "ServiceMoveDecalY")
              ((string-equal n "ServiceMoveDecalY") "ServiceMoveDecalX")
              (t nil))))
      (when other
        (or (gethash other *service-bank-table*)
            (gethash (cobol-slot-table-name-key other) *service-bank-table*))))))

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
  (when (and *service-bank-table* name)
    (let ((n (if (stringp name) name (format nil "~a" name))))
      (or (gethash n *service-bank-table*)
          (gethash (cobol-slot-table-name-key n) *service-bank-table*)
          (%service-move-decal-paired-bank n)))))

(defun pic-width-table-lookup (name pic-width-table)
  "Byte width for NAME from PIC-WIDTH-TABLE, with canonical key then raw.

When @code{*CLASS-ID*} is bound, also tries @code{cobol-strip-class-copybook-prefix}
(unqualified suffix of a class-qualified name) and @code{ClassId-Name} qualified
form so instance slots written as @code{05 HP} or @code{05 Character-HP} in the
copybook both resolve (fixes 16-bit @code{IF HP OF Self > Max-HP OF Self}).

Finally tries @code{(string-upcase (cobol-hyphenated-to-pascal-concat NAME))} so
@code{Max-HP} (stabby key @code{MAX-HP}) matches copybook rows keyed as @code{MAXHP}
from @code{05 MAX-HP} (same concatenated form as @code{MAX-HP} in source)."
  (when (and name pic-width-table)
    (let ((n (if (stringp name) name (format nil "~a" name))))
      (or (gethash (cobol-slot-table-name-key n) pic-width-table)
          (gethash n pic-width-table)
          (when *class-id*
            (let ((short (cobol-strip-class-copybook-prefix n)))
              (when short
                (or (gethash (cobol-slot-table-name-key short) pic-width-table)
                    (gethash short pic-width-table)))))
          (when *class-id*
            (let ((qualified (format nil "~a-~a"
                                     (class-id-to-copybook-filename *class-id*)
                                     n)))
              (or (gethash (cobol-slot-table-name-key qualified) pic-width-table)
                  (gethash qualified pic-width-table))))
          (gethash (string-upcase (cobol-hyphenated-to-pascal-concat n))
                   pic-width-table)))))

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
          (cobol-hyphenated-to-pascal-concat s)
          (format nil "~{~a~^_~}"
                  (mapcar #'cobol-hyphenated-to-pascal-concat groups))))))

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
  (cobol-hyphenated-to-pascal-concat name))

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

(defun slot-origin-class (slot-name current-class)
  "Return the origin class for SLOT-NAME, or CURRENT-CLASS as fallback. Uses *SLOT-TABLE*."
  (if *slot-table*
      (or (slot-table-origin-lookup slot-name) current-class)
      current-class))

(defun slot-name-to-assembly-segment (name)
  "Convert COBOL data item NAME to the slot suffix of an OOPS assembly symbol.
Short all-caps names (2–4 letters, e.g. HP) stay uppercase so labels match
CharacterHP; hyphenated names use @code{cobol-hyphenated-to-pascal-concat} (H-P → HP)."
  (let ((s (string-trim " " (if (stringp name) name (princ-to-string name)))))
    (cond
      ((zerop (length s)) "")
      ((cl-ppcre:scan "^[A-Z]{2,4}$" s) s)
      (t (cobol-hyphenated-to-pascal-concat s)))))

(defvar *parent-class-slot-prefix-words*
  '("Course" "Entity" "Character" "Actor" "BasicObject" "Item" "Boat" "BasicEnemy"
    "BasicScenery" "NonPlayerCharacter" "Player" "Ally")
  "First hyphen-separated word of slot names whose assembly segment already carries that
parent’s PascalCase prefix (e.g. @code{Course-Last-Frame} → @code{CourseLastFrame}).
Used when the copybook @code{*slot-table*} origin is the compiling subclass (e.g. MummyCourse)
so we do not emit @code{MummyCourseCourseLastFrame}.")

(defun slot-name-first-hyphen-word (slot-name)
  "Return the substring before the first single hyphen in SLOT-NAME, or @code{NIL}.

@table @asis
@item SLOT-NAME
COBOL data name (e.g. @code{Course-Last-Frame}).
@end table"
  (let ((s (string-trim " " (if (stringp slot-name) slot-name (format nil "~a" slot-name)))))
    (let ((pos (position #\- s)))
      (when pos
        (subseq s 0 pos)))))

(defun slot-symbol-parent-prefixed-segment-p (slot-name segment)
  "True when SLOT-NAME is @code{Parent-…} and SEGMENT already starts with Parent’s PascalCase.

Subclass copybooks may set slot origin to the compiling class while inherited slots keep
names like @code{Course-Waypoint-X}; layout labels stay @code{CourseWaypointX}.

@table @asis
@item SLOT-NAME
@item SEGMENT
From @code{slot-name-to-assembly-segment}.
@end table"
  (let ((lead (slot-name-first-hyphen-word slot-name)))
    (when (and lead (member lead *parent-class-slot-prefix-words* :test #'string-equal)
              (plusp (length segment)))
      (let ((lead-p (to-pascal-case lead)))
        (when (and (plusp (length lead-p))
                   (>= (length segment) (length lead-p))
                   (string-equal lead-p segment :end2 (length lead-p)))
          t)))))

(defun slot-symbol (slot-name current-class)
  "Return assembly copybook offset symbol for SLOT-NAME. Uses *SLOT-TABLE* for origin class.

When the hyphenated slot segment already begins with the origin class’s PascalCase name
(e.g. origin @code{Course}, name @code{Course-Last-Frame} → segment @code{CourseLastFrame}),
returns the segment alone (@code{CourseLastFrame}) so labels match OOPS copybooks, not
@code{CourseCourseLastFrame}. When the copybook origin is a subclass (e.g. MummyCourse) but
the slot name is parent-prefixed (@code{Course-Last-Frame}), returns @code{CourseLastFrame},
not @code{MummyCourseCourseLastFrame}. Otherwise @code{{OriginClass}{SlotSegment}} (e.g. @code{CharacterHP})."
  (let* ((origin (slot-origin-class slot-name current-class))
         (origin-pascal (to-pascal-case (format nil "~a" origin)))
         (segment (slot-name-to-assembly-segment slot-name)))
    (cond
      ((and (plusp (length origin-pascal))
            (plusp (length segment))
            (>= (length segment) (length origin-pascal))
            (string-equal origin-pascal segment :end2 (length origin-pascal)))
       segment)
      ((slot-symbol-parent-prefixed-segment-p slot-name segment)
       segment)
      (t
       (format nil "~a~a" origin-pascal segment)))))

(defun phantasia-global-bare-data-name-p (name)
  "True when NAME is a Phantasia CartRAM / ZeroPage item without a @code{*SLOT-TABLE*} row.

When @file{Phantasia-Globals.cpy} is not loaded, bare names like @code{Motion-Frame} would
otherwise fall through to @code{slot-symbol} and pick up the compiling class prefix. Keys
match @code{cobol-slot-table-name-key}.

@table @asis
@item NAME
COBOL identifier (string).
@end table"
  (let ((k (cobol-slot-table-name-key name)))
    (member k
            '("MOTION-FRAME" "FRAMES-PER-SECOND" "CURRENT-COURSE" "CURRENT-ACTOR")
            :test #'string=)))

(defun bare-data-assembly-symbol (name current-class-id)
  "Assembly label for a bare data reference (not @code{:of}). NAME must not be a constant (caller handles constants).

OOPS slots from class copybooks use @code{slot-symbol}; globals use @code{cobol-global-data-name-to-assembly-symbol}.

@table @asis
@item NAME
COBOL data name.
@item CURRENT-CLASS-ID
Compiling class (e.g. Character) for @code{slot-symbol} fallback.
@end table

@subsection Outputs
One assembly symbol string."
  (let ((n (if (stringp name) name (format nil "~a" name))))
    (let ((origin (slot-table-origin-lookup n)))
      (cond
        ((and origin (not (slot-origin-global-data-p origin)))
         (slot-symbol n current-class-id))
        ((and origin (slot-origin-global-data-p origin))
         (cobol-global-data-name-to-assembly-symbol n))
        ((phantasia-global-bare-data-name-p n)
         (cobol-global-data-name-to-assembly-symbol n))
        (t
         ;; Unknown bare names default to non-slot data labels (WORKING-STORAGE/global scratch),
         ;; not implicit instance slots.
         (cobol-global-data-name-to-assembly-symbol n))))))

(defun cobol-double-hyphen-grouped-name-p (name)
  "True when NAME uses COBOL 77/78 grouped spelling with @code{--} (e.g. @code{Song--Hurt--ID}).

These are manifest constants in Phantasia, never per-instance OOPS slots; they must not use
@code{ldy #Class…} / @code{lda (Self),y} when @code{*CONST-TABLE*} lacks a row (e.g. partial compile)."
  (and (stringp name)
       (search "--" name)))

(defun implicit-instance-slot-p (name class-id)
  "True when bare NAME is a per-instance OOPS slot for CLASS-ID (emit @code{ldy}/@code{lda (Self),y}).

Globals from copybooks whose origin matches @code{slot-origin-global-data-p} are not instance slots.

@table @asis
@item NAME
COBOL data name (string).
@item CLASS-ID
Compiling class id string (e.g. @code{\"Character\"}).
@end table

@subsection Outputs
Boolean."
  (let ((n (if (stringp name) name (format nil "~a" name))))
    (when (cobol-double-hyphen-grouped-name-p n)
      (return-from implicit-instance-slot-p nil))
    (when (phantasia-global-bare-data-name-p n)
      (return-from implicit-instance-slot-p nil))
    (when (and class-id (not (constant-p n)))
      (let ((origin (slot-table-origin-lookup n)))
        (cond
          ((and origin (slot-origin-global-data-p origin)) nil)
          ((and origin (not (slot-origin-global-data-p origin))) t)
          ;; No copybook origin: only treat as implicit instance slot when a PIC width row
          ;; exists (i.e., class copybook declared it). Otherwise prefer bare/global labels.
          (t (not (null (pic-width-table-lookup n *pic-width-table*)))))))))

(defun const-table-name-key (name)
  "Trimmed identifier for @code{*const-table*} @code{gethash}/@code{setf}.

@code{*const-table*} uses @code{equalp}, so string keys match case-insensitively;
only surrounding space is normalized."
  (string-trim " " (if (stringp name) name (format nil "~a" name))))

(defun phantasia-global-object-reference-class (var-name)
  "Return OBJECT REFERENCE class for well-known Phantasia globals when @code{*TYPE-TABLE*} has no row.

Covers @code{Current-Course} and @code{Current-Actor} so @code{INVOKE} emits @code{.CallMethod}
instead of @code{Call…Method} placeholders when @file{Phantasia-Globals.cpy} is absent.

@table @asis
@item VAR-NAME
COBOL data name (e.g. @code{Current-Actor}).
@end table

@subsection Outputs
Class id string (e.g. @code{\"Character\"}) or @code{NIL}."
  (let ((n (if (stringp var-name) var-name (format nil "~a" var-name))))
    (let ((k (cobol-slot-table-name-key n)))
      (cdr (assoc k
                  '(("CURRENT-COURSE" . "Course")
                    ("CURRENT-ACTOR" . "Character"))
                  :test #'string=)))))

(defun var-class (var-name)
  "Return the OBJECT REFERENCE class for VAR-NAME, or NIL. Uses *TYPE-TABLE* then Phantasia fallbacks."
  (let ((n (if (stringp var-name) var-name (format nil "~a" var-name))))
    (or (and *type-table*
             (or (gethash (cobol-slot-table-name-key n) *type-table*)
                 (gethash n *type-table*)))
        (phantasia-global-object-reference-class n))))

(defun %const-table-resolve (name)
  "Return @code{(values integer-value cobol-name-for-assembly)} or @code{(values nil nil)}.

Looks up @code{*CONST-TABLE*} by trimmed name, canonical slot key, and by stripping a leading
@code{Class-} prefix when @code{*CLASS-ID*} is bound (matches unqualified copybook 77/78 names)."
  (unless (and *const-table* name)
    (return-from %const-table-resolve (values nil nil)))
  (let ((trimmed (const-table-name-key name)))
    (multiple-value-bind (val present) (gethash trimmed *const-table*)
      (when present
        (return-from %const-table-resolve (values val trimmed))))
    (let ((canon (cobol-slot-table-name-key name)))
      (multiple-value-bind (val present) (gethash canon *const-table*)
        (when present
          (return-from %const-table-resolve (values val trimmed)))))
    (when *class-id*
      (let ((short (cobol-strip-class-copybook-prefix name)))
        (when short
          (multiple-value-bind (val present) (gethash (const-table-name-key short) *const-table*)
            (when present
              (return-from %const-table-resolve (values val short))))
          (multiple-value-bind (val present) (gethash (cobol-slot-table-name-key short) *const-table*)
            (when present
              (return-from %const-table-resolve (values val short)))))))
    (values nil nil)))

(defun constant-value (name)
  "Return the integer value of constant NAME, or NIL. Uses *CONST-TABLE*."
  (multiple-value-bind (val _) (%const-table-resolve name)
    val))

(defun constant-p (name)
  "True if NAME is a known compile-time constant. Uses *CONST-TABLE*."
  (multiple-value-bind (val _) (%const-table-resolve name)
    (not (null val))))

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

(defun pic-size (name)
  "Return PIC X(n) size for NAME, or NIL if unknown. Uses *PIC-SIZE-TABLE*."
  (and *pic-size-table*
       (let ((n (if (stringp name) name (format nil "~a" name))))
         (or (gethash (cobol-slot-table-name-key n) *pic-size-table*)
             (gethash n *pic-size-table*)))))

(defun pic-width (name)
  "Return PIC byte width for NAME (1–8 bytes). NIL if unknown. Uses *PIC-WIDTH-TABLE*."
  (and *pic-width-table*
       (pic-width-table-lookup (if (stringp name) name (format nil "~a" name))
                               *pic-width-table*)))

(defun expr-slot-of-slot-name (expr)
  "Return slot data name for @code{*pic-width-table*} when EXPR is slot OF object.

Parser @code{(data-name of data-name)} yields @code{(\"HP\" \"OF\" \"Self\")}; backend
prefers @code{(:of \"HP\" :self)}. Both must resolve the same PIC width (e.g. 16-bit HP).

@table @asis
@item EXPR
Expression AST leaf or nested form.
@end table

@subsection Outputs
Slot name string (e.g. @code{\"Max-HP\"}), or NIL if not a slot-OF form."
  (cond
    ((and (listp expr) (eq (first expr) :of))
     (format nil "~a" (second expr)))
    ((and (listp expr) (= (length expr) 3))
     (let ((slot (first expr))
           (of (second expr)))
       (when (or (equal of "OF")
                 (and (symbolp of) (string-equal (symbol-name of) "OF")))
         (format nil "~a" slot))))
    (t nil)))

(defun expr-to-width-name (expr)
  "Return variable name for pic-width lookup. NIL for literals/constants."
  (cond
    ((stringp expr) (format nil "~a" expr))
    ((and (listp expr) (eq (first expr) :subscript))
     (format nil "~a" (second expr)))
    (t (expr-slot-of-slot-name expr))))

(defun operand-width (expr &optional (pic-width-table *pic-width-table*))
  "Return byte width (1–8) for EXPR. Default 1 for literals/unknown.
When PIC-WIDTH-TABLE is supplied, width is resolved from that table; otherwise
from *PIC-WIDTH-TABLE* (used by backends that pass per-compile tables).
If the name is an OBJECT REFERENCE (@code{*type-table*}) but has no PIC width row,
uses @code{+object-reference-storage-width+}."
  (let ((name (expr-to-width-name expr)))
    (if name
        (let ((n (if (stringp name) name (format nil "~a" name))))
          (or (when (string-equal n "Self")
                +object-reference-storage-width+)
              (pic-width-table-lookup n pic-width-table)
              (when (var-class n) +object-reference-storage-width+)
              1))
        1)))

(defun expression-operand-width (expr &optional (pic-width-table *pic-width-table*))
  "Return max byte width (1–8) for EXPR and its leaves.

For composite arithmetic (@code{:add-expr}, @code{:subtract-expr}, @dots{}) returns
the maximum of recursive leaf widths. For other forms, @code{operand-width}.
Use when MOVE/ADD/SUB sources may be expressions so width matches the widest operand.

@table @asis
@item EXPR
Expression AST or bare identifier.
@item PIC-WIDTH-TABLE
Optional width table (defaults to @code{*pic-width-table*}).
@end table

@subsection Outputs
Integer byte count at least 1."
  (labels ((rec (e)
             (cond
               ((and (listp e)
                     (member (first e)
                             '(:add-expr :subtract-expr :multiply-expr :divide-expr
                               :shift-left :shift-right :bit-and :bit-or :bit-xor)
                             :test #'eq))
                (max (rec (second e)) (rec (third e))))
               ((and (listp e) (eq (first e) :bit-not))
                (rec (second e)))
               (t (operand-width e pic-width-table)))))
    (max 1 (rec expr))))

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

(defun operand-nybble-semantics-p (expr)
  "True when EXPR names a copybook row in @code{*pic-nybble-semantics-table*} (single @code{PIC 9} digit)."
  (and *pic-nybble-semantics-table*
       (let ((name (expr-to-width-name expr)))
         (and name
              (let ((n (if (stringp name) name (format nil "~a" name))))
                (or (gethash (cobol-slot-table-name-key n) *pic-nybble-semantics-table*)
                    (gethash n *pic-nybble-semantics-table*)))))))

(defun backend-unsupported-operand-width (cpu-name op-name width max-supported)
  "Signal an error when WIDTH exceeds MAX-SUPPORTED for CPU-NAME on OP-NAME (multi-byte codegen)."
  (when (> width max-supported)
    (error "EIGHTBOL/~a: ~a operand width ~d bytes not supported (max ~d)"
           cpu-name op-name width max-supported)))

(defun %sanitize-cobol-source-for-assembly-comment (text)
  "Return TEXT safe to embed after @code{;} in 64tass-style assembly (no nested @code{;}).

Collapses line breaks to spaces and maps @code{#\\;} to @code{#\\:} so the rest of the
line is not swallowed as a nested assembler comment."
  (when (and text (plusp (length text)))
    (with-output-to-string (out)
      (loop for ch across text
            do (write-char (case ch
                             (#\; #\:)
                             (#\Newline #\Space)
                             (#\Return #\Space)
                             (#\Linefeed #\Space)
                             (#\Page #\Space)
                             (t ch))
                           out)))))

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
          (src (safe-getf plist :source-text))
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
