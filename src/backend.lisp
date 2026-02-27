;; src/backend.lisp — generic code-generation interface
(in-package :eightbol)

;;; ---------------------------------------------------------------
;;; Public generic function
;;; ---------------------------------------------------------------

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

;;; ---------------------------------------------------------------
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
;;; ---------------------------------------------------------------

(defun class-id-to-copybook-filename (class-id)
  "Convert CLASS-ID (PascalCase) to copybook filename base in Title-And-Hyphens form.
   Character → Character, NonPlayerCharacter → Non-Player-Character"
  (title-case (param-case class-id)))

(defvar *eightbol-root-directory* nil
  "Root directory of the project; bound during compile-eightbol-class.
Used by backends to locate generated copybook files.")

;;; Dynamic bindings for copybook tables (bound during compilation)
(defvar *output-stream* nil "Assembly output stream; bound during compile-to-assembly.")
(defvar *class-id* nil)
(defvar *method-id* nil "Current method being compiled; used for paragraph labels.")
(defvar *slot-table* nil)
(defvar *type-table* nil)
(defvar *const-table* nil)
(defvar *service-bank-table* nil)
(defvar *usage-table* nil)
(defvar *sign-table* nil)
(defvar *pic-size-table* nil)
(defvar *pic-width-table* nil)
(defvar *pic-frac-bits-table* nil
  "Variable-name → fractional bits (0..n). From USAGE BINARY WITH nVm BITS (m = fractional).")

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
                  ("^\\s*\\b(Bank[A-Za-z0-9]+)\\s*=\\s*\\$([0-9a-fA-F]+)\\b" line)
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
                       (handler-case
                           (with-open-file (stream path :direction :input)
                             (loop for line = (read-line stream nil nil) while line do
                                   (cl-ppcre:register-groups-bind (hex)
                                       ("^\\s*BANK\\s*=\\s*\\$([0-9a-fA-F]+)\\b" line)
                                     (when hex
                                       (setf bank-num (parse-integer hex :radix 16))
                                       (setf bank-sym (cdr (assoc bank-num bank-num->symbol-alist)))))
                                   (cl-ppcre:register-groups-bind (service)
                                       ("^\\s*\\.Dispatch\\s+(Service[A-Za-z0-9]+)\\s*," line)
                                     (when (and service bank-sym)
                                       (setf (gethash service *service-bank-lut*) bank-sym))))))
                         (file-error () nil)))))))
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

(defun load-copybook-tables (class-id &key root-dir)
  "Load copybooks for CLASS-ID and return 8 tables: slot, type, const, service-bank,
usage, sign, pic-size, pic-width. Uses *copybook-paths* and *eightbol-root-directory*.
Merges *service-bank-lut* (built from bank files) into service-bank-table so CALL SERVICE
resolves without a Service-Banks copybook."
  (let* ((root (or root-dir *eightbol-root-directory* (truename ".")))
         (paths (or *copybook-paths* (list (merge-pathnames
                                           (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                                           root))))
         (slot-table (make-hash-table :test 'equal))
         (type-table (make-hash-table :test 'equal))
         (const-table (make-hash-table :test 'equal))
         (service-bank-table (make-hash-table :test 'equal))
         (usage-table (make-hash-table :test 'equal))
         (sign-table (make-hash-table :test 'equal))
         (pic-size-table (make-hash-table :test 'equal))
         (pic-width-table (make-hash-table :test 'equal)))
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
                 (let ((current-origin nil))
                   (with-open-file (stream path :direction :input)
                     (loop for line = (read-line stream nil nil) while line do
                           (let ((parsed (parse-cpy-line line)))
                             (when parsed
                               (ecase (first parsed)
                                 (:section-comment
                                  (setf current-origin
                                        (or (getf (rest parsed) :origin-class)
                                            (getf (rest parsed) :own-class))))
                                 (:data-item
                                  (let ((name (getf (rest parsed) :name))
                                        (type-cls (getf (rest parsed) :type-class))
                                        (constp (getf (rest parsed) :constant-p))
                                        (value (getf (rest parsed) :value))
                                        (usage-bcd (getf (rest parsed) :usage-bcd))
                                        (usage-signed (getf (rest parsed) :usage-signed))
                                        (pic-size (getf (rest parsed) :pic-size))
                                        (pic-width (getf (rest parsed) :pic-width)))
                                    (when current-origin
                                      (setf (gethash name slot-table) current-origin))
                                    (when type-cls
                                      (setf (gethash name type-table) type-cls))
                                    (when (and constp value)
                                      (setf (gethash name const-table) value))
                                    (when usage-bcd
                                      (setf (gethash name usage-table) :bcd))
                                    (when usage-signed
                                      (setf (gethash name sign-table) t))
                                    (when pic-size
                                      (setf (gethash name pic-size-table) pic-size))
                                    (when pic-width
                                      (setf (gethash name pic-width-table) pic-width))))
                                 (:service-bank
                                  (let ((service (getf (rest parsed) :service))
                                        (bank (getf (rest parsed) :bank)))
                                    (when (and service bank)
                                      (setf (gethash service service-bank-table) bank)))))))))))
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
                  (return-from load-globals))))))))
      (values slot-table type-table const-table service-bank-table
              usage-table sign-table pic-size-table pic-width-table))))))

(defun pic-digits-to-width (pic-rest)
  "From PIC clause rest, return byte width (1–8).
Examples: 1 byte = S9/99 USAGE DECIMAL or BINARY, S99 USAGE BINARY;
2 bytes = 9999/9(4)/S9(4); up to 8 bytes = 9(8)/S9(8) USAGE BINARY or DECIMAL.
Formula: max(1, ceiling(digits/2)). For BCD, S9 = 1 byte (sign nybble + 1 digit)."
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
    ;; Data item line:  NN Name PIC ... VALUE ...
    (cl-ppcre:register-groups-bind (level-str name rest)
        ("^(\\d+)\\s+(\\S+)\\s+(.*)\\.$" s)
      (let* ((level    (parse-integer level-str))
             (type-cls (cl-ppcre:register-groups-bind (c)
                           ("OBJECT REFERENCE (\\S+)" rest)
                         c))
             (is-const (or (= level 77) (= level 78)))
             (value    (when is-const
                         (cl-ppcre:register-groups-bind (v)
                             ("VALUE (\\d+)" rest)
                           (ignore-errors (parse-integer v)))))
             (usage-bcd (cl-ppcre:scan "(?i)USAGE\\s+(?:BCD|DECIMAL|PACKED\\s*[- ]?DECIMAL)" rest))
             (usage-signed (or (cl-ppcre:scan "(?i)(?:PIC|PICTURE)\\s+S\\d" rest)
                               (cl-ppcre:scan "(?i)SIGN\\s+(?:IS\\s+)?(?:LEADING|TRAILING)" rest)
                               (cl-ppcre:scan "(?i)\\bSIGNED\\b" rest)))
             (pic-size   (cl-ppcre:register-groups-bind (n)
                            ("(?i)(?:PIC|PICTURE)\\s+X\\s*\\(\\s*(\\d+)\\s*\\)" rest)
                          (when n (parse-integer n))))
             (pic-width  (pic-digits-to-width rest))
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
                :pic-frac-bits pic-frac-bits))))
    nil))

(defun load-copybook (copybook-name)
  "Loads a copybook COPYBOOK-NAME when commanded"
  (let ((path (block find-path
                (dolist (dir *copybook-paths*)
                  (let ((copybook (make-pathname
                                   :name (title-case copybook-name)
                                   :type "cpy"
                                   :directory dir)))
                    (when (probe-file copybook)
                      (return-from find-path copybook))))
                (error "Could not find copybook ~a.cpy on path ~s" (title-case copybook-name) *copybook-paths*))))
    ;; Parse each file
     (let ((current-origin nil))
        (handler-case
            (with-open-file (stream path :direction :input)
              (loop for line = (read-line stream nil nil) while line do
                (let ((parsed (parse-eig)))
                      (when parsed
                        (ecase (first parsed)
                          (:section-comment
                           (setf current-origin
                                 (or (getf (rest parsed) :origin-class)
                                     (getf (rest parsed) :own-class))))
                          (:data-item
                           (let ((name        (getf (rest parsed) :name))
                                 (type-cls    (getf (rest parsed) :type-class))
                                 (constp      (getf (rest parsed) :constant-p))
                                 (value       (getf (rest parsed) :value))
                                 (usage-bcd   (getf (rest parsed) :usage-bcd))
                                 (usage-signed (getf (rest parsed) :usage-signed))
                                 (pic-size    (getf (rest parsed) :pic-size))
                                 (pic-width   (getf (rest parsed) :pic-width)))
                             ;; Slot origin (for class copybooks with section comments)
                             (when current-origin
                               (setf (gethash name slot-table) current-origin))
                             ;; Object reference type
                             (when type-cls
                               (setf (gethash name type-table) type-cls))
                             ;; Constants (77/78)
                             (when (and constp value)
                               (setf (gethash name const-table) value))
                             ;; USAGE BCD / PACKED-DECIMAL (for RP2A03 software BCD)
                             (when usage-bcd
                               (setf (gethash name usage-table) :bcd))
                             ;; PIC S99, SIGN LEADING/TRAILING, SIGNED (for sign extension in MOVE)
                             (when usage-signed
                               (setf (gethash name sign-table) t))
                             ;; PIC X(n) size for INSPECT REPLACING
                             (when pic-size
                               (setf (gethash name pic-size-table) pic-size))
                             ;; PIC 99/9999 byte/word width for arithmetic
                             (when pic-width
                               (setf (gethash name pic-width-table) pic-width))))
                          (:service-bank
                           (let ((service (getf (rest parsed) :service))
                                 (bank    (getf (rest parsed) :bank)))
                             (when (and service bank)
                               (setf (gethash service service-bank-table) bank)))))))))
          (file-error (e)
            (error "EIGHTBOL: could not read copybook ~a: ~a" path e))))))

(defun usage-bcd-p (name)
  "True if NAME is a BCD variable (USAGE BCD or PACKED-DECIMAL) in *USAGE-TABLE*."
  (and *usage-table*
       (eq :bcd (gethash (if (stringp name) name (format nil "~a" name)) *usage-table*))))

(defun operand-signed-p (expr)
  "True if EXPR is a variable/slot with signed PIC (PIC S99, SIGN LEADING/TRAILING, SIGNED).
Uses *SIGN-TABLE*."
  (and *sign-table*
       (let ((name (expr-to-width-name expr)))
         (and name (gethash (if (stringp name) name (format nil "~a" name)) *sign-table*)))))

;;; ---------------------------------------------------------------
;;; Accessor helpers (used by backends)
;;; ---------------------------------------------------------------

(defun slot-origin-class (slot-name current-class)
  "Return the origin class for SLOT-NAME, or CURRENT-CLASS as fallback. Uses *SLOT-TABLE*."
  (if *slot-table*
      (or (gethash slot-name *slot-table*) current-class)
      current-class))

(defun slot-symbol (slot-name current-class)
  "Return assembly symbol {OriginClass}{SlotName}, e.g. CharacterHP. Uses *SLOT-TABLE*."
  (to-pascal-case
   (format nil "~a-~a"
           (slot-origin-class slot-name current-class)
           slot-name)))

(defun var-class (var-name)
  "Return the OBJECT REFERENCE class for VAR-NAME, or NIL. Uses *TYPE-TABLE*."
  (and *type-table*
       (gethash (if (stringp var-name) var-name (format nil "~a" var-name))
                *type-table*)))

(defun constant-value (name)
  "Return the integer value of constant NAME, or NIL. Uses *CONST-TABLE*."
  (and *const-table*
       (gethash (if (stringp name) name (format nil "~a" name))
                *const-table*)))

(defun constant-p (name)
  "True if NAME is a known compile-time constant. Uses *CONST-TABLE*."
  (and *const-table*
       (nth-value 1 (gethash (if (stringp name) name (format nil "~a" name))
                             *const-table*))))

(defun pic-size (name)
  "Return PIC X(n) size for NAME, or NIL if unknown. Uses *PIC-SIZE-TABLE*."
  (and *pic-size-table*
       (gethash (if (stringp name) name (format nil "~a" name)) *pic-size-table*)))

(defun pic-width (name)
  "Return PIC byte width for NAME (1–8 bytes). NIL if unknown. Uses *PIC-WIDTH-TABLE*."
  (and *pic-width-table*
       (gethash (if (stringp name) name (format nil "~a" name)) *pic-width-table*)))

(defun expr-to-width-name (expr)
  "Return variable name for pic-width lookup. NIL for literals/constants."
  (cond
    ((stringp expr) (format nil "~a" expr))
    ((and (listp expr) (eq (first expr) :of))
     (format nil "~a" (second expr)))
    ((and (listp expr) (eq (first expr) :subscript))
     (format nil "~a" (second expr)))
    (t nil)))

(defun operand-width (expr)
  "Return byte width (1–8) for EXPR. Default 1 for literals/unknown. Uses *PIC-WIDTH-TABLE*."
  (let ((name (expr-to-width-name expr)))
    (if name
        (or (pic-width name) 1)
        1)))

;;; ---------------------------------------------------------------
;;; Statement compilation — generic dispatch on CPU and AST node type
;;;
;;; (compile-statement cpu ast-node-symbol ast-node-data)
;;; Specializes on cpu and ast-node-symbol only. Uses *output-stream*.
;;; Each CPU defines brief methods for each statement type it supports.
;;; ---------------------------------------------------------------

(defgeneric compile-statement (cpu ast-node-symbol ast-node-data)
  (:documentation "Compile one statement. Dispatches on CPU and statement type. Uses *output-stream*."))

(defmethod compile-statement (cpu ast-node-symbol ast-node-data)
  (declare (ignore ast-node-data))
  (error "compile-statement: no method for CPU ~s statement ~s" cpu ast-node-symbol))

;;; ---------------------------------------------------------------
;;; Legacy helpers kept for backwards compatibility
;;; ---------------------------------------------------------------

(defun build-slot-table (data-items)
  "Build slot-origin table from AST :data items (fallback when no copybook)."
  (let ((table (make-hash-table :test 'equal))
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
        (setf (gethash (second item) table) current-origin)))
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
