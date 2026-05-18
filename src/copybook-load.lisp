;; src/copybook-load.lisp — COPYBOOK (.cpy) line parsing and table merge for EIGHTBOL
;;
;; Provides @code{parse-cpy-line} and @code{load-copybook-tables} used by all backends
;; and by the unit tests in @file{eightbol-tests.lisp}.
(in-package :eightbol)

(defun %cpy-trim (line)
  "Trim leading/trailing ASCII whitespace on LINE."
  (string-trim '(#\Space #\Tab) line))

(defun %cpy-strip-trailing-period (s)
  "Remove a single trailing period from copybook line S when present."
  (let ((trimmed (string-trim '(#\Space #\Tab) s)))
    (if (and (plusp (length trimmed)) (char= #\. (char trimmed (1- (length trimmed)))))
        (subseq trimmed 0 (1- (length trimmed)))
        trimmed)))

(defun %cpy-extract-pic-clause (tail)
  "Return substring from PIC/PICTURE through the token before USAGE, or NIL."
  (let ((up (string-upcase tail)))
    (let ((p (or (search "PIC" up) (search "PICTURE" up)))
          (u (search "USAGE" up)))
      (when (and p u (< p u))
        (string-trim " " (subseq tail p u))))))

(defun %parse-cpy-data-item (level name tail)
  "Return @code{(:data-item …)} for LEVEL, NAME, and clause TAIL, or NIL if unparseable."
  (let ((tail-up (string-upcase tail)))
    (cond
      ((search "OBJECT REFERENCE" tail-up)
       (or (cl-ppcre:register-groups-bind (cls) ("(?i)OBJECT\\s+REFERENCE\\s+(\\S+)" tail)
             (list :data-item :level level :name name :type-class cls :usage :object-ref))
           (list :blank)))
      (t
       (let* ((pic-str (%cpy-extract-pic-clause tail))
              (usage (cond ((search "PACKED-DECIMAL" tail-up) :decimal)
                           ((search "USAGE IS DECIMAL" tail-up) :decimal)
                           ((search "USAGE DECIMAL" tail-up) :decimal)
                           ((search "USAGE IS BINARY" tail-up) :binary)
                           ((search "USAGE BINARY" tail-up) :binary)
                           (t :binary)))
              (const-p (or (search "VALUE" tail-up) (search "VALUES" tail-up)))
              (value
                (or (cl-ppcre:register-groups-bind (hex)
                        ("(?i)VALUE\\s+IS\\s+CONSTANT\\s+X'([0-9A-Fa-f]+)'" tail)
                      (parse-integer hex :radix 16))
                    (cl-ppcre:register-groups-bind (dec)
                        ("(?i)VALUE\\s+IS\\s+(-?\\d+)" tail)
                      (parse-integer dec))
                    (cl-ppcre:register-groups-bind (dec)
                        ("(?i)VALUE\\s+(-?\\d+)" tail)
                      (parse-integer dec))))
              (frac-bits
                (cl-ppcre:register-groups-bind (i f) ("(?i)WITH\\s+(\\d+)\\s*V\\s*(\\d+)\\s*BITS" tail)
                  (declare (ignore i))
                  (parse-integer f))))
         (list :data-item
               :level level :name name
               :pic pic-str
               :usage usage
               :constant-p const-p
               :value value
               :pic-frac-bits frac-bits))))))

(defun parse-cpy-line (line)
  "Parse one physical COPYBOOK line into a small plist form.

Returns @code{(:section-comment …)}, @code{(:data-item …)}, @code{(:comment)}, or @code{(:blank)}.

@table @asis
@item LINE
Raw text from a @file{.cpy} file (may include leading spaces).
@end table"
  (let ((s (%cpy-strip-trailing-period (%cpy-trim line))))
    (cond
      ((zerop (length s))
       (list :blank))
      ((char= #\* (char s 0))
       (or (cl-ppcre:register-groups-bind (c) ("(?i)^\\*\\s*Inherited\\s+from\\s+(\\S+):" s)
             (list :section-comment :origin-class c))
           (cl-ppcre:register-groups-bind (c) ("(?i)^\\*\\s*Own\\s+slots\\s+\\(([^)]+)\\)" s)
             (list :section-comment :own-class c))
           (list :comment :text s)))
      (t
       (or (cl-ppcre:register-groups-bind (lev name tail) ("^(\\d+)\\s+([A-Za-z0-9][-A-Za-z0-9]*)\\s+(.*)$" s)
             (%parse-cpy-data-item (parse-integer lev) name tail))
           (list :blank))))))

(defun %origin-class-from-slots-cpy (path)
  "Read CLASS-ID from a generated @file{*-Slots.cpy} file, or NIL."
  (with-open-file (in path :direction :input :if-does-not-exist nil)
    (when in
      (loop for line = (read-line in nil nil) while line
            do (when-let (cls (cl-ppcre:register-groups-bind (c)
                                  ("(?i)CLASS-ID\\.\\s+([^\\.]+)" line)
                                (string-trim " " c)))
                 (return (header-case cls)))))))

(defun %merge-one-cpy-file (path slot-table type-table const-table
                            usage-table sign-table pic-size-table
                            pic-width-table pic-frac-bits-table
                            pic-nybble-semantics-table origin-class file-label)
  "Read PATH and merge rows into the various hash tables.

FILE-LABEL is @code{\"Phantasia-Globals\"} when merging globals so slot origins match tests."
  (declare (ignore sign-table))
  (when (probe-file path)
    (with-open-file (in path :direction :input :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil nil) while line do
          (let ((node (parse-cpy-line line)))
            (case (first node)
              (:section-comment
               (when-let (c (getf (rest node) :origin-class))
                 (setf origin-class c))
               (when-let (c (getf (rest node) :own-class))
                 (setf origin-class c)))
              (:data-item
               (let* ((name (getf (rest node) :name))
                      (key (cobol-slot-table-name-key name))
                      (pic (getf (rest node) :pic))
                      (usage (getf (rest node) :usage))
                      (type-class (getf (rest node) :type-class))
                      (o (if (and file-label (string-equal file-label "Phantasia-Globals"))
                             "Phantasia-Globals"
                             origin-class)))
                 (cond
                   (type-class
                    (setf (gethash key type-table) type-class))
                   (t
                    (setf (gethash key slot-table) o)
                    (when pic
                      (setf (gethash key pic-width-table)
                            (or (pic-digits-to-width pic :usage usage) 1))
                      (setf (gethash key pic-size-table) (gethash key pic-width-table))
                      (when (eq usage :binary)
                        (setf (gethash key usage-table) :binary))
                      (when (eq usage :decimal)
                        (setf (gethash key usage-table) :decimal))
                      (when-let (fb (getf (rest node) :pic-frac-bits))
                        (setf (gethash key pic-frac-bits-table) fb))
                      (when (pic-nybble-semantics-p pic)
                        (setf (gethash key pic-nybble-semantics-table) t))
                      (when (getf (rest node) :constant-p)
                        (when-let (v (getf (rest node) :value))
                          (setf (gethash key const-table) v)))))))))))))))

(defun load-copybook-tables (class-id &key root-dir)
  "Load generated COPYBOOK files for CLASS-ID into compiler tables.

Returns (values slot-table type-table const-table service-bank-table usage-table sign-table
pic-size-table pic-width-table pic-frac-bits-table pic-nybble-semantics-table) as fresh tables.

@table @asis
@item CLASS-ID
PascalCase class name (e.g. @code{\"Character\"}).
@item ROOT-DIR
Project root; used for @code{AssetIDs.cpy} and service-bank scan.
@end table"
  (let ((slot-table (make-hash-table :test 'equalp))
        (type-table (make-hash-table :test 'equalp))
        (const-table (make-hash-table :test 'equalp))
        (service-bank-table (make-hash-table :test 'equalp))
        (usage-table (make-hash-table :test 'equalp))
        (sign-table (make-hash-table :test 'equalp))
        (pic-size-table (make-hash-table :test 'equalp))
        (pic-width-table (make-hash-table :test 'equalp))
        (pic-frac-bits-table (make-hash-table :test 'equalp))
        (pic-nybble-semantics-table (make-hash-table :test 'equalp))
        (origin-class class-id)
        (root (uiop:ensure-directory-pathname (or root-dir *eightbol-root-directory* (truename "."))))
        (copy-base (when *copybook-paths*
                     (uiop:ensure-directory-pathname (first *copybook-paths*)))))
    (declare (ignore sign-table))
    (when copy-base
      (let ((slots-path (merge-pathnames
                         (make-pathname :name (format nil "~a-Slots" (class-id-to-copybook-filename class-id))
                                        :type "cpy")
                         copy-base))
            (globals-path (merge-pathnames (make-pathname :name "Phantasia-Globals" :type "cpy") copy-base)))
        (%merge-one-cpy-file slots-path slot-table type-table const-table
                             usage-table sign-table pic-size-table pic-width-table
                             pic-frac-bits-table pic-nybble-semantics-table origin-class nil)
        (%merge-one-cpy-file globals-path slot-table type-table const-table
                             usage-table sign-table pic-size-table pic-width-table
                             pic-frac-bits-table pic-nybble-semantics-table origin-class
                             "Phantasia-Globals")
        (let ((classes-cpy (merge-pathnames #p"Classes.cpy" copy-base)))
          (when (probe-file classes-cpy)
            (dolist (slots (directory (merge-pathnames #p"*-Slots.cpy" copy-base)))
              (let ((slots-origin (or (%origin-class-from-slots-cpy slots) origin-class)))
                (%merge-one-cpy-file slots slot-table type-table const-table
                                     usage-table sign-table pic-size-table pic-width-table
                                     pic-frac-bits-table pic-nybble-semantics-table
                                     slots-origin nil)))))))
    (let ((machine (infer-machine-from-copybook-paths)))
      (when machine
        (let ((asset-ids (merge-pathnames
                          (make-pathname :directory `(:relative "Source" "Generated" ,machine)
                                         :name "AssetIDs" :type "cpy")
                          root)))
          (%merge-one-cpy-file asset-ids slot-table type-table const-table
                               usage-table sign-table pic-size-table pic-width-table
                               pic-frac-bits-table pic-nybble-semantics-table origin-class nil))))
    (let ((machine (infer-machine-from-copybook-paths)))
      (when machine
        (build-service-bank-lut-from-banks root machine)))
    (maphash (lambda (k v) (setf (gethash k service-bank-table) v)) *service-bank-lut*)
    (values slot-table type-table const-table service-bank-table usage-table sign-table
            pic-size-table pic-width-table pic-frac-bits-table pic-nybble-semantics-table)))
