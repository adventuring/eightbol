;; tests/phantasia-method-port-tests.lisp — Classes.Defs method introductions vs .cob + legacy *Class.s
;;
;; Run: (asdf:test-system :eightbol) — suite :phantasia-method-port

(in-package :eightbol/test)

(fiveam:def-suite :phantasia-method-port
  :description "Phantasia: Defs # methods, EIGHTBOL .cob METHOD-ID, OOPS label regression")

(fiveam:def-suite :phantasia-method-port-legacy
  :description "Optional: legacy *Class.s Method* labels vs .cob METHOD-ID (override parity)")

(in-suite :phantasia-method-port)

(defun normalize-method-dispatch-name (name)
  "Return PascalCase assembly suffix for METHOD-ID or Defs method NAME (string/symbol)."
  (cobol-id-to-assembly-symbol (format nil "~a" name)))

(defun cob-file-method-ids-normalized (cob-path)
  "Collect normalized dispatch names from METHOD-ID clauses in COB-PATH."
  (let ((names '()))
    (with-open-file (in cob-path :direction :input
                                 :element-type 'character
                                 :external-format :utf-8)
      (loop for line = (read-line in nil nil)
            while line
            do (cl-ppcre:register-groups-bind (quoted unquoted)
                   ("(?i).*METHOD-ID\\.\\s*(?:\"([^\"]+)\"|([A-Za-z0-9À-ÿ]+))\\s*\\." line)
                 (let ((raw (or (and quoted (plusp (length quoted)) quoted)
                                (and unquoted (plusp (length unquoted)) unquoted))))
                   (when raw
                     (push (normalize-method-dispatch-name raw) names))))))
    (remove-duplicates (nreverse names) :test #'string=)))

(defun defs-pair-check-method-in-cob (pair root)
  "Assert Defs method PAIR (class . method-name) has normalized METHOD-ID in class .cob under ROOT."
  (let* ((class (car pair))
         (method (cdr pair))
         (cob (merge-pathnames (make-pathname :name class :type "cob")
                               (merge-pathnames "Source/Classes/" root))))
    (is (probe-file cob) "Missing .cob for class ~a: ~a" class cob)
    (when (probe-file cob)
      (let ((have (cob-file-method-ids-normalized cob))
            (want (normalize-method-dispatch-name method)))
        (is (member want have :test #'string=)
            "Class ~a must define METHOD-ID for Defs method ~a (~a); have ~s"
            class method want have)))))

(defun legacy-class-s-assert-methods-in-cob (path cname root)
  "For legacy *Class.s PATH with class CNAME, assert each Method* label has METHOD-ID in .cob."
  (let* ((cob (merge-pathnames (make-pathname :name cname :type "cob")
                               (merge-pathnames "Source/Classes/" root)))
         (suffixes (legacy-method-suffixes-from-class-s path cname))
         (cob-methods (when (probe-file cob) (cob-file-method-ids-normalized cob))))
    (when cob-methods
      (dolist (suf suffixes)
        (is (member suf cob-methods :test #'string=)
            "Legacy ~a defines Method~a~a; add matching METHOD-ID to ~a"
            path cname suf cob)))))

(defun legacy-class-name-from-class-s-path (path)
  "Return class name from *Class.s filename (e.g. CuttingWeaponClass.s → CuttingWeapon), or NIL."
  ;; pathname-name omits the type; compare the full basename (name.type) for a Class.s suffix.
  (let* ((name (pathname-name path))
         (type (pathname-type path))
         (n (if (and name type)
                (concatenate 'string name "." type)
                name)))
    (when (and n (> (length n) (length "Class.s"))
                (string-equal "Class.s" n :start2 (- (length n) (length "Class.s"))))
      (subseq n 0 (- (length n) (length "Class.s"))))))

(defun legacy-method-suffixes-from-class-s (path class-name)
  "Collect normalized method suffixes from Method<Class><Method> labels in PATH."
  (let* ((prefix (format nil "Method~a" class-name))
         (text (alexandria:read-file-into-string path))
         (lines (uiop:split-string text :separator (string #\newline)))
         (out '()))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (unless (or (zerop (length trimmed)) (char= #\; (char trimmed 0)))
          (when (and (>= (length trimmed) (+ (length prefix) 2))
                     (string= trimmed prefix :end1 (length prefix)))
            (let* ((rest (subseq trimmed (length prefix)))
                   (colon (position #\: rest)))
              (when colon
                (let ((after-colon (string-trim '(#\Space #\Tab #\Return)
                                              (subseq rest (min (1+ colon) (length rest))))))
                  ;; Skip alias lines (= …) and one-instruction stubs (e.g. ": rts") that are
                  ;; not EIGHTBOL procedure bodies; porting parity targets real .block methods.
                  (unless (or (search "=" trimmed)
                              (member after-colon '("rts" "") :test #'string-equal))
                    (push (normalize-method-dispatch-name (subseq rest 0 colon)) out)))))))))
    (remove-duplicates (nreverse out) :test #'string=)))

(test phantasia-method-port/defs-introductions-sanity
  "list-classes-defs-method-introductions matches expected # count when Phantasia present."
  (unless (phantasia-project-root)
    (skip "Phantasia project root not found"))
  (let* ((defs (phantasia-classes-defs-path))
         (pairs (skyline-tool:list-classes-defs-method-introductions defs)))
    (is (= 34 (length pairs))
        "Expected 34 # method lines in Classes.Defs, got ~d" (length pairs))))

(test phantasia-method-port/defs-method-in-cob
  "Each Classes.Defs # introduction has matching METHOD-ID in introducing class .cob."
  (unless (phantasia-project-root)
    (skip "Phantasia project root not found"))
  (let* ((root (phantasia-project-root))
         (pairs (skyline-tool:list-classes-defs-method-introductions
                 (phantasia-classes-defs-path))))
    (dolist (pair pairs)
      (defs-pair-check-method-in-cob pair root))))

(in-suite :phantasia-method-port-legacy)

(test phantasia-method-port-legacy/class-s-parity
  "Each real .block Method* label in remaining *Class.s has METHOD-ID in matching .cob.

Run (fiveam:run! :phantasia-method-port-legacy) when checking override parity; alias lines
and one-line stubs (e.g. \": rts\") are ignored."
  (unless (phantasia-project-root)
    (skip "Phantasia project root not found"))
  (let* ((root (phantasia-project-root))
         (legacy-dir (merge-pathnames "Source/Code/7800/Classes/" root)))
    (unless (probe-file legacy-dir)
      (skip "Legacy Classes directory not found"))
    (dolist (path (directory (merge-pathnames "*Class.s" legacy-dir)))
      (let ((cname (legacy-class-name-from-class-s-path path)))
        (when cname
          (legacy-class-s-assert-methods-in-cob path cname root))))))

(defparameter *phantasia-entity-label-regression-cob-text*
  "000000** Fixture — Entity minimal (METHOD-ID Get-Bounds label vs OOPS)
000030 IDENTIFICATION DIVISION.
000040 CLASS-ID. Entity.
000042 INHERITS. BasicObject.
000050 AUTHOR. Bruce-Robert Pocock.
000060 DATE-WRITTEN. 2026.
000080 ENVIRONMENT DIVISION.
000095 DATA DIVISION.
000096     WORKING-STORAGE SECTION.
000097         COPY Phantasia-Globals.
000100 OBJECT.
000110     DATA DIVISION.
000120         COPY Entity-Slots.
000150     PROCEDURE DIVISION.
000330         IDENTIFICATION DIVISION.
000332         METHOD-ID. \"Get-Bounds\".
000340         PROCEDURE DIVISION.
000350             MOVE 3 TO Bound-Width.
000360             MOVE 3 TO Bound-Height.
000370             GOBACK.
000380         END METHOD \"Get-Bounds\".
000400 END OBJECT.
000420 END CLASS Entity.
999999
")

(test phantasia-method-port/entity-get-bounds-label-matches-oops
  "6502 output uses MethodEntityGetBounds (no hyphen) for METHOD-ID. \"Get-Bounds\"."
  (unless (phantasia-project-root)
    (skip "Phantasia project root not found"))
  (unless (phantasia-globals-cpy)
    (skip "Phantasia-Globals.cpy not found"))
  (let* ((root (phantasia-project-root))
         (defs (phantasia-classes-defs-path))
         (globals-dir (uiop:pathname-directory-pathname (phantasia-globals-cpy)))
         (tmp (uiop:ensure-directory-pathname
               (merge-pathnames
                (make-pathname :directory (list :relative "phantasia-entity-label-"
                                                (format nil "~a" (get-internal-real-time))))
                (uiop:temporary-directory))))
         (tmp-classes (merge-pathnames "Source/Generated/7800/Classes/" tmp))
         (entity-cob (merge-pathnames "Entity-label-regression.cob" tmp)))
    (ensure-directories-exist tmp-classes)
    (with-open-file (out entity-cob :direction :output :if-exists :supersede
                                   :external-format :utf-8)
      (write-string *phantasia-entity-label-regression-cob-text* out))
    (let ((skyline-tool::*machine* 7800))
      (uiop:call-with-current-directory tmp
        (lambda ()
          (skyline-tool::make-eightbol-copybooks defs))))
    (let ((copybook-paths (list (uiop:ensure-directory-pathname tmp-classes)
                                (uiop:ensure-directory-pathname globals-dir))))
      (compile-eightbol-class (list entity-cob)
                              :cpus '(:6502)
                              :copybook-paths copybook-paths
                              :root-directory tmp))
    (let ((out (merge-pathnames
                (make-pathname
                 :directory (list :relative "Source" "Generated" "Classes"
                                 (string (eightbol::cpu-display-name :6502)))
                 :name "EntityClass"
                 :type "s")
                tmp)))
      (is (probe-file out))
      (let ((asm (alexandria:read-file-into-string out)))
        (is (search "MethodEntityGetBounds" asm))
        (is (not (search "MethodEntityGet-Bounds" asm)))))))
