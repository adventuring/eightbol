;; src/eightbol-compile.lisp — top-level EIGHTBOL compilation pipeline
;;
;; compile-eightbol-class orchestrates:
;;   1. Lex (with COPY expansion) → YACC parse → AST plist
;;   2. Write AST sexp to  Object/Classes/{ClassName}.eightbol
;;   3. For each target CPU: write assembly to
;;      Source/Generated/Classes/{cpu}/{ClassName}Class.s
;;      where {cpu} uses case-preserving names (6502, 65c02, HuC6280, cp1610, RP2A03, Z80, 65c816, m68k, i286, etc.)
(in-package :eightbol)

;;; Case-preserving display  labels for UI (help  text, error messages).
;;; Input matching is case-insensitive (string-equal); display preserves
;;; intended case.
(define-constant +cpu-display-names+
    '((:6502 . "6502")
      (:65c02 . "65c02")
      (:65c816 . "65c816")
      (:cp1610 . "cp1610")
      (:huc6280 . "HuC6280")
      (:rp2a03 . "RP2A03")
      (:z80 . "Z80")
      (:sm83 . "SM83")
      (:m68k . "m68k")
      (:i286 . "i286")
      (:arm7 . "ARM7"))
  :test 'equalp)

;;; All  supported  CPU keywords,  in  order  of increasing  complexity.
;;; Use  plain  keywords   (no  pipe  escaping);  display   case  is  in
;;; +cpu-display-names+.
(define-constant +supported-cpus+
    (mapcar #'first +cpu-display-names+)
  :test 'equal)

(defun cpu-display-name (cpu)
  "Return the case-preserving UI label for CPU."
  (rest (assoc cpu +cpu-display-names+)))

(defun cpu-directory-name (&optional (cpu *cpu*))
  "Return the canonical output directory component for a CPU keyword.
Uses display names: cp1610, 65c02, RP2A03, m68k, i286, Z80, etc."
  (cpu-display-name cpu))

(defun default-copybook-paths (root-directory)
  "Return the default list of copybook search paths relative to ROOT-DIRECTORY.
  Path: Source/Generated/Classes/{cpu}"
  (list (make-pathname :defaults root-directory
                  :directory (list :relative "Source" "Generated"
                                   "Classes" (cpu-directory-name)))))

(defun write-copybook-deps (input-file class-id cpus root-directory output-file
                            copybook-deps)
  "Write a Makefile .d file listing assembly targets and their dependencies.
Enables correct rebuilds when copybooks change. Includes bin/eightbol so
rebuilding the compiler triggers recompilation of all .s outputs."
  (let* ((input-truename (truename input-file))
         (eightbol-bin (merge-pathnames #p"bin/eightbol" root-directory))
         (deps (list* input-truename eightbol-bin (nreverse copybook-deps)))
         (targets
           (loop for cpu in cpus
                 for first-p = t then nil
                 collect (if (and first-p output-file (not (string= output-file "")))
                             (merge-pathnames (pathname output-file) root-directory)
                             (merge-pathnames
                              (make-pathname
                               :directory (list :relative "Source" "Generated" "Classes"
                                                (cpu-directory-name cpu))
                               :name (concatenate 'string class-id "Class") :type "s")
                              root-directory)))))
    (let ((d-path (merge-pathnames
                   (make-pathname :directory '(:relative "Source" "Generated" "Classes")
                                 :name class-id :type "d")
                   root-directory)))
      (ensure-directories-exist d-path)
      (with-open-file (out d-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (format out "~{~a~^ ~}: ~{~a~^ ~}~%"
                (mapcar #'namestring targets)
                (mapcar #'namestring deps))))))

(defun compile-eightbol-class
    (input-files
     &key (cpus '(:6502))
          copybook-paths
          (root-directory (truename "."))
          output-file)
  "Compile INPUT-FILE (.cob):
   1. Parse to AST and write to  {root}/Object/Classes/{ClassName}.eightbol
   2. For each cpu in CPUS compile to
      {root}/Source/Generated/Classes/{cpu}/{ClassName}Class.s
      (or OUTPUT-FILE for the single CPU when specified)
   3. Write {root}/Source/Generated/Classes/{ClassName}.d for Makefile includes
Returns the AST plist."
  (let ((*eightbol-root-directory* root-directory)
        (*copybook-paths* (or copybook-paths
                              (default-copybook-paths root-directory)))
        (*copybook-dependencies* ()))
    ;; ── Phase 1: parse ──────────────────────────────────────────
    (let ((ast (dolist (input-file input-files)
                 (handler-case
                     (with-open-file (stream input-file :direction :input)
                       (let ((*source-file-pathname* (enough-namestring input-file)))
                         (parse-eightbol stream)))
                   (source-error (e)
                     ;; Re-signal with file context filled in if the lexer didn't
                     ;; already capture the source pathname.
                     (when (and (null (eightbol-error-file e))
                                input-file)
                       (error 'source-error
                              :source-file     (pathname-name input-file)
                              :source-line     (eightbol-error-line e)
                              :source-sequence (eightbol-error-sequence e)
                              :terminal        (eightbol-error-terminal e)
                              :token-value     (eightbol-error-value e)
                              :expected        (eightbol-error-expected e)
                              :message         (eightbol-error-message e)))
                     (error e))))))
      (unless (and (listp ast) (eq (first ast) :program))
        (error "EIGHTBOL: parse of ~a did not yield a :program node" input-file))
      (setf ast (optimize-ast ast))
      (let ((class-id (ast-class-id ast)))
        ;; ── Phase 2: write AST ───────────────────────────────────
        (let ((ast-path (merge-pathnames
                         (make-pathname
                          :directory '(:relative "Object" "Classes")
                          :name class-id :type "eightbol")
                         root-directory)))
          (ensure-directories-exist ast-path)
          (with-open-file (out ast-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (write-ast ast out))
          (format t  "~2&EIGHTBOL: wrote AST to ~a" (enough-namestring ast-path)))
        ;; ── Phase 3: backends ────────────────────────────────────
        (do ((cpu-list cpus (rest cpu-list))
             (cpu (first cpus) (first cpu-list))
             (first-p t nil))
            ((null cpu-list))
          (handler-case
              (let ((asm-path (if (and first-p output-file)
                                 (pathname output-file)
                                 (merge-pathnames
                                  (make-pathname
                                   :directory (list :relative "Source" "Generated" "Classes"
                                                    (cpu-directory-name cpu))
                                   :name (concatenate 'string class-id "Class") :type "s")
                                  root-directory))))
                (ensure-directories-exist asm-path)
                (with-open-file (out asm-path
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                  (compile-to-assembly ast cpu out))
                (format t  "~2&EIGHTBOL: wrote ~a assembly to ~a"
                        (cpu-display-name cpu) (enough-namestring asm-path)))
            (error (e)
              (format *error-output*  "~2&EIGHTBOL: error compiling ~a for ~a: ~a"
                      class-id (cpu-display-name cpu) e)
              (error e))))
        ;; ── Phase 4: dependency file for Make ─────────────────────
        (write-copybook-deps input-file class-id cpus root-directory output-file
                            *copybook-dependencies*)
        ast))))

(defun compile-eightbol-class-from-ast
    (ast
     &key (cpus *supported-cpus*)
          (root-directory (truename "."))
          output-file)
  "Re-compile from a previously-parsed (or loaded) AST plist.
Writes only assembly output (no AST write)."
  (unless (and (listp ast) (eq (first ast) :program))
    (error "EIGHTBOL: not a :program AST node"))
  (let ((class-id (ast-class-id ast)))
    (do ((cpu-list cpus (rest cpu-list))
         (cpu (first cpus) (first cpu-list))
         (first-p t nil))
        ((null cpu-list))
      (handler-case
          (let ((asm-path (if (and first-p output-file)
                             (pathname output-file)
                             (merge-pathnames
                              (make-pathname
                               :directory (list :relative "Source" "Generated" "Classes"
                                                (cpu-directory-name cpu))
                               :name (concatenate 'string class-id "Class") :type "s")
                              root-directory))))
            (ensure-directories-exist asm-path)
            (with-open-file (out asm-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (compile-to-assembly ast cpu out)))
        (error (e)
          (format *error-output*  "~2&EIGHTBOL: error compiling ~a for ~a: ~a"
                  class-id (cpu-display-name cpu) e)
          (error e))))))

(defun load-eightbol-ast (ast-file)
  "Read a previously-serialised AST from AST-FILE (.eightbol)."
  (with-open-file (stream ast-file :direction :input)
    (read-ast stream)))
