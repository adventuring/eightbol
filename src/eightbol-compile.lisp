;;; src/eightbol-compile.lisp — top-level EIGHTBOL compilation pipeline
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;
;; compile-eightbol orchestrates:
;;   1. Lex (with COPY expansion) → YACC parse → AST plist
;;   2. Write AST sexp to  Object/Classes/{ClassName}.eightbol
;;   3. For each target CPU: write assembly to
;;      Source/Generated/Classes/{cpu}/{ClassName}Class.s
;;      where {cpu} uses case-preserving names (6502, 65c02, HuC6280, cp1610, RP2A03, Z80, 65c816, m68k, i286, etc.)
(in-package :eightbol)

(defun parse-cli-cpu-arg (display-string)
  "Parse CLI @code{-m} DISPLAY-STRING to CPU keyword or @code{:all}.
When DISPLAY-STRING is not @code{\"all\"} and not a known CPU label (case-
insensitive), signal @code{unknown-cpu-error}.

@table @asis
@item DISPLAY-STRING
Argument after @code{-m} (e.g. @code{\"6502\"}, @code{\"all\"}).
@end table"
  (let ((s (string display-string)))
    (cond ((string-equal s "all") :all)
          (t (let ((pair (find s +cpu-display-names+
                               :test #'string-equal :key #'cdr)))
               (if pair
                   (car pair)
                   (error 'unknown-cpu-error
                          :cpu s
                          :known-cpus (mapcar #'cdr +cpu-display-names+))))))))

(defun default-copybook-paths (root-directory &optional cpu)
  "Return the default list of copybook search paths relative to ROOT-DIRECTORY.
Path: Source/Generated/Classes/{cpu-display-name}. CPU defaults to @code{*cpu*}, or
@code{:6502} when @code{*cpu*} is unbound (so paths never contain a NIL directory
component when @code{compile-eightbol} runs outside @code{compile-to-assembly}).

@table @asis
@item ROOT-DIRECTORY
Project root; merged with the relative @code{Source/Generated/Classes/…} path.
@item CPU
Optional target ISA keyword (e.g. @code{:6502}); passed by @code{compile-eightbol}
as @code{(first cpus)} so the copybook directory matches the first backend target.
@end table

@subsection Outputs
A one-element list of pathname designators."
  (let ((c (or cpu *cpu* :6502)))
    (list (make-pathname :defaults root-directory
		     :directory (list :relative "Source" "Generated"
				  "Classes" (cpu-directory-name c))))))

(defun %generated-classes-copybook-dir-p (dir)
  "True when DIR exists and contains @file{Classes.cpy} (Generated …/Classes tree)."
  (and (probe-file dir)
       (probe-file (merge-pathnames (make-pathname :name "Classes" :type "cpy")
                                    (uiop:ensure-directory-pathname dir)))))

(defun project-copybook-paths (root-directory &optional cpu)
  "Return COPY search directories for Phantasia-style class sources.

Lists each @code{Source/Generated/@var{machine}/Classes/} that contains
@file{Classes.cpy} and globals copybooks first so @code{load-copybook-tables}
(which reads @code{*-Slots.cpy} and @code{Phantasia-Globals.cpy} from the
@emph{first} path only) matches COBOL builds.  Then @code{Source/Classes}, then
@code{default-copybook-paths} for CPU (flat @code{Source/Generated/Classes/@var{cpu}/}
fallback).  This mirrors Skyline Tool @code{eightbol-compile} path set while
satisfying the first-directory merge rule.

BASIC compilation uses this list when @code{:copybook-paths} is not supplied so
@code{COPY Classes.}, @code{COPY …-Globals.}, and @code{COPY Asset-IDs.} resolve
like @file{Source/Classes/*.cob}.

@table @asis
@item ROOT-DIRECTORY
Project root (directory pathname).
@item CPU
Optional ISA keyword for the final @code{default-copybook-paths} entry; defaults
as for @code{default-copybook-paths}.
@end table

@subsection Outputs
Ordered list of directory pathnames suitable for @code{compile-eightbol}
@code{:copybook-paths}."
  (let ((root (uiop:ensure-directory-pathname root-directory))
        (ordered '()))
    (flet ((add (dir)
             (when dir
               (let ((d (uiop:ensure-directory-pathname (merge-pathnames dir root))))
                 (when (probe-file d)
                   (unless (member d ordered :test #'equalp)
                     (setf ordered (append ordered (list d)))))))))
      (let ((gen (merge-pathnames #p"Source/Generated/" root)))
        (when (probe-file gen)
          (dolist (mach (uiop:directory* (merge-pathnames #p"*/" gen)))
            (let ((d (merge-pathnames #p"Classes/" mach)))
              (when (%generated-classes-copybook-dir-p d)
                (add d))))))
      (add #p"Source/Classes/")
      (dolist (p (default-copybook-paths root-directory cpu))
        (add p)))
    ordered))

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
                 collect (if (and first-p
                                  output-file
                                  (not (zerop (length (namestring (pathname output-file))))))
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

(defun compile-eightbol (input-files
		     &key (cpus '(:6502))
			copybook-paths
			(root-directory (truename #p"."))
			output-file
			ast-output-file)
  "Compile INPUT-FILES (.cob pathnames): one file or a list.

Skyline-Tool invokes this via APPLY with a single path string; normalize that
to a one-element list so DOLIST does not iterate characters.

   1. Parse to AST and write to  {root}/Object/Classes/{ClassName}.eightbol
      (or AST-OUTPUT-FILE when provided)

   2. For each cpu in CPUS compile to
      {root}/Source/Generated/Classes/{cpu}/{ClassName}Class.s
      (or OUTPUT-FILE for the single CPU when specified)

   3. Write {root}/Source/Generated/Classes/{ClassName}.d for Makefile includes

Returns the AST plist."
  (restart-case
      (setf input-files
            (cond
              ((null input-files)
               (error "EIGHTBOL: INPUT-FILES is empty"))
              ((and (consp input-files)
                    (every (lambda (x) (or (pathnamep x) (stringp x))) input-files))
               (mapcar (lambda (x) (if (pathnamep x) x (pathname x))) input-files))
              ((or (pathnamep input-files) (stringp input-files))
               (list (if (pathnamep input-files) input-files (pathname input-files))))
              (t
               (error "EIGHTBOL: INPUT-FILES must be a pathname designator or a proper ~
  list of pathname designators, got ~s"
                      input-files))))
    (retry-compile ()
      :report "Retry the entire compilation process from the beginning."
      (compile-eightbol input-files
                        :cpus cpus
                        :copybook-paths copybook-paths
                        :root-directory root-directory
                        :output-file output-file
                        :ast-output-file ast-output-file)))
  (let ((*eightbol-root-directory* root-directory)
        (*copybook-paths* (or copybook-paths
                              (default-copybook-paths root-directory (first cpus))))
        (*copybook-dependencies* ())
        (optimum))
    (restart-case
        (progn
          (dolist (input-file input-files)
            (let ((ast (with-open-file (stream input-file :direction :input)
                         (let* ((*source-file-pathname* (enough-namestring input-file))
                                (*current-token-location* (list :source-file *source-file-pathname*
                                                                :source-line nil
                                                                :source-sequence nil)))
                           (parse-eightbol stream)))))
              (unless (and (listp ast)
                           (every #'listp ast)
                           (every (lambda (section)
                                    (eq (first section) :program))
                                  ast))
                (error "EIGHTBOL: parse of ~{~a~^, ~} did not yield a Program node~%~s"
                       input-files ast))
              (appendf optimum (remove-if #'null (mapcar #'optimize-ast ast)))))
          (let ((*class-id* (ast-class-id optimum))
                (*program-id* (ast-program-id optimum)))
            ;;  Phase 2: write AST 
            (let ((ast-path (or ast-output-file
                                (make-pathname
                                 :directory (if *class-id*
                                                '(:relative "Object" "Classes")
                                                '(:relative "Object" "Eightbol"))
                                 :name (or *class-id* *program-id*) :type "eightbol"))))
              (ensure-directories-exist ast-path)
              (with-open-file (out ast-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
                (write-ast optimum out))
              (format t "~2&EIGHTBOL: wrote AST to ~a" (enough-namestring ast-path)))
            ;;  Phase 3: backends 
            ;; Step CPU from (REST CPU-LIST): (FIRST CPU-LIST) would repeat the first
            ;; CPU and skip the last (e.g. F8 never emitted in ALL-BACKENDS-ONE-FIXTURE).
            (dolist (cpu cpus)
              (handler-case
                  (let ((asm-path (if (and (eql cpu (first cpus)) output-file)
                                      (pathname output-file)
                                      (merge-pathnames
                                       (make-pathname
                                        :directory (list :relative "Source" "Generated" "Classes"
                                                         (cpu-directory-name cpu))
                                        :name
                                        (if *class-id*
                                            (concatenate 'string (pascal-case *class-id*) "Class")
                                            (pascal-case *program-id*))
                                        :type "s")
                                       root-directory))))
                    (ensure-directories-exist asm-path)
                    (with-open-file (out asm-path
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                      (dolist (module optimum)
                        (compile-to-assembly module cpu out)))
                    (format t "~2&EIGHTBOL: wrote ~a assembly to ~a"
                            (cpu-display-name cpu) (enough-namestring asm-path)))
                (error (e)
                  (format *error-output* "~2&EIGHTBOL: error compiling ~a for ~a: ~a"
                          (or *class-id* *program-id* "?") (cpu-display-name cpu) e)
                  (error e))))
            ;; ;;  Phase 4: dependency file for Make 
            ;; (write-copybook-deps (or primary-input-file (first input-files))
            ;;                      class-id cpus root-directory output-file
            ;;                      *copybook-dependencies*)
            optimum))
      (retry-compile ()
        :report "Retry the entire compilation process from the beginning."
        (compile-eightbol input-files
                          :cpus cpus
                          :copybook-paths copybook-paths
                          :root-directory root-directory
                          :output-file output-file
                          :ast-output-file ast-output-file)))))

(defun compile-to-assembly-with-ast-passes (ast cpu output-stream
                                            &key (validate-termination t))
  "Run optimize-ast (routine AST optimizations), optional VALIDATE-EIGHTBOL-PROGRAM, then compile.
Use when AST comes from parse only (e.g. unit tests). `compile-eightbol`
already calls optimize-ast before `compile-to-assembly`.

When VALIDATE-TERMINATION is NIL, skip @code{validate-method-terminations} (e.g. synthetic ASTs
with non-terminating tails for tail-call layout tests)."
  (dolist (module (remove-if #'null
                             (mapcar #'optimize-ast
                                     (if (and (listp ast)
                                              (listp (first ast))
                                              (eq (first (first ast)) :program))
                                         ast
                                         (list ast)))))
    (validate-eightbol-program module :validate-termination validate-termination)
    (compile-to-assembly module cpu output-stream))
  (format output-stream "~%"))

(defun parse-eightbol-string-for-codegen (string &optional (pathname "<String>"))
  "Parse STRING and apply optimize-ast so the result matches codegen input."
  (optimize-ast (parse-eightbol-string string pathname)))

(defun compile-eightbol-from-ast (ast &key (cpus +supported-cpus+))
  "Re-compile from a previously-parsed (or loaded) AST plist.
Writes only assembly output (no AST write). Applies optimize-ast so the same
routine AST passes run as in `compile-eightbol`."
  (unless (and (listp ast) (eq (first ast) :program))
    (error "EIGHTBOL: not a :program AST node"))
  (setf ast (optimize-ast ast))
  (let ((class-id (ast-class-id ast)))
    (dolist (cpu cpus)
      (handler-case
          (let ((asm-path (if class-id
                              (make-pathname
                               :directory (list :relative "Source" "Generated" "Classes"
                                                (cpu-directory-name cpu))
                               :name (concatenate 'string class-id "Class") :type "s")
                              
                              (make-pathname
                               :directory (list :relative "Source" "Generated" "MapsRC"
                                                (cpu-directory-name cpu))
                               :name (ast-program-id ast) :type "s"))))
            (ensure-directories-exist asm-path)
            (with-open-file (out asm-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (compile-to-assembly ast cpu out)))
        (error (e)
          (format *error-output* "~2&EIGHTBOL: error compiling ~a for ~a: ~a"
                  class-id (cpu-display-name cpu) e)
          (error e))))))

(defun load-eightbol-ast (ast-file)
  "Read a previously-serialised AST from AST-FILE (.eightbol)."
  (with-open-file (stream ast-file :direction :input)
    (read-ast stream)))

(defvar *class-methods* (make-hash-table :test 'equalp))
(defvar *parent-classes* (make-hash-table :test 'equalp))

(defun method-class (class-id method-id)
  (let ((canon-method (pascal-case method-id))
        (most-general-class nil))
    (if (member method-id (list "Destroy" "Class-P") :test #'string-equal)
        (return-from method-class "Basic-Object")
        (when (equal "Basic-Object" class-id)
          (return-from method-class "Basic-Object")))
    (unless (gethash class-id *parent-classes*)
      (load-classes))
    (let ((consider-class class-id)
          (seen (list "Destroy" "Class-P")))
      (loop
         (push (gethash consider-class *class-methods*) seen)
         (when (find canon-method (gethash consider-class *class-methods*)
	           :test #'string-equal)
	 (setf most-general-class consider-class))
         (setf consider-class (gethash consider-class *parent-classes*))
         (when (or (null consider-class) (equal consider-class "BasicObject"))
           (if most-general-class
               (return-from method-class most-general-class)
                (return-from method-class class-id)))))))

(defun slot-class (object slot-id
                   &optional (class-id
                              (header-case (if (string-equal "Self" object)
                                               *class-id*
                                               (oops-class-of object)))))
  (when (and (listp slot-id) (eql :subscript (first slot-id)))
    (setf slot-id (second slot-id)))
  (when (string-equal "Destroy" slot-id)
    (return-from slot-class "Basic-Object"))
  (unless (gethash class-id *parent-classes*)
    (load-classes))
  (let ((consider-class class-id))
    (loop
       (when (or (gethash (list :of slot-id consider-class) *working-storage*)
                 (when *slot-table*
                   (let ((origin (gethash (cobol-slot-table-name-key slot-id) *slot-table*)))
                     (and origin (string-equal origin (header-case consider-class))))))
         (return-from slot-class (header-case consider-class)))
       (setf consider-class (gethash consider-class *parent-classes*))
        (when (or (null consider-class)
                  (equal consider-class "Basic-Object"))
          (if (string-equal class-id (oops-class-of object))
              ;; When the hierarchy walk fails, check if the slot exists
              ;; in ANY class via *slot-table* origin — the slot may be
              ;; defined in a derived class that the walk never reaches.
              (if-let ((origin (when *slot-table*
                                (gethash (cobol-slot-table-name-key slot-id)
                                         *slot-table*))))
                  (return-from slot-class (header-case origin))
                  (error "~s is not a slot of class ~s, nor its parent classes up to ~s"
                         slot-id class-id consider-class))
              (slot-class object slot-id (oops-class-of object)))))))

(defun load-classes ()
  (when (plusp (hash-table-count *parent-classes*))
    (return-from load-classes))
  (with-input-from-file (classes.defs (pathname "Source/Classes/Classes.Defs"))
    (loop with last-class = "BasicObject"
	for line = (read-line classes.defs nil nil)
	while line
	do (cond ((search "<" line)
		(destructuring-bind (child parent)
		    (mapcar (lambda (word)
			    (string-trim #(#\Space #\Tab) word))
			  (split-sequence #\< line))
		  (setf (gethash (header-case child) *parent-classes*) (header-case parent)
		        last-class (header-case child))))
	         ((and (< 2 (length line)) (char= #\# (char line 0)))
		(appendf (gethash last-class *class-methods* '())
		         (cons (subseq line 1) nil)))))))
