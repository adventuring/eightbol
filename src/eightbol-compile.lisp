;; src/eightbol-compile.lisp — top-level EIGHTBOL compilation pipeline
;;
;; compile-eightbol-class orchestrates:
;;   1. Lex (with COPY expansion) → YACC parse → AST plist
;;   2. Write AST sexp to  Object/Classes/{ClassName}.eightbol
;;   3. For each target CPU: write assembly to
;;      Source/Generated/Classes/{cpu}/{ClassName}Class.s
;;      where {cpu} uses case-preserving names (6502, 65c02, HuC6280, cp1610, RP2A03, Z80, 65c816, m68k, i286, etc.)
(in-package :eightbol)

;;; Case-preserving display labels for UI (help text, error messages).
;;; Input matching is case-insensitive (string-equal); display preserves
;;; intended case.
;;;
;;; After changing +cpu-display-names+ or +supported-cpus+, rebuild
;;; bin/skyline-tool (make bin/skyline-tool) so the buildapp image matches;
;;; otherwise alexandria:define-constant may signal when sources are
;;; recompiled against an older embedded value.
(define-constant +cpu-display-names+
    '((:6502 . "6502")
      (:65c02 . "65c02")
      (:65c816 . "65c816")
      (:cp1610 . "cp1610")
      (:huc6280 . "HuC6280")
      (:rp2a03 . "RP2A03")
      (:z80 . "Z80")
      (:sm83 . "SM83")
      (:m6800 . "m6800")
      (:m68k . "m68k")
      (:i286 . "i286")
      (:arm7 . "ARM7")
      (:f8 . "F8"))
  :test 'equalp
  :documentation
  "Alist of CPU keyword to case-preserving directory / UI label string.")

;;; All supported CPU keywords, in canonical order (see +cpu-display-names+).
(define-constant +supported-cpus+
    (mapcar #'first +cpu-display-names+)
  :test 'equal
  :documentation
  "List of supported CPU keywords in canonical order; derived from +cpu-display-names+.")

(defvar *cpu* nil
  "Current target CPU keyword; bound during compile-to-assembly.")

(defun cpu-display-name (&optional (cpu *cpu*))
  "Return the case-preserving UI label for CPU."
  (rest (assoc cpu +cpu-display-names+)))

(defun cpu-directory-name (&optional (cpu *cpu*))
  "Return the canonical output directory component for a CPU keyword.
Uses display names: cp1610, 65c02, RP2A03, m68k, i286, Z80, etc."
  (cpu-display-name cpu))

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
component when @code{compile-eightbol-class} runs outside @code{compile-to-assembly}).

@table @asis
@item ROOT-DIRECTORY
Project root; merged with the relative @code{Source/Generated/Classes/…} path.
@item CPU
Optional target ISA keyword (e.g. @code{:6502}); passed by @code{compile-eightbol-class}
as @code{(first cpus)} so the copybook directory matches the first backend target.
@end table

@subsection Outputs
A one-element list of pathname designators."
  (let ((c (or cpu *cpu* :6502)))
    (list (make-pathname :defaults root-directory
		     :directory (list :relative "Source" "Generated"
				  "Classes" (cpu-directory-name c))))))

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
                                  (not (string= (namestring (pathname output-file)) "")))
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

(defun compile-eightbol-class (input-files
			 &key (cpus '(:6502))
			      copybook-paths
			      (root-directory (truename #p"."))
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
                              (default-copybook-paths root-directory (first cpus))))
        (*copybook-dependencies* ()))
    ;;  Phase 1: parse 
    (let (ast primary-input-file)
      (dolist (input-file input-files)
        (unless primary-input-file
          (setf primary-input-file input-file))
        (setf ast (with-open-file (stream input-file :direction :input)
                    (let* ((*source-file-pathname* (enough-namestring input-file))
                           (*current-token-location* (list :source-file *source-file-pathname*
                                                           :source-line nil
                                                           :source-sequence nil)))
                      (parse-eightbol stream)))))
      (unless (and (listp ast) (eq (first ast) :program))
        (error "EIGHTBOL: parse of ~a did not yield a Program node"
               (first input-files)))
      (setf ast (optimize-ast ast))
      (let ((class-id (ast-class-id ast)))
        ;;  Phase 2: write AST 
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
          (format t "~2&EIGHTBOL: wrote AST to ~a" (enough-namestring ast-path)))
        ;;  Phase 3: backends 
        ;; Step CPU from (REST CPU-LIST): (FIRST CPU-LIST) would repeat the first
        ;; CPU and skip the last (e.g. F8 never emitted in ALL-BACKENDS-ONE-FIXTURE).
        (do ((cpu-list cpus (rest cpu-list))
             (cpu (first cpus) (first (rest cpu-list)))
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
                (format t "~2&EIGHTBOL: wrote ~a assembly to ~a"
                        (cpu-display-name cpu) (enough-namestring asm-path)))
            #+()(error (e)
	        (format *error-output* "~2&EIGHTBOL: error compiling ~a for ~a: ~a"
		      class-id (cpu-display-name cpu) e)
	        (error e))))
        ;; ;;  Phase 4: dependency file for Make 
        ;; (write-copybook-deps (or primary-input-file (first input-files))
        ;;                      class-id cpus root-directory output-file
        ;;                      *copybook-dependencies*)
        ast))))

(defun compile-to-assembly-with-ast-passes (ast cpu output-stream)
  "Run optimize-ast (routine AST optimizations), optional VALIDATE-EIGHTBOL-PROGRAM, then compile.
Use when AST comes from parse only (e.g. unit tests). `compile-eightbol-class`
already calls optimize-ast before `compile-to-assembly`."
  (let ((opt (optimize-ast ast)))
    (validate-eightbol-program opt)
    (compile-to-assembly opt cpu output-stream)))

(defun parse-eightbol-string-for-codegen (string &optional (pathname "<String>"))
  "Parse STRING and apply optimize-ast so the result matches codegen input."
  (optimize-ast (parse-eightbol-string string pathname)))

(defun compile-eightbol-class-from-ast
    (ast
     &key (cpus +supported-cpus+)
          (root-directory (truename "."))
          output-file)
  "Re-compile from a previously-parsed (or loaded) AST plist.
Writes only assembly output (no AST write). Applies optimize-ast so the same
routine AST passes run as in `compile-eightbol-class`."
  (unless (and (listp ast) (eq (first ast) :program))
    (error "EIGHTBOL: not a :program AST node"))
  (setf ast (optimize-ast ast))
  (let ((class-id (ast-class-id ast)))
    (do ((cpu-list cpus (rest cpu-list))
         (cpu (first cpus) (first (rest cpu-list)))
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
          (format *error-output* "~2&EIGHTBOL: error compiling ~a for ~a: ~a"
                  class-id (cpu-display-name cpu) e)
          (error e))))))

(defun load-eightbol-ast (ast-file)
  "Read a previously-serialised AST from AST-FILE (.eightbol)."
  (with-open-file (stream ast-file :direction :input)
    (read-ast stream)))

(defvar *class-methods* (make-hash-table :test 'equalp))
(defvar *class-slots* (make-hash-table :test 'equalp))
(defvar *parent-classes* (make-hash-table :test 'equalp))

(defun method-class (class-id method-id)
  (let ((canon-class (pascal-case class-id))
        (canon-method (pascal-case method-id)))
    #+()(format *trace-output* "~&Method ~s Class ~s" canon-method canon-class)
    #+()(force-output *trace-output*)
    (if (equal "Destroy" canon-method)
        (return-from method-class "Basic-Object")
        (when (equal "BasicObject" canon-class)
	(error "~s is not a method of Basic-Object" method-id)))
    (unless (gethash canon-class *parent-classes*)
      (load-classes))
    (let ((consider-class canon-class))
      (loop
        #+()(format *trace-output* "~&~4TLooking for ~s in ~s" canon-method consider-class)
        #+()(force-output *trace-output*)
            (when (find canon-method (gethash consider-class *class-methods*)
		    :test #'string-equal)
	    (return-from method-class (header-case consider-class)))
            (setf consider-class (gethash consider-class *parent-classes*))
            (when (or (null consider-class) (equal consider-class "BasicObject"))
	    (error "~s is not a method of class ~s, nor its parent classes up to ~s"
		 (header-case method-id) (header-case canon-class) consider-class))))))

(defun slot-class (class-id slot-id)
  (let ((canon-class (pascal-case class-id))
        (canon-slot (pascal-case slot-id)))
    #+()(format *trace-output* "~&Slot ~s Class ~s" canon-slot canon-class)
    #+()(force-output *trace-output*)
    (if (equal "Destroy" canon-slot)
        (return-from slot-class "Basic-Object")
        (when (equal "BasicObject" canon-class)
	(error "~s is not a slot of Basic-Object" slot-id)))
    (unless (gethash canon-class *parent-classes*)
      (load-classes))
    (let ((consider-class canon-class))
      (loop
        #+()(format *trace-output* "~&~4TLooking for ~s in ~s" canon-slot consider-class)
        #+()(force-output *trace-output*)
            (when (find canon-slot (gethash consider-class *class-slots*)
		    :test #'string-equal)
	    (return-from slot-class (header-case consider-class)))
            (setf consider-class (gethash consider-class *parent-classes*))
            (when (or (null consider-class) (equal consider-class "BasicObject"))
	    (error "~s is not a slot of class ~s, nor its parent classes up to ~s"
		 (header-case slot-id) (header-case canon-class) consider-class))))))

(defun load-classes ()
  (when (plusp (hash-table-count *parent-classes*))
    (return-from load-classes))
  (with-input-from-file (classes.defs #p"Source/Classes/Classes.Defs")
    (loop with last-class = "BasicObject"
	for line = (read-line classes.defs nil nil)
	while line
	do (cond ((search "<" line)
		(destructuring-bind (child parent)
		    (mapcar (lambda (word)
			    (string-trim #(#\Space #\Tab) word))
			  (split-sequence #\< line))
		  (setf (gethash child *parent-classes*) parent
		        last-class child)))
	         ((and (< 2 (length line)) (char= #\# (char line 0)))
		(appendf (gethash last-class *class-methods* '())
		         (cons (subseq line 1) nil)))
	         ((and (< 2 (length line)) (char= #\. (char line 0)))
		(appendf (gethash last-class *class-slots* '())
		         (cons (subseq line 1 (position #\Space line)) nil)))))))
