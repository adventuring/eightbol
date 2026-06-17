;; basic-shell.lisp — Dartmouth BASIC interactive shell and compile driver
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defun basic-interactive-p ()
  "Return true when @code{*query-io*} is interactive (prompts allowed)."
  (interactive-stream-p *query-io*))

(defun read-class-name-designator (&optional (prompt "Class name"))
  "Read one line of input and return a trimmed class / file stem string.
When not interactive and EOF, signal @code{usage-error}. PROMPT is printed
to @code{*query-io*} before @code{read-line}.

@table @asis
@item PROMPT
Text shown before reading (no trailing newline added here).
@end table

@subsection Outputs
The trimmed string, or @code{NIL} on empty interactive input (caller may retry)."
  (when prompt
    (format *query-io* "~&~A: " prompt)
    (finish-output *query-io*))
  (multiple-value-bind (line eof) (read-line *query-io* nil nil)
    (when (and eof (not (basic-interactive-p)))
      (error 'usage-error
             :option "class name"
             :argument nil
             :message "Standard input is not a TTY; supply the class name on the command."))
    (unless line
      (return-from read-class-name-designator nil))
    (let ((s (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
      (if (zerop (length s))
          nil
          s))))

(defun basic-parse-quoted-name (token)
  "If TOKEN is @code{\"…\"}, return inner string; else return TOKEN trimmed."
  (let ((s (string-trim '(#\Space #\Tab) token)))
    (if (and (>= (length s) 2)
             (char= #\" (char s 0))
             (char= #\" (char s (1- (length s)))))
        (subseq s 1 (1- (length s)))
        s)))

(defun basic-classes-directory (&optional (root (truename ".")))
  "Return pathname for @code{Source/Classes/} under ROOT."
  (merge-pathnames (make-pathname :directory '(:relative "Source" "Classes"))
                   (uiop:ensure-directory-pathname root)))

(defun basic-path-for-class (class-stem &optional (root (truename ".")))
  "Return @code{Source/Classes/{CLASS-STEM}.bas} under ROOT (header-case stem)."
  (merge-pathnames
   (make-pathname :name (header-case class-stem) :type "bas")
   (basic-classes-directory root)))

(defun read-basic-file-text (path)
  "Read PATH as UTF-8 and return a string."
  (with-open-file (in path :direction :input :element-type 'character
                              :external-format :utf-8)
    (with-output-to-string (out)
      (loop for c = (read-char in nil nil)
            while c
            do (write-char c out)))))

(defun write-basic-file-text (path text)
  "Write TEXT to PATH as UTF-8."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string text out)))

(defun basic-workspace-lines-as-string (lines)
  "Format sorted LINES @code{(n . text)} as BASIC source lines."
  (with-output-to-string (out)
    (dolist (pair (stable-sort (copy-list lines) #'< :key #'car))
      (format out "~D ~A~%" (car pair) (cdr pair)))))

(defun basic-workspace-sync-text (workspace)
  "Set WORKSPACE @code{:text} from its @code{:lines} alist."
  (setf (getf workspace :text)
        (basic-workspace-lines-as-string (getf workspace :lines))))

(defun basic-listing-banner (program-name stream)
  "Print LIST banner lines to STREAM for PROGRAM-NAME."
  (format stream "---- LISTING BEGINS FOR PROGRAM \"~A\"~%" program-name))

(defun basic-listing-footer (stream)
  "Print LIST footer line to STREAM."
  (format stream "---- END OF LISTING~%"))

(defun basic-shell-list (workspace program-name stream)
  "Write LIST output for WORKSPACE plist to STREAM."
  (basic-listing-banner program-name stream)
  (basic-workspace-sync-text workspace)
  (write-string (getf workspace :text) stream)
  (unless (zerop (length (getf workspace :text)))
    (unless (char= #\Newline (char (getf workspace :text) (1- (length (getf workspace :text)))))
      (terpri stream)))
  (basic-listing-footer stream))

(defun basic-shell-renum (lines start inc)
  "Return new alist with line numbers renumbered from START by INC."
  (let ((n start)
        (sorted (stable-sort (copy-list lines) #'< :key #'car)))
    (mapcar (lambda (pair)
              (prog1 (cons n (cdr pair))
                (incf n inc)))
            sorted)))

(defun guess-project-copybook-directory (root)
  "Return a directory pathname under ROOT with generated class copybooks, or @code{NIL}.
Delegates to @code{project-copybook-paths} and returns the first directory whose
pathname contains @code{…/Generated/@var{machine}/Classes/} (machine not the
literal @code{Classes} segment used in the flat per-CPU layout)."
  (find-if
   (lambda (p)
     (let* ((dir (uiop:ensure-directory-pathname p))
            (path (pathname-directory dir)))
       (and (listp path)
            (let ((pos (position "Generated" path :test #'string-equal)))
              (and pos (< (1+ pos) (length path))
                   (not (string-equal "Classes" (nth (1+ pos) path))))))))
   (project-copybook-paths root :6502)))

(defun compile-basic-from-path (bas-path
                                &key (cpus '(:6502))
                                  output-file
                                  ast-output-file
                                  copybook-paths
                                  (root-directory (truename "."))
                                  (game-name *basic-default-game-name*))
  "Transpile @code{.bas} at BAS-PATH to temporary COBOL and run @code{compile-eightbol}.
CPUS, OUTPUT-FILE, AST-OUTPUT-FILE, COPYBOOK-PATHS, and ROOT-DIRECTORY follow
@code{compile-eightbol}. GAME-NAME selects the Globals COPY stem.
When COPYBOOK-PATHS is @code{NIL}, @code{project-copybook-paths} is used so
@code{COPY} matches Phantasia @file{.cob} sources (@code{Source/Generated/…/Classes/},
@code{Source/Classes}, per-CPU fallback).

@table @asis
@item BAS-Path
Pathname designator to UTF-8 BASIC source.
@end table

@subsection Outputs
Return value of @code{compile-eightbol}."
  (let* ((path (uiop:parse-native-namestring (namestring bas-path)))
         (text (read-basic-file-text path))
         (class-id (class-id-from-bas-pathname path))
         (cobol (transpile-basic-to-cobol-string class-id text :game-name game-name))
         (paths (or copybook-paths
                    (project-copybook-paths root-directory (first cpus)))))
    (uiop:with-temporary-file (:pathname tmp-cob :suffix "eightbol-basic-gen.cob")
      (with-open-file (out tmp-cob :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
        (write-string cobol out))
      (compile-eightbol (list tmp-cob)
                              :cpus cpus
                              :copybook-paths paths
                              :root-directory root-directory
                              :output-file (when output-file (pathname output-file))
                              :ast-output-file (when ast-output-file (pathname ast-output-file))))))

(defun basic-shell-catalog (root stream)
  "List @code{Source/Classes/*.bas} files under ROOT to STREAM."
  (let ((dir (basic-classes-directory root)))
    (when (probe-file dir)
      (dolist (p (sort (directory (merge-pathnames "*.bas" dir)) #'string<
                     :key (lambda (x) (string-downcase (namestring x)))))
        (format stream "~A~%" (pathname-name p))))))

(defun basic-shell-command-keyword (line)
  "Return uppercased command word from LINE, or @code{NIL} if line starts with a digit."
  (let ((s (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
    (when (zerop (length s))
      (return-from basic-shell-command-keyword :empty))
    (when (digit-char-p (char s 0))
      (return-from basic-shell-command-keyword nil))
    (let ((p (position #\Space s)))
      (string-upcase (if p (subseq s 0 p) s)))))

(defun basic-shell-command-rest (line)
  "Return argument substring after first word of LINE."
  (let ((s (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
    (let ((p (position #\Space s)))
      (if p
          (string-trim '(#\Space #\Tab) (subseq s (1+ p)))
          ""))))

(defun basic-merge-program-line (workspace line)
  "Insert or replace one numbered BASIC source LINE into WORKSPACE."
  (let ((parsed (parse-basic-source-lines line)))
    (dolist (p parsed)
      (setf (getf workspace :lines)
            (remove (car p) (getf workspace :lines) :key #'car :test #'=)))
    (setf (getf workspace :lines)
          (stable-sort (append (getf workspace :lines) parsed) #'< :key #'car))
    (setf (getf workspace :lines) (ensure-basic-line-numbers (getf workspace :lines)))
    (basic-workspace-sync-text workspace)))

(defun eightbol-basic (&key (root-directory (truename ".")))
  "Run the Dartmouth BASIC shell on @code{*query-io*} until @code{BYE}.
ROOT-DIRECTORY defaults to the current directory for @code{NEW}/@code{OLD}/
@code{SAVE} paths.

@subsection Behavior
Reads lines, dispatches shell commands, and merges numbered program lines into
the workspace."
  (let ((workspace (list :class-id nil :lines '() :text ""))
        (root root-directory))
    (format *query-io* "~&(eightbol-basic) Dartmouth BASIC shell. Type HELP.~%")
    (loop
      (when (basic-interactive-p)
        (format *query-io* "~&BASIC> ")
        (finish-output *query-io*))
      (let ((line (read-line *query-io* nil nil)))
        (when (null line)
          (return))
        (handler-case
            (let ((cmd (basic-shell-command-keyword line)))
              (cond
                ((eq cmd :empty)
                 nil)
                ((null cmd)
                 (basic-merge-program-line workspace line))
                ((string-equal cmd "BYE")
                 (format *query-io* "~&Goodbye.~%")
                 (return))
                ((string-equal cmd "HELP")
                 (format *query-io* "~&Commands: NEW [\"name\"], OLD [\"name\"], SAVE [\"name\"], LIST [\"name\"], RENUM [start[,inc]], RUN (compiles but does not execute), BYE, SCRATCH, CATALOG, RENAME [\"name\"], UNSAVE [\"name\"], HELP.~%"))
                ((string-equal cmd "SCRATCH")
                 (setf (getf workspace :lines) nil)
                 (basic-workspace-sync-text workspace)
                 (format *query-io* "~&Workspace cleared (filename unchanged).~%"))
                ((string-equal cmd "CATALOG")
                 (basic-shell-catalog root *query-io*))
((string-equal cmd "NEW")
                  (let ((rest (basic-shell-command-rest line)))
                    (cond
                      ((plusp (length rest))
                       (let ((name (header-case (basic-parse-quoted-name rest))))
                         (let ((p (basic-path-for-class name root)))
                           (if (probe-file p)
                               (error "Conflict: ~A already exists; use OLD to load it" p)
                               (progn
                                 (setf (getf workspace :lines) nil)
                                 (setf (getf workspace :class-id) name)
                                 (write-basic-file-text p "")
                                 (format *query-io* "~&New program ~A.bas~%" name)))))
                      (t
                       (setf (getf workspace :lines) nil
                             (getf workspace :class-id) nil)
                       (let ((name (or (read-class-name-designator "New program class name")
                                       (read-class-name-designator "New program class name (retry)"))))
                         (when name
                           (let ((p (basic-path-for-class (header-case name) root)))
                             (if (probe-file p)
                                 (error "Conflict: ~A already exists; use OLD to load it" p)
                                 (progn
                                   (setf (getf workspace :class-id) (header-case name))
                                   (write-basic-file-text p "")
                                   (format *query-io* "~&New program ~A.bas~%" (getf workspace :class-id))))))))))
                ((string-equal cmd "OLD")
                 (let* ((rest (basic-shell-command-rest line))
                        (name (or (and (plusp (length rest)) (header-case (basic-parse-quoted-name rest)))
                                  (read-class-name-designator "Program to load (class name)")
                                  (getf workspace :class-id))))
                   (when name
                     (setf (getf workspace :class-id) name)
                     (let ((p (basic-path-for-class name root)))
                       (if (probe-file p)
                           (progn
                             (setf (getf workspace :text) (read-basic-file-text p))
                             (setf (getf workspace :lines)
                                   (ensure-basic-line-numbers (parse-basic-source-lines (getf workspace :text))))
                             (basic-workspace-sync-text workspace)
                             (format *query-io* "~&Loaded ~A~%" p))
                           (format *query-io* "~&File not found: ~A~%" p))))))
                ((string-equal cmd "SAVE")
                 (let* ((rest (basic-shell-command-rest line))
                        (name (or (and (plusp (length rest)) (header-case (basic-parse-quoted-name rest)))
                                  (getf workspace :class-id)
                                  (read-class-name-designator "Save as class name"))))
                   (when name
                     (setf (getf workspace :class-id) name)
                     (basic-workspace-sync-text workspace)
                     (write-basic-file-text (basic-path-for-class name root) (getf workspace :text))
                     (format *query-io* "~&Saved ~A~%" (basic-path-for-class name root)))))
                ((string-equal cmd "LIST")
                 (let* ((rest (basic-shell-command-rest line))
                        (name (if (plusp (length rest))
                                  (header-case (basic-parse-quoted-name rest))
                                  (getf workspace :class-id))))
                   (if (and (plusp (length rest)) name)
                       (let ((p (basic-path-for-class name root)))
                         (if (probe-file p)
                             (let ((w (list :lines (ensure-basic-line-numbers (parse-basic-source-lines (read-basic-file-text p)))
                                            :text "")))
                               (basic-workspace-sync-text w)
                               (basic-shell-list w name *query-io*))
                             (format *query-io* "~&File not found: ~A~%" p)))
                       (progn
                         (unless name
                           (error "LIST: no current program; use OLD or NEW first"))
                         (basic-shell-list workspace name *query-io*)))))
                ((string-equal cmd "RENUM")
                 (let ((rest (basic-shell-command-rest line))
                       (start 100)
                       (inc 100))
                   (when (plusp (length rest))
                     (let ((parts (mapcar (lambda (x) (parse-integer (string-trim '(#\Space #\Tab) x) :junk-allowed nil))
                                          (split-sequence #\, rest :remove-empty-subseqs t))))
                       (when (first parts) (setf start (first parts)))
                       (when (second parts) (setf inc (second parts)))))
                   (setf (getf workspace :lines)
                         (basic-shell-renum (getf workspace :lines) start inc))
                   (basic-workspace-sync-text workspace)
                   (format *query-io* "~&Renumbered (~D step ~D).~%" start inc)))
                ((string-equal cmd "RENAME")
                 (let ((rest (basic-shell-command-rest line))
                       (new (or (and (plusp (length rest)) (header-case (basic-parse-quoted-name rest)))
                                (read-class-name-designator "New class name"))))
                   (when new
                     (setf (getf workspace :class-id) new)
                     (format *query-io* "~&Current name set to ~A~%" new))))
                ((string-equal cmd "UNSAVE")
                 (let* ((rest (basic-shell-command-rest line))
                        (name (or (and (plusp (length rest)) (header-case (basic-parse-quoted-name rest)))
                                  (getf workspace :class-id)
                                  (read-class-name-designator "Class name to delete"))))
                   (when name
                     (let ((p (basic-path-for-class name root)))
                       (when (probe-file p)
                         (delete-file p)
                         (format *query-io* "~&Deleted ~A~%" p))))))
                ((string-equal cmd "RUN")
                 (unless (getf workspace :class-id)
                   (error "RUN: set class name with NEW or OLD first"))
                 (basic-workspace-sync-text workspace)
                 (let ((paths (project-copybook-paths root (first +supported-cpus+))))
                   (uiop:with-temporary-file (:pathname tmp :suffix "eightbol-basic-run.cob")
                     (with-open-file (out tmp :direction :output :if-exists :supersede
                                             :external-format :utf-8)
                       (write-string (transpile-basic-to-cobol-string (getf workspace :class-id)
                                                                      (getf workspace :text))
                                     out))
                     (compile-eightbol (list tmp)
                                             :cpus +supported-cpus+
                                             :root-directory root
                                             :copybook-paths paths)))
                 (format *query-io* "~&RUN completed for all CPUs.~%"))
                (t
                 (basic-merge-program-line workspace line))))
          (error (e)
            (format *query-io* "~&Error: ~A~%" e)))))))
