;; src/main.lisp
(in-package :eightbol)

(defun extract-basic-cli (args)
  "Strip @code{--basic} and optional @code{.bas} path from ARGS (argv without program).
Return @code{(values BASIC-P BASIC-FILE REMAINING-ARGS)}. When the first token after
@code{--basic} does not start with @code{#\\-}, it is consumed as BASIC-FILE.

@table @asis
@item ARGS
List of strings (e.g. from @code{uiop:command-line-arguments}).
@end table

@subsection Outputs
@code{BASIC-P} is true when @code{--basic} was present; @code{BASIC-FILE} is the optional
path token; @code{REMAINING-ARGS} is suitable for @code{parse-arguments}."
  (let ((pos (position "--basic" args :test #'string=)))
    (if (null pos)
        (values nil nil args)
        (let* ((before (subseq args 0 pos))
               (after (subseq args (1+ pos)))
               (file (when (and after (stringp (first after)))
                       (let ((x (first after)))
                         (when (and (plusp (length x))
                                    (not (char= #\- (char x 0))))
                           x))))
               (rest (append before (if file (rest after) after))))
          (values t file rest)))))

(defun parse-arguments (args)
  "Parse command line arguments."
  (let ((argv
	 (remove
	  nil
	  (flatten
	   (loop for arg in args
                 with last-wants-arg-p = nil
                 collect (cond
                           ((string= arg "--version")
                            '(:version t))
                           ((string= arg "--help")
                            '(:help t))
                           ((string= arg "-I")
                            (setf last-wants-arg-p :include-path)
                            nil)
                           ((string= arg "-m")
                            (setf last-wants-arg-p :machine)
                            nil)
                           ((string= arg "-o")
                            (setf last-wants-arg-p :object-file)
                            nil)
                           ((equal #\- (char arg 0))
			    (error 'unknown-option-error :option arg))
                           (t (if last-wants-arg-p
                                  (prog1
                                      (list last-wants-arg-p arg)
                                    (setf last-wants-arg-p nil))
                                  (list :input-file arg)))))))))
    (when (keywordp (lastcar argv))
      (error "Looks like a dangling option ends arguments: missing ~a"
	     (lastcar argv)))
    argv))


(defun format-date (&optional (universal-time (get-universal-time)))
  (local-time:format-timestring nil
				(local-time:universal-to-timestamp universal-time)
				:format '(:year #\- (:month 2) #\- (:day 2))))

(defun main (args)
  "Run the EIGHTBOL CLI from ARGS (argv including program name).

@table @asis
@item ARGS
@code{(uiop:command-line-arguments)}-style list; @code{--basic} and an optional
following @file{.bas} path are stripped before option parsing.
@end table

@subsection Behavior
Compiles @file{.cob} or @file{.bas}, prints @code{--help}, or starts the BASIC
shell when @code{--basic} is given without a file on an interactive stream.

Buildapp passes argv as a single argument; use @code{uiop:command-line-arguments}
when available."
  (multiple-value-bind (basic-p basic-file argv-rest)
      (extract-basic-cli (rest args))
    (let ((options (parse-arguments argv-rest))
          (start-time (get-universal-time)))
      (cond
        ((getf options :version)
         (format t "EIGHTBOL Compiler version 0.6~%"))
        ((getf options :help)
         (let ((cpus (append (mapcar #'cdr +cpu-display-names+)
                             (list "all"))))
           (format t "Usage: eightbol [OPTIONS] input-file.cob
       eightbol --basic [file.bas] [OPTIONS]
  Options:
	-I <path>    Add <path> to include path for copybooks
	-m <cpu>     Target CPU: ~{~a~^, ~} (or \"all\")
	-o <file>    Output to <file>
	--basic      Dartmouth BASIC: with file.bas, transpile then compile; without file, start REPL on a TTY
	--version    Print the version of the compiler
	--help       Print this help message"
                   cpus)))
        (basic-p
         (let* ((copybook-paths
                  (loop for (key value) on options by #'cddr
                        when (and (eql key :include-path)
                                  (or (stringp value) (pathnamep value)))
                          collect (uiop:ensure-directory-pathname
                                   (merge-pathnames (pathname value) (truename ".")))))
                (cpu-opt (when (getf options :machine)
                           (parse-cli-cpu-arg (getf options :machine))))
                (cpus (cond ((null cpu-opt) (list :6502))
                            ((eq cpu-opt :all) +supported-cpus+)
                            (t (list cpu-opt))))
                (output-file (getf options :object-file)))
           (cond
             (basic-file
              (unless (probe-file basic-file)
                (error 'input-file-not-found :path basic-file))
              (format t "~&Compiling BASIC ~a to ~a" basic-file
                      (or output-file "per-CPU outputs"))
              (compile-basic-from-path basic-file
                                        :cpus cpus
                                        :copybook-paths (when copybook-paths copybook-paths)
                                        :output-file (when output-file (pathname output-file))
                                        :root-directory (truename "."))
              (fresh-line)
              (format t " … completed in ~[less than a second~:;~~~:*~:d second~:p~]."
                      (- (get-universal-time) start-time))
              (terpri)
              (finish-output))
             ((interactive-stream-p *query-io*)
              (eightbol-basic :root-directory (truename ".")))
             (t
              (error 'usage-error
                     :option "--basic"
                     :message
                     "No BASIC file after --basic and standard input is not interactive; pass a .bas path (e.g. eightbol --basic Source/Classes/Foo.bas -m 6502 -o …).")))))
        ((getf options :input-file)
         (let* ((input-file (getf options :input-file))
                (output-file (getf options :object-file))
                (cpu-opt (when (getf options :machine)
                           (parse-cli-cpu-arg (getf options :machine))))
                (cpus (cond ((null cpu-opt) (list :6502))
                            ((eq cpu-opt :all) +supported-cpus+)
                            (t (list cpu-opt))))
                (copybook-paths (loop for (key value) on options by #'cddr
                                      when (and (eql key :include-path)
                                                (or (stringp value) (pathnamep value)))
                                        collect (uiop:ensure-directory-pathname
                                                 (merge-pathnames (pathname value) (truename "."))))))
           (unless (probe-file input-file)
             (error 'input-file-not-found :path input-file))
           (format t "~&Compiling ~a to ~a" input-file
                   (or output-file "standard output"))
           (compile-eightbol (list input-file)
                                   :cpus cpus
                                   :copybook-paths (when copybook-paths copybook-paths)
                                   :output-file (when output-file (pathname output-file))
                                   :root-directory (truename "."))
           (fresh-line)
           (format t " … completed in ~[less than a second~:;~~~:*~:d second~:p~]."
                   (- (get-universal-time) start-time))
           (terpri)
           (finish-output)))
        (t (error 'usage-error
	          :option "input file"
	          :argument nil
	          :message
	          (format nil
		        "No input file specified. ~
Use --help for usage information.
Processed: ~s" options)))))))
