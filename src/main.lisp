;; src/main.lisp
(in-package :eightbol)

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
  "Main entry point for the compiler.
Buildapp passes argv as single argument; use uiop:command-line-arguments when available."
  (let ((options (parse-arguments (rest args)))
        (start-time (get-universal-time)))
    (cond
      ((getf options :version)
       (format t "EIGHTBOL Compiler version 0.3~%"))
      ((getf options :help)
       (let ((cpus (append (mapcar #'cdr +cpu-display-names+)
                           (list "all"))))
         (format t "Usage: eightbol [OPTIONS] input-file.cob
  Options:
	-I <path>    Add <path> to include path for copybooks
	-m <cpu>     Target CPU: ~{~a~^, ~}
	-o <file>    Output to <file>
	--version    Print the version of the compiler
	--help       Print this help message"
                 cpus)))
      ((getf options :input-file)
       (let* ((input-file (getf options :input-file))
              (output-file (getf options :object-file))
              (cpu-opt (getf options :cpu))
              (cpus (cond ((null cpu-opt) (list :6502))
                          ((eq cpu-opt :all) +supported-cpus+)
                          (t (list cpu-opt))))
              (copybook-paths (loop for (key value) on options by #'cddr
                                    when (and (eql key :include-path)
                                              (or (stringp value) (pathnamep value)))
                                    collect (uiop:ensure-directory-pathname
                                             (merge-pathnames (pathname value) (truename "."))))))
         (unless (probe-file input-file)
           (error "Can't find input file ~a" input-file))
         (format t "~&Compiling ~a to ~a" input-file (or output-file "standard output"))
         (compile-eightbol-class (list input-file)
                                 :cpus cpus
                                 :copybook-paths (when copybook-paths copybook-paths)
                                 :output-file (when output-file (pathname output-file))
                                 :root-directory (truename "."))))
      (t (error 'usage-error
		:option "input file"
		:argument nil
		:message
		(format nil
			"No input file specified. ~
Use --help for usage information.
Processed: ~s" options))))))
