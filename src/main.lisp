;; src/main.lisp — Command-line interface for EIGHTBOL compiler
;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defparameter *eightbol-version*
  (asdf:component-version (asdf:find-system :eightbol)))

;; We use unix-opts for command-line parsing (must match print-usage)
(defmacro define-eightbol-opts ()
  `(unix-opts:define-opts
     (:name :help
      :description "Print usage information"
      :short #\h
      :long "help")
     (:name :version
      :description "Print version"
      :short #\V
      :long "version")
     (:name :basic
      :description "Dartmouth BASIC: start REPL on a TTY"
      :long "basic")
     (:name :lang
      :description "Language for next input file: bas/basic, cob/cobol, f/fortran, ls/lingo, lua, m/objective-c, p/pascal, st/smalltalk"
      :short #\l
      :long "lang"
      :arg-parser #'identity)
     (:name :machine
      :description "Target CPU: <name> or \"all\""
      :short #\m
      :long "machine"
      :arg-parser #'identity)
     (:name :output-file
      :description "Output to file"
      :short #\o
      :long "output"
      :arg-parser #'identity)
     (:name :include-path
      :description "Include directory"
      :short #\I
      :long "include"
      :arg-parser #'identity)))

(defun parse-arguments (args)
  "Parse command line arguments using unix-opts.
Returns a plist of parsed options.

@table @asis
@item :input-file
Pathname string(s) for input COBOL source file(s).
@item :machine
String value of -m option (CPU name or \"all\").
@item :output-file
String value of -o option (output pathname).
@item :include-path
String value of -I option (copybook include directory).
@item :basic
Boolean, true when --basic flag is present.
@item :lang
Keyword value of -l option (language override).
@item :root-directory
Root directory for output paths.
@end table"
  (multiple-value-bind (options free-args)
      (unix-opts:get-opts args)
    (let ((result (copy-list options)))
      (when free-args
        (setf (getf result :input-file)
              (if (= (length free-args) 1)
                  (first free-args)
                  free-args)))
      result)))

(defun expand-language-alias (lang)
  "Expand a short language alias to a keyword."
  (let ((ext (string-downcase lang)))
    (cond
      ((member ext '("agi" "scr") :test #'string=) :agi)
      ((member ext '("bas" "basic") :test #'string=) :basic)
      ((member ext '("cob" "cobol" "cbl") :test #'string=) :cobol)
      ((member ext '("f" "fortran" "for" "for77") :test #'string=) :fortran)
      ((member ext '("ls" "lingo" "stx") :test #'string=) :lingo)
      ((string= ext "lua") :lua)
      ((member ext '("m" "objective-c") :test #'string=) :objective-c)
      ((member ext '("p" "pascal" "pas") :test #'string=) :pascal)
      ((string= ext "st") :smalltalk)
      ((member ext '("bms") :test #'string=) :burgermistress)
      ((member ext '("mdl") :test #'string=) :muddle)
      ((member ext '("sc") :test #'string=) :sci)
      ((member ext '("scc" "scumm") :test #'string=) :scumm)
      ((member ext '("zil") :test #'string=) :zil)
      (t nil))))

(defun language-from-extension (pathname)
  "Guess language from file extension."
  (expand-language-alias (pathname-type pathname)))

(defun get-cpus (machine-arg)
  "Get the list of CPUs from the --machine argument."
  (cond
    ((null machine-arg) +supported-cpus+)
    ((string= machine-arg "all") +supported-cpus+)
    (t (list (parse-cli-cpu-arg machine-arg)))))

(defun dispatch-language (input-file options)
  "Dispatch compilation based on language derived from extension or -l flag."
  (let* ((explicit-lang (getf options :lang))
         (lang (or (and explicit-lang (expand-language-alias explicit-lang))
                   (language-from-extension input-file))))
     (unless lang
       (error "Cannot determine language for file ~a. Use -l <lang> (agi, bas, bms, cob, f, ls, lua, m, mdl, p, sc, scc, st, zil)." input-file))
    (let* ((output-file (getf options :output-file))
           (cpus (get-cpus (getf options :machine)))
           (include-paths (loop for (key value) on options by #'cddr
                                when (eql key :include-path)
                                collect (uiop:ensure-directory-pathname
                                          (merge-pathnames (pathname value)
                                                           (truename "."))))))
      (case lang
        (:agi
         (compile-agi-from-path input-file :cpus cpus :output-file output-file))
        (:basic
         (compile-basic-from-path input-file :cpus cpus))
        (:cobol
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :copybook-paths (or include-paths
                                               (project-copybook-paths (truename ".")))
                           :output-file (when output-file (pathname output-file))))
        (:fortran
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :output-file (when output-file (pathname output-file))))
        (:lingo
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :output-file (when output-file (pathname output-file))))
        (:lua
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :output-file (when output-file (pathname output-file))))
        (:objective-c
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :output-file (when output-file (pathname output-file))))
        (:pascal
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :output-file (when output-file (pathname output-file))))
        (:smalltalk
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :output-file (when output-file (pathname output-file))))
        (:burgermistress
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :output-file (when output-file (pathname output-file))))
        (:muddle
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :output-file (when output-file (pathname output-file))))
         (:sci
          (compile-sci-from-path input-file :cpus cpus :output-file output-file))
         (:scumm
          (compile-scumm-from-path input-file :cpus cpus :output-file output-file))
        (:zil
         (compile-eightbol (list input-file)
                           :cpus cpus
                           :output-file (when output-file (pathname output-file))))
        (otherwise
         (error "Unsupported language ~a for file ~a" lang input-file))))))

(defun main (args)
  "Main entry point for the EIGHTBOL compiler. ARGS is the argv list
including the program name (as passed by buildapp)."
  (define-eightbol-opts)
  (let* ((options (parse-arguments (rest args)))
         (raw-free (getf options :input-file))
         (free-args (cond ((null raw-free) nil)
                          ((listp raw-free) raw-free)
                          (t (list raw-free)))))
    (when (getf options :help)
      (unix-opts:describe
       :usage-of "eightbol"
       :args "[OPTIONS] input-file"
       :prefix "Usage: eightbol [OPTIONS] input-file"
       :suffix "Options are parsed with unix-opts library.")
      (finish-output)
      (return-from main 0))
    (when (getf options :version)
      (format t "Eightbol Compiler Version ~a~%" *eightbol-version*)
      (finish-output)
      (return-from main 0))
    (when (getf options :basic)
      (if (null free-args)
          (progn (eightbol-basic)
                 (return-from main 0))
          (progn
            (format t "Error: --basic cannot be used with input file~%")
            (return-from main 1))))
    (cond
      ((null free-args)
       (format t "Error: No input file specified.~%")
       (unix-opts:describe
        :usage-of "eightbol"
        :args "[OPTIONS] input-file")
       1)
      ((> (length free-args) 1)
       (format t "Error: Multiple input files specified: ~{~a~^ ~}~%" free-args)
       1)
      (t
       (let ((input-file (first free-args)))
         (unless (probe-file input-file)
           (error "Input file does not exist: ~a" input-file))
         (format t "Compiling ~a~%" input-file)
         (let ((result (dispatch-language input-file options)))
           (format t "~&Compilation complete.~%")
           (if result 0 1)))))))