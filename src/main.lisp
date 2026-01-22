;; src/main.lisp
(in-package :eightbol)

(defun parse-arguments (args)
  "Parse command line arguments.
Recognised options:
  --version          print version and exit
  --help             print usage and exit
  -I <path>          add copybook include path
  -o <file>          write first-CPU assembly output to <file>
                     (default: Source/Generated/Classes/{cpu}/{class}.s)
  -m <cpu>           target CPU: <cpu> or 'all' for all backends (default: all)
                     CPUs: 6502, 65c02, 65c816, cp1610, HuC6280, RP2A03, Z80, SM83, m68k, i286, ARM7
  <file>             EIGHTBOL source file to compile"
  (let (last-wants-arg result)
    (dolist (arg args)
      (cond
        ((string= arg "--version") (push :version result) (push t result))
        ((string= arg "--help")    (push :help result)    (push t result))
        ((string= arg "-I")        (setf last-wants-arg :include-path))
        ((string= arg "-o")        (setf last-wants-arg :object-file))
        ((string= arg "-m")        (setf last-wants-arg :cpu))
        ((and (string-prefix-p "-" arg) (not (string= arg "-")))
         (error "Unknown option: ~a" arg))
        (last-wants-arg
         (cond
           ((eq last-wants-arg :cpu)
            ;; convert cpu name string to keyword or :all; only one -m allowed
            (when (getf result :cpu)
              (error "eightbol: -m may only be specified once"))
            (if (string-equal arg "all")
                (progn (push :cpu result) (push :all result))
                (let ((kw (find arg *supported-cpus*
                                :key #'cpu-display-name
                                :test #'string-equal)))
                  (unless kw
                    (error "Unknown CPU ~s; known: ~{~a~^, ~}, all"
                           arg (mapcar #'cpu-display-name *supported-cpus*)))
                  (push :cpu result)
                  (push kw result))))
           (t
            (push last-wants-arg result)
            (push arg result)))
         (setf last-wants-arg nil))
        (t
         (push :input-file result)
         (push arg result))))
    (when last-wants-arg
      (error "Dangling option flag at end of arguments: missing ~a" last-wants-arg))
    (nreverse result)))

(defparameter +format-time-date+ '((:day 2) #\space :short-month #\space (:year 4))
  "Format list for local-time: \"DD Mon YYYY\" (e.g. 10 Feb 2026).")

(defun format-date (&optional (universal-time (get-universal-time)))
  "Format UNIVERSAL-TIME as \"DD Mon YYYY\" (e.g. 10 Feb 2026)."
  (local-time:format-timestring nil
                                (local-time:universal-to-timestamp universal-time)
                                :format +format-time-date+))

(defun main (argv)
  "Main entry point for the EIGHTBOL compiler.
ARGV is the full POSIX argv list (argv[0] is the program name)."
  (handler-case
      (let ((options (parse-arguments (rest argv))))  ; skip argv[0]
        (cond
          ((getf options :version)
           (format t  "~2&EIGHTBOL Compiler version 0.2.0")
           (fresh-line)
           (sb-ext:exit :code 0))
          ((getf options :help)
           (format t "~&Usage: eightbol [OPTIONS] input-file.cob~%~
Options:~%~
  -I <path>    Add <path> to copybook include search path~%~
  -o <file>    Write first CPU output to <file>~%~
               (default: Source/Generated/Classes/{cpu}/{class}.s)~%~
  -m <cpu>     Target CPU: <cpu> or 'all' (default: all)~%~
               Valid: ~{~a~^, ~}, all~%~
  --version    Print compiler version~%~
  --help       Print this message"
                (mapcar #'cpu-display-name *supported-cpus*))
           (fresh-line)
           (sb-ext:exit :code 0))
          ((getf options :input-file)
           (let* ((input-files
                    (loop for (k v) on options by #'cddr
                          when (eq k :input-file) collect v))
                  (input-file (first input-files))
                  (include-paths
                    (loop for (k v) on options by #'cddr
                          when (eq k :include-path) collect v))
                  (selected-cpus
                    (let ((cpu-opt (getf options :cpu)))
                      (cond ((eq cpu-opt :all) *supported-cpus*)
                            (cpu-opt (list cpu-opt))
                            (t *supported-cpus*)))))
             ;; Validate exactly one input file
             (cond ((null input-files)
                    (format *error-output* "~2&eightbol: no input file specified. Try 'eightbol --help' for usage.")
                    (fresh-line)
                    (sb-ext:exit :code 1))
                   ((rest input-files)
                    (format *error-output* "~2&eightbol: multiple input files not supported (~{~a~^, ~}); specify exactly one."
                            input-files)
                    (fresh-line)
                    (sb-ext:exit :code 1)))
             ;; Validate input file extension
             (unless (or (string-suffix-p ".cob" input-file)
                         (string-suffix-p ".COB" input-file))
               (format *error-output* "~2&eightbol: input file ~s should have .cob extension" input-file)
               (fresh-line)
               (sb-ext:exit :code 1))
             ;; Validate input file exists
             (unless (probe-file input-file)
               (format *error-output* "~2&eightbol: cannot find input file ~a" input-file)
               (fresh-line)
               (sb-ext:exit :code 1))
             ;; Validate -I paths exist and are directories
             (dolist (p include-paths)
               (unless (uiop:directory-exists-p p)
                 (format *error-output* "~2&eightbol: include path ~s does not exist or is not a directory" p)
                 (fresh-line)
                 (sb-ext:exit :code 1)))
             ;; Validate -o parent directory when specified
             (let ((out (getf options :object-file)))
               (when (and out (not (string= out "")))
                 (let ((parent (uiop:pathname-directory-pathname (pathname out))))
                   (when (and parent (not (uiop:directory-exists-p parent)))
                     (ensure-directories-exist parent)))))
             (format t  "~2&EIGHTBOL: compiling ~a" input-file)
             (let ((root (truename "."))
                   (copybook-paths (append (or include-paths nil)
                                          (default-copybook-paths (truename ".")))))
               (compile-eightbol-class input-file
                                       :cpus selected-cpus
                                       :copybook-paths copybook-paths
                                       :root-directory root
                                       :output-file (getf options :object-file)))
             (fresh-line)
             (sb-ext:exit :code 0)))
          (t
           (format *error-output*
                    "~2&eightbol: no input file specified. ~
Try 'eightbol --help' for usage information.")
           (fresh-line)
           (sb-ext:exit :code 1))))
    (source-error (e)
      ;; GCC-style: file:line: error: message
      (format *error-output*  "~2&~@[~a:~]~@[~a:~] error: [~a] ~a"
              (eightbol-error-file e)
              (eightbol-error-line e)
              (eightbol-error-sequence e)
              (eightbol-error-message e))
      (fresh-line)
      (sb-ext:exit :code 1))
    (copybook-not-found (e)
      (format *error-output* "~2&error: ~a" e)
      (fresh-line)
      (sb-ext:exit :code 1))
    (error (e)
      (format *error-output*  "~2&eightbol: ~a" e)
      (fresh-line)
      (sb-ext:exit :code 1))))
