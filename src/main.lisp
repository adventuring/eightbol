;; src/main.lisp
(in-package :eightbol)

(defun parse-arguments (args)
  "Parse command line arguments.
Recognised options:
 --version print version and exit
 --help print usage and exit
 -I <path> add copybook include path
 -o <file> write first-CPU assembly output to <file>
 (default: Source/Generated/Classes/{cpu}/{class}.s)
 -m <cpu> target CPU: <cpu> or 'all' for all backends
 CPUs: 6502, 65c02, 65c816, cp1610, HuC6280, RP2A03, Z80, SM83, m68k, i286, ARM7
 <file> EIGHTBOL source file to compile"
  (let (last-wants-arg result)
    (dolist (arg args)
      (cond
        ((string= arg "--version") (push :version result) (push t result))
        ((string= arg "--help") (push :help result) (push t result))
        ((string= arg "-I") (setf last-wants-arg :include-path))
        ((string= arg "-o") (setf last-wants-arg :object-file))
        ((string= arg "-m") (setf last-wants-arg :cpu))
        ((and (string-prefix-p "-" arg) (not (string= arg "-")))
         (error "Unknown option: ~a" arg))
        (last-wants-arg
         (cond
           ((eq last-wants-arg :cpu)
            ;; convert cpu name string to keyword or :all; only one -m allowed
            (if (string-equal arg "all")
                (setf (getf result :cpu) :all)
                (let ((kw (find arg +supported-cpus+
                                :key #'cpu-display-name
                                :test #'string-equal)))
                  (unless kw
                    (error "Unknown CPU ~s; known: ~{~a~^, ~}, all"
                           arg (mapcar #'cpu-display-name +supported-cpus+)))
                  (appendf (getf result :cpu) (list kw)))))
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

(define-constant +format-time-date+
    '((:day 2) #\space :short-month #\space (:year 4))
  :documentation "Format list for local-time: \"DD Mon YYYY\" (e.g. 10 Feb 2026)."
  :test 'equalp)

(defun format-date (&optional (universal-time (get-universal-time)))
  "Format UNIVERSAL-TIME as \"DD Mon YYYY\" (e.g. 10 Feb 2026)."
  (local-time:format-timestring nil
                                (local-time:universal-to-timestamp universal-time)
                                :format +format-time-date+))

(defun main (argv)
  "Main entry point for the EIGHTBOL compiler.
ARGV is the full POSIX argv list (argv[0] is the program name)."
  (handler-case
      (let ((options (parse-arguments (rest argv)))) ; skip argv[0]
        (cond
          ((getf options :version)
           (format t "~2&EIGHTBOL Compiler version 0.3~%"))
          ((getf options :help)
           (format t "~&Usage: eightbol [OPTIONS] input-file.cob
Options:
~2t-I <path>~20tAdd <path> to copybook include search path
~2t-o <file>~20tWrite first CPU output to <file>
~12t(default: Source/Generated/Classes/{cpu}/{class}.s)
~2t-m <cpu>~20tTarget CPU: <cpu> or 'all' (default: all)
~12tValid: ~{~a~^, ~}, all
~2t--version~20tPrint compiler version
~2t--help~20tPrint this message~%"
                   (mapcar #'cpu-display-name +supported-cpus+)))
          ((getf options :input-file)
           (let* ((input-files
                    (loop for (k v) on options by #'cddr
                          when (eq k :input-file) collect v))
                  (input-file (first input-files))
                  (include-paths
                    (loop for (k v) on options by #'cddr
                          when (eq k :include-path) collect v))
                  (selected-cpus
                    (loop for (k v) on options by #'cddr
                          when (eq k :cpu) collect v)))
             (unless input-files
               (error "no input file specified. Try 'eightbol --help' for usage."))
             (dolist (p include-paths)
               (unless (uiop:directory-exists-p p)
                 (error "include path ~s does not exist or is not a directory" p)))
             (let ((out (getf options :object-file)))
               (when (and out (not (string= out "")))
                 (let ((parent (uiop:pathname-directory-pathname (pathname out))))
                   (ensure-directories-exist parent))))
             (format t "~2&EIGHTBOL: compiling ~{~a~^, ~}…" input-files)
             (let ((root (truename "."))
                   (copybook-paths (append (or include-paths nil)
                                           (default-copybook-paths root))))
               (compile-eightbol-class input-files
                                       :cpus selected-cpus
                                       :copybook-paths copybook-paths
                                       :root-directory root
                                       :output-file (getf options :object-file)))
             (fresh-line)))
          (t
           (error
            "no input file specified. ~
Try 'eightbol --help' for usage information."))))
    (source-error (e)
                                        ;y; GCC-style: file:line: error: message
      (format *error-output* "~2&~@[~a:~]~@[~a:~] error: [~a] ~a~%"
              (eightbol-error-file e)
              (eightbol-error-line e)
              (eightbol-error-sequence e)
              (eightbol-error-message e))
      2)
    (copybook-not-found (e)
      (format *error-output* "~2&error: ~a~%" e)
      3)
    (error (e)
      (format *error-output* "~2&eightbol: ~a~%" e)
      1)))
