;; tests/compile-regression-tests.lisp — Regression tests ensuring custom test .cob files compile
;;
;; Uses custom fixture files from tests/fixtures/ (no project class files).
;; Fixtures use COPY for slots; copybooks are generated from tests/fixtures/Classes.Defs
;; via skyline-tool:make-eightbol-copybooks. Compiles to a temporary directory and
;; asserts output exists, is non-empty, and contains expected assembly patterns.
;;
;; To run: (asdf:test-system :eightbol)
;;        — or — (fiveam:run! :compile-regression)

(in-package :eightbol/test)

(defparameter *compile-regression-cpu* :6502
  "Target CPU for regression compilation.")

(defparameter *compile-regression-machine* 7800
  "Machine/port for copybook generation (matches make-eightbol-copybooks output path).")

;;;; Fixture discovery

(defun compile-regression-fixtures-directory ()
  "Return pathname for tests/fixtures/ relative to eightbol system."
  (merge-pathnames
   (make-pathname :directory '(:relative "tests" "fixtures"))
   (asdf:system-source-directory :eightbol)))

(defun compile-regression-classes-defs-path ()
  "Return pathname for tests/fixtures/Classes.Defs."
  (merge-pathnames
   (make-pathname :name "Classes" :type "Defs")
   (compile-regression-fixtures-directory)))

(defun compile-regression-fixture-cob-files ()
  "Return list of .cob pathnames in tests/fixtures/."
  (let ((dir (compile-regression-fixtures-directory)))
    (when (probe-file dir)
      (sort (directory (merge-pathnames
                        (make-pathname :name :wild :type "cob")
                        dir))
            #'string<
            :key #'namestring))))

;;;; Copybook generation from Classes.Defs

(defun compile-regression-generate-copybooks (tmp-dir)
  "Generate *-Slots.cpy from tests/fixtures/Classes.Defs into TMP-DIR.
Uses skyline-tool:make-eightbol-copybooks with *machine* bound.
Output: TMP-DIR/Source/Generated/7800/Classes/*.cpy"
  (let ((classes-defs (compile-regression-classes-defs-path))
        (skyline-tool::*machine* *compile-regression-machine*))
    (unless (probe-file classes-defs)
      (error "Classes.Defs not found: ~a" (namestring classes-defs)))
    (uiop:call-with-current-directory tmp-dir
      (lambda ()
        (skyline-tool::make-eightbol-copybooks classes-defs)))))

(defun compile-regression-copybook-paths (tmp-dir)
  "Return copybook search paths for generated *-Slots.cpy."
  (list (uiop:ensure-directory-pathname
         (merge-pathnames
          (make-pathname :directory (list :relative "Source" "Generated"
                                          (format nil "~a" *compile-regression-machine*)
                                          "Classes"))
          tmp-dir))))

;;;; Test suite

(fiveam:def-suite :compile-regression
  :description "Regression tests: custom test .cob fixtures compile successfully")
(in-suite :compile-regression)

(test compile-regression/fixtures-exist
  "Fixture directory, Classes.Defs, and at least one .cob file exist."
  (let ((dir (compile-regression-fixtures-directory))
        (classes-defs (compile-regression-classes-defs-path))
        (cobs (compile-regression-fixture-cob-files)))
    (is (probe-file dir) "tests/fixtures/ must exist")
    (is (probe-file classes-defs) "tests/fixtures/Classes.Defs must exist")
    (is-true cobs "At least one .cob fixture must exist in tests/fixtures/")))

(defun compile-regression-output-patterns (cpu)
  "Return list of substrings that indicate valid assembly for CPU.
6502: .block or .proc; cp1610: PROC; Z80: ret; m68k: rts; all: Method."
  (let* ((cpu-str (string (eightbol::cpu-display-name cpu)))
         (cpu-patterns
           (cond
             ((string-equal cpu-str "6502") '(".block" ".proc"))
             ((string-equal cpu-str "65c02") '(".block" ".proc"))
             ((string-equal cpu-str "65c816") '(".block" ".proc"))
             ((string-equal cpu-str "HuC6280") '(".block" ".proc"))
             ((string-equal cpu-str "RP2A03") '(".block" ".proc"))
             ((string-equal cpu-str "cp1610") '("PROC" "ENDP"))
             ((string-equal cpu-str "Z80") '("ret"))
             ((string-equal cpu-str "m68k") '("rts" ".text"))
             (t nil)))
         (fallback '(".block" ".proc" "ret" "rts")))
    (append (list "Method")
            (or cpu-patterns fallback))))

(test compile-regression/all-fixtures-compile
  "Each custom .cob fixture compiles to non-empty assembly with expected patterns.
Generates copybooks from tests/fixtures/Classes.Defs before compiling."
  (let ((cob-files (compile-regression-fixture-cob-files)))
    (when cob-files
      (let ((tmp (uiop:ensure-directory-pathname
                  (merge-pathnames
                   (make-pathname :directory
                                  `(:relative "eightbol-compile-regression-"
                                    ,(format nil "~a" (get-internal-real-time))))
                   (uiop:temporary-directory)))))
        (ensure-directories-exist tmp)
        (compile-regression-generate-copybooks tmp)
        (let ((copybook-paths (compile-regression-copybook-paths tmp)))
          (dolist (cob cob-files)
            (let* ((class-id (pathname-name cob))
                   (ast (compile-eightbol-class (list cob)
                                                :cpus (list *compile-regression-cpu*)
                                                :copybook-paths copybook-paths
                                                :root-directory tmp))
                   (output-path (merge-pathnames
                                 (make-pathname
                                  :directory (list :relative "Source" "Generated" "Classes"
                                                  (string (eightbol::cpu-display-name
                                                           *compile-regression-cpu*)))
                                  :name (concatenate 'string class-id "Class")
                                  :type "s")
                                tmp)))
              (is (probe-file output-path)
                  (format nil "Output ~a must exist for ~a"
                          (namestring output-path) (namestring cob)))
              (let ((content (alexandria:read-file-into-string output-path)))
                (is (> (length content) 0)
                    (format nil "Output for ~a must be non-empty" (namestring cob)))
                (is (search class-id content)
                    (format nil "Output for ~a must contain class name ~a"
                            (namestring cob) class-id))
                (is (some (lambda (pat) (search pat content))
                          (compile-regression-output-patterns *compile-regression-cpu*))
                    (format nil
                            "Output for ~a must contain .block, .proc, Method, ret, or rts"
                            (namestring cob)))))))))))

(test compile-regression/all-backends-one-fixture
  "One fixture compiles for all 11 CPUs (regression for to-pascal-case and backends)."
  (let ((cob (merge-pathnames
              (make-pathname :name "MinimalEmpty" :type "cob")
              (compile-regression-fixtures-directory)))
        (cpus +supported-cpus+))
    (when (probe-file cob)
      (let ((tmp (uiop:ensure-directory-pathname
                  (merge-pathnames
                   (make-pathname :directory
                                  `(:relative "eightbol-all-cpus-"
                                    ,(format nil "~a" (get-internal-real-time))))
                   (uiop:temporary-directory)))))
        (ensure-directories-exist tmp)
        (compile-regression-generate-copybooks tmp)
        (let ((copybook-paths (compile-regression-copybook-paths tmp)))
          (compile-eightbol-class (list cob)
                                  :cpus cpus
                                  :copybook-paths copybook-paths
                                  :root-directory tmp)
          (dolist (cpu cpus)
            (let* ((output-path (merge-pathnames
                                 (make-pathname
                                  :directory (list :relative "Source" "Generated" "Classes"
                                                   (string (eightbol::cpu-display-name cpu)))
                                  :name "MinimalEmptyClass"
                                  :type "s")
                                tmp)))
              (is (probe-file output-path)
                  (format nil "Output for CPU ~a must exist"
                          (eightbol::cpu-display-name cpu))))))))))

(test compile-regression/multi-backend-compile
  "MinimalEmpty compiles for cp1610, Z80, and m68k with expected output patterns."
  (let ((cob (merge-pathnames
              (make-pathname :name "MinimalEmpty" :type "cob")
              (compile-regression-fixtures-directory)))
        (cpus '(:cp1610 :z80 :m68k)))
    (when (probe-file cob)
      (let ((tmp (uiop:ensure-directory-pathname
                  (merge-pathnames
                   (make-pathname :directory
                                  `(:relative "eightbol-multi-backend-"
                                    ,(format nil "~a" (get-internal-real-time))))
                   (uiop:temporary-directory)))))
        (ensure-directories-exist tmp)
        (compile-regression-generate-copybooks tmp)
        (let ((copybook-paths (compile-regression-copybook-paths tmp)))
          (compile-eightbol-class (list cob)
                                  :cpus cpus
                                  :copybook-paths copybook-paths
                                  :root-directory tmp)
          (dolist (cpu cpus)
            (let* ((output-path (merge-pathnames
                                 (make-pathname
                                  :directory (list :relative "Source" "Generated" "Classes"
                                                   (string (eightbol::cpu-display-name cpu)))
                                  :name "MinimalEmptyClass"
                                  :type "s")
                                tmp)))
              (is (probe-file output-path)
                  (format nil "Output ~a must exist for ~a" (namestring output-path) cpu))
              (when (probe-file output-path)
                (let ((content (alexandria:read-file-into-string output-path)))
                  (is (> (length content) 0)
                      (format nil "Output for ~a must be non-empty" cpu))
                  (is (search "MinimalEmpty" content)
                      (format nil "Output for ~a must contain class name" cpu))
                  (is (some (lambda (pat) (search pat content))
                            (compile-regression-output-patterns cpu))
                      (format nil "Output for ~a must match CPU-specific patterns" cpu)))))))))))
