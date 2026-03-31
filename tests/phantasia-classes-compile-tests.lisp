;; tests/phantasia-classes-compile-tests.lisp — Phantasia Source/Classes copybook + compile checks
;;
;; To run: (asdf:test-system :eightbol) — suite :phantasia-classes-compile

(in-package :eightbol/test)

(defun phantasia-project-root ()
  "Return truename of Phantasia repo root (directory containing SkylineTool/), or NIL."
  (let ((eightbol-dir (asdf:system-source-directory :eightbol)))
    (when eightbol-dir
      (let ((cand (merge-pathnames "../../" (uiop:pathname-directory-pathname eightbol-dir))))
        (when (and (probe-file (merge-pathnames "Source/Classes/Classes.Defs" cand))
                   (probe-file (merge-pathnames "SkylineTool/eightbol/eightbol.asd" cand)))
          (truename cand))))))

(defun phantasia-classes-defs-path ()
  (let ((root (phantasia-project-root)))
    (when root
      (merge-pathnames "Source/Classes/Classes.Defs" root))))

(defun phantasia-class-cob-files ()
  (let ((root (phantasia-project-root)))
    (when root
      (let ((dir (merge-pathnames "Source/Classes/" root)))
        (when (probe-file dir)
          (sort (directory (merge-pathnames "*.cob" dir))
                #'string<
                :key #'namestring))))))

(defun phantasia-globals-cpy ()
  "Return pathname of Phantasia-Globals.cpy under Generated/7800/Classes, or NIL."
  (let ((root (phantasia-project-root)))
    (when root
      (let ((p (merge-pathnames "Source/Generated/7800/Classes/Phantasia-Globals.cpy" root)))
        (when (probe-file p) p)))))

(fiveam:def-suite :phantasia-classes-compile
  :description "Phantasia game class copybooks and optional full .cob compile")
(in-suite :phantasia-classes-compile)

(test phantasia-classes-compile/project-root-or-skip
  "When Phantasia tree is present, Classes.Defs exists."
  (if (phantasia-project-root)
      (is (probe-file (phantasia-classes-defs-path)))
      (skip "Phantasia project root not found")))

(test phantasia-classes-compile/make-eightbol-copybooks-anenmy-variant
  "make-eightbol-copybooks writes *-Slots.cpy for Anénemy (accented CLASS-ID)."
  (unless (phantasia-project-root)
    (skip "Phantasia project root not found"))
  (let* ((tmp (uiop:ensure-directory-pathname
               (merge-pathnames
                (make-pathname :directory (list :relative "phantasia-anenmy-cpy-test"
                                                (format nil "~a" (get-internal-real-time))))
                (uiop:temporary-directory))))
         (defs (merge-pathnames "Source/Classes/Classes.Defs" tmp))
         (cpy-name (concatenate 'string
                                (skyline-tool::pascal-to-copybook-filename "Anénemy")
                                "-Slots"))
         (cpy-path (merge-pathnames (make-pathname :name cpy-name :type "cpy")
                                    (merge-pathnames "Source/Generated/7800/Classes/" tmp))))
    (ensure-directories-exist defs)
    (with-open-file (out defs :direction :output :if-exists :supersede)
      (write-line ";;; Fixture — Anénemy copybook filename (accented id)" out)
      (write-line "Entity < BasicObject" out)
      (write-line ".Decal 1" out)
      (write-line "Anenemy < Entity" out)
      (write-line ".State 1" out)
      (write-line "Anénemy < Anenemy" out))
    (let ((skyline-tool::*machine* 7800))
      (uiop:call-with-current-directory tmp
        (lambda ()
          (skyline-tool::make-eightbol-copybooks defs)
          (is (probe-file cpy-path)
              "Expected ~a" (namestring cpy-path)))))))

(test phantasia-classes-compile/all-cobs-6502
  "Each Source/Classes/*.cob compiles for 6502 after copybooks in a temp tree.
Skipped when Phantasia-Globals.cpy is missing (generate with skyline-tool first)."
  (unless (phantasia-project-root)
    (skip "Phantasia project root not found"))
  (unless (phantasia-globals-cpy)
    (skip "Phantasia-Globals.cpy not found under Source/Generated/7800/Classes/"))
  (let* ((proj (phantasia-project-root))
         (defs (phantasia-classes-defs-path))
         (cobs (phantasia-class-cob-files))
         (globals-dir (uiop:pathname-directory-pathname (phantasia-globals-cpy)))
         (tmp (uiop:ensure-directory-pathname
               (merge-pathnames
                (make-pathname :directory (list :relative "phantasia-all-cob-"
                                                (format nil "~a" (get-internal-real-time))))
                (uiop:temporary-directory))))
         (tmp-classes (merge-pathnames "Source/Generated/7800/Classes/" tmp)))
    (ensure-directories-exist tmp-classes)
    ;; Service bank LUT is built from Source/Code/{machine}/ under root; symlink project tree.
    (let ((src-code7800 (merge-pathnames "Source/Code/7800/" proj))
          (dst-code-parent (merge-pathnames "Source/Code/" tmp)))
      (ensure-directories-exist dst-code-parent)
      (let ((dst7800 (merge-pathnames "7800" dst-code-parent)))
        (when (and (probe-file src-code7800) (not (probe-file dst7800)))
          (uiop:run-program (list "ln" "-s"
                                  (namestring (truename src-code7800))
                                  (namestring dst7800))
                            :ignore-error-status nil))))
    (let ((skyline-tool::*machine* 7800))
      (uiop:call-with-current-directory tmp
        (lambda ()
          (skyline-tool::make-eightbol-copybooks defs))))
    (let ((copybook-paths (list (uiop:ensure-directory-pathname tmp-classes)
                                (uiop:ensure-directory-pathname globals-dir))))
      (dolist (cob cobs)
        (let (ast)
          (handler-case
              (setf ast (compile-eightbol-class (list cob)
                                                 :cpus '(:6502)
                                                 :copybook-paths copybook-paths
                                                 :root-directory tmp))
            (error (e)
              (fail "Compile failed for ~a: ~a" cob e)))
          (is (listp ast))
          (is (eq :program (first ast)))
          (let* ((class-id (pathname-name cob))
                 (out (merge-pathnames
                       (make-pathname
                        :directory (list :relative "Source" "Generated" "Classes"
                                        (string (eightbol::cpu-display-name :6502)))
                        :name (concatenate 'string class-id "Class")
                        :type "s")
                       tmp)))
            (is (probe-file out) "Missing ~a" out)
            (let ((content (alexandria:read-file-into-string out)))
              (is (> (length content) 0))
              (is (search class-id content)))))))))

(test phantasia-classes-compile/library-vs-service-call-codegen
  "Regression: CALL ... IN LIBRARY emits @code{jsr Lib.*}; CALL SERVICE emits far call.

Prevents regressions where library calls became local @code{jmp} and where
service dispatch failed to resolve via the service-bank LUT."
  (unless (phantasia-project-root)
    (skip "Phantasia project root not found"))
  (let* ((proj (phantasia-project-root))
         (tmp (uiop:ensure-directory-pathname
               (merge-pathnames
                (make-pathname :directory (list :relative "phantasia-call-regression-"
                                                (format nil "~a" (get-internal-real-time))))
                (uiop:temporary-directory))))
         (cob (merge-pathnames "CallSemantics.cob" tmp))
         (out (merge-pathnames "Source/Generated/Classes/6502/CallSemanticsClass.s" tmp))
         (service-bank (merge-pathnames "Source/Code/7800/Banks/Bank11/" proj)))
    (ensure-directories-exist cob)
    ;; Provide a bank tree so service-bank LUT can resolve ServiceCheckWall -> BankAnimation.
    (ensure-directories-exist (merge-pathnames "Source/Code/7800/Banks/" tmp))
    (let ((dst (merge-pathnames "Source/Code/7800/Banks/Bank11" tmp)))
      (unless (probe-file dst)
        (uiop:run-program (list "ln" "-s"
                                (namestring (truename service-bank))
                                (namestring dst))
                          :ignore-error-status nil)))
    (with-open-file (s cob :direction :output :if-exists :supersede)
      (write-line "000010 IDENTIFICATION DIVISION." s)
      (write-line "000020 CLASS-ID. CallSemantics." s)
      (write-line "000030 ENVIRONMENT DIVISION." s)
      (write-line "000040 OBJECT." s)
      (write-line "000050     DATA DIVISION." s)
      (write-line "000060         WORKING-STORAGE SECTION." s)
      (write-line "000070         05 Dummy PIC 99 USAGE BINARY." s)
      (write-line "000080     PROCEDURE DIVISION." s)
      (write-line "000090         IDENTIFICATION DIVISION." s)
      (write-line "000100         METHOD-ID. \"Step\"." s)
      (write-line "000110         PROCEDURE DIVISION." s)
      (write-line "000120             CALL Move-Decal-Y IN LIBRARY." s)
      (write-line "000130             CALL SERVICE Check-Wall." s)
      (write-line "000140             GOBACK." s)
      (write-line "000150         END METHOD \"Step\"." s)
      (write-line "000160 END OBJECT." s)
      (write-line "000170 END CLASS CallSemantics." s))
    (let ((ast (compile-eightbol-class (list cob)
                                       :cpus '(:6502)
                                       :copybook-paths '()
                                       :root-directory tmp)))
      (is (listp ast))
      (is (probe-file out) "Missing ~a" out)
      (let ((asm (alexandria:read-file-into-string out)))
        (is (search "jsr Lib.MoveDecalY" asm))
        (is (or (search ".FarCall ServiceCheckWall, BankAnimation" asm)
                (search ".FarJSR BankAnimation, ServiceCheckWall" asm)))
        (is (null (search "jmp MoveDecalY" asm)))))))
