;;; Basic  transpile  functionality  Converts EIGHTBOL  code  to  target
;;; backend  assembly  Handles  OPCODE  mapping  and  special  variables
;;; Maintains  state across  parser/lexer iterations  Register: Internal
;;; data  storage  mechanism  Alternative:  Use  'basic-shell'  for  CLI
;;; operations — Dartmouth BASIC → EIGHTBOL COBOL text (front-end only)
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :eightbol)

(defparameter *basic-default-game-name* "Phantasia"
  "Default COPY book stem for @code{$(GAME)-Globals.cpy} when emitting class shells.")

(defun basic-date-yyyy-mm-dd (&optional (universal-time (get-universal-time)))
  "Format UNIVERSAL-TIME as @code{YYYY-MM-DD} for DATE-WRITTEN in generated COBOL."
  (local-time:format-timestring nil
                              (local-time:universal-to-timestamp universal-time)
                              :format '(:year #\- (:month 2) #\- (:day 2))))

(defun basic-line-number-regex ()
  "Return scanner for line numbers with optional quoted labels.
   Matches: '10' or '10 \"LABEL\"' at start of line."
  "^\\s*(\\d+)(?:\\s+\"([^\"]+)\")?\\s*(.*)")

(defun parse-basic-source-lines (text)
  "Split TEXT into alist of (line-no . body). Handles numeric labels
   and quoted string labels after the number. Lines beginning with ';'
   (or empty) are ignored."
  (let ((lines (serapeum:lines text))
        (acc '())
        (rx (cl-ppcre:create-scanner (basic-line-number-regex)
                                     :case-insensitive-mode t)))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\; (char trimmed 0))
                    (char= #\' (char trimmed 0))) ; REM in BASIC
          (multiple-value-bind (whole groups)
              (cl-ppcre:scan-to-strings rx trimmed)
            (declare (ignore whole))
            (when groups
              (let* ((lineno (parse-integer (aref groups 0)))
                     (str-label (and (> (length groups) 1) (aref groups 1)))
                     (body (and (> (length groups) 2) (aref groups 2))))
                (when body
                  (push (cons (list lineno str-label)
                              (string-trim '(#\Space #\Tab) body))
                        acc))))))
        (stable-sort (nreverse acc) #'< :key (lambda (x) (car (car x))))))))

(defun strip-basic-quotes (s)
  "Remove one pair of surrounding ASCII double-quotes from S when present."
  (let ((u (string-trim '(#\Space #\Tab) s)))
    (if (and (>= (length u) 2)
             (char= #\" (char u 0))
             (char= #\" (char u (1- (length u)))))
        (subseq u 1 (1- (length u)))
        u)))

(defun basic-tokenize-method-name (body)
  "If BODY is @code{METHOD \"…\"} (case-insensitive), return the method string."
  (let ((rx (cl-ppcre:create-scanner
             "^\\s*METHOD\\s+\"([^\"]+)\"\\s*$"
             :case-insensitive-mode t)))
    (multiple-value-bind (whole groups)
        (cl-ppcre:scan-to-strings rx body)
      (when whole
        (aref groups 0)))))

(defun split-basic-into-methods (lines)
  "Partition sorted LINES into @code{((method-id-string . lines-in-method) …)}."
  (let ((current "Class-P")
        (buckets (make-hash-table :test 'equalp))
        (order (list "Class-P")))
    (dolist (pair lines)
      (let* ((body (cdr pair))
             (mn (basic-tokenize-method-name body)))
        (cond
          (mn
           (setf current mn)
           (unless (member current order :test #'string-equal)
             (setf order (append order (list current)))))
          (t
           (appendf (gethash current buckets '()) (list pair))))))
    (loop for id in order
          for lines-in = (gethash id buckets)
          when lines-in
          collect (cons id lines-in))))

(defun basic-transpile-expression (expr)
  "Map a BASIC expression string toward COBOL/EIGHTBOL token syntax (minimal)."
  (let ((e (string-trim '(#\Space #\Tab) expr)))
    ;; Normalize "field OF object" → preserve OF for qualification
    (setf e (cl-ppcre:regex-replace-all "(?i)\\bOF\\b" e "OF "))
    ;; Strip quotes around simple identifiers
    (setf e (cl-ppcre:regex-replace-all "\"([^\"]+)\"" e "\\1"))
    e))

(defun basic-transpile-statement-one-line (line)
  "Transpile one BASIC LINE (string) to COBOL statement string.
Handles assignment, control flow, and method calls."
  (let ((l (string-trim '(#\Space #\Tab #\Return #\Linefeed) line)))
    (cond
      ;; LET assignment: LET A = B → MOVE B TO A
      ((cl-ppcre:scan "^\\s*LET\\s+([A-Za-z0-9_]+)\\s*=\\s*(.+)\\s*$" l :case-insensitive-mode t)
       (multiple-value-bind (whole parts)
           (cl-ppcre:scan-to-strings "^\\s*LET\\s+([A-Za-z0-9_]+)\\s*=\\s*(.+)\\s*$" l :case-insensitive-mode t)
         (declare (ignore whole))
         (format nil "MOVE ~A TO ~A" (aref parts 1) (aref parts 0))))
      
      ;; GOTO: GOTO 100 → GO TO 100 (but GO TO is unsupported, so this should be an error)
      ((cl-ppcre:scan "^\\s*GOTO\\s+(\\d+)\\s*$" l :case-insensitive-mode t)
       (multiple-value-bind (whole parts)
           (cl-ppcre:scan-to-strings "^\\s*GOTO\\s+(\\d+)\\s*$" l :case-insensitive-mode t)
         (declare (ignore whole))
         (format nil "GO TO ~A" (aref parts 0))))
      
      ;; GOSUB: GOSUB 100 → PERFORM 100
      ((cl-ppcre:scan "^\\s*GOSUB\\s+(\\d+)\\s*$" l :case-insensitive-mode t)
       (multiple-value-bind (whole parts)
           (cl-ppcre:scan-to-strings "^\\s*GOSUB\\s+(\\d+)\\s*$" l :case-insensitive-mode t)
         (declare (ignore whole))
         (format nil "PERFORM ~A" (aref parts 0))))
      
      ;; RETURN → GOBACK
      ((cl-ppcre:scan "^\\s*RETURN\\s*$" l :case-insensitive-mode t)
       "GOBACK")
      
      ;; IF...THEN...ELSE structure (simplified)
      ((cl-ppcre:scan "^\\s*IF\\s+(.+)\\s+THEN\\s+(.+)\\s+(?:ELSE\\s+(.+))?\\s*$" l :case-insensitive-mode t)
       (multiple-value-bind (whole parts)
           (cl-ppcre:scan-to-strings "^\\s*IF\\s+(.+)\\s+THEN\\s+(.+)\\s+(?:ELSE\\s+(.+))?\\s*$" l :case-insensitive-mode t)
         (declare (ignore whole))
         (let ((condition (aref parts 0))
               (then-branch (aref parts 1))
               (else-branch (aref parts 2)))
           (if else-branch
               (format nil "IF ~A THEN ~A ELSE ~A" condition then-branch else-branch)
               (format nil "IF ~A THEN ~A" condition then-branch)))))
      
      ;; FOR loop: FOR I = 1 TO 10 → PERFORM VARYING I FROM 1 UNTIL I > 10
      ((cl-ppcre:scan "^\\s*FOR\\s+([A-Za-z0-9_]+)\\s*=\\s*(\\d+)\\s+TO\\s*(\\d+)\\s*(?:STEP\\s+(\\d+))?\\s*$" l :case-insensitive-mode t)
       (multiple-value-bind (whole parts)
           (cl-ppcre:scan-to-strings "^\\s*FOR\\s+([A-Za-z0-9_]+)\\s*=\\s*(\\d+)\\s+TO\\s*(\\d+)\\s*(?:STEP\\s+(\\d+))?\\s*$" l :case-insensitive-mode t)
         (declare (ignore whole))
         (let ((var (aref parts 0))
               (start (aref parts 1))
               (end (aref parts 2))
               (step (aref parts 3)))
           (if step
               (format nil "PERFORM VARYING ~A FROM ~A BY ~A UNTIL ~A > ~A" var start step var end)
               (format nil "PERFORM VARYING ~A FROM ~A UNTIL ~A > ~A" var start var end)))))
      
      ;; Default: pass through as-is (should be handled by caller for unsupported statements)
      (t l))))

(defun transpile-basic-to-cobol-string (class-id basic-text &key (game-name *basic-default-game-name*))
  "Transpile BASIC TEXT to COBOL string for CLASS-ID with GAME-NAME context.
Returns a complete COBOL program string including class definition and method."
  (let* ((lines (parse-basic-source-lines basic-text))
         (methods (split-basic-into-methods lines))
         (method-strings (loop for (method-name . method-lines) in methods
                               collect (transpile-basic-method method-name method-lines)))
         (class-header (format nil "
~8tIDENTIFICATION DIVISION.
~8tPROGRAM-ID. \"~A\".

~8tDATA DIVISION.
~8tWORKING-STORAGE SECTION.
~8tCOPY \"~A-Globals\".

~8tPROCEDURE DIVISION.

" class-id game-name)))
    (concatenate 'string class-header (format nil "~{~A~%~}" method-strings))))

(defun transpile-basic-method (method-name method-lines)
  "Transpile METHOD-LINES to a COBOL method with METHOD-NAME."
  (let ((statements (loop for (line-num . line-text) in method-lines
                          collect (basic-transpile-statement-one-line line-text))))
    (format nil "METHOD-ID. \"~A\".~%PROCEDURE DIVISION.~%~{~%~A~}~%EXIT METHOD.~%~%END METHOD \"~A\".~%~%" 
            method-name 
            (remove-if #'null statements)
            method-name)))

(defun ensure-basic-line-numbers (lines)
  "Ensure LINE-NUMBERS are sequential starting from 10, skipping multiples of 10 for labels."
  (let ((current 10)
        (result '()))
    (dolist (pair lines)
      (let ((original-line (car pair))
            (line-text (cdr pair)))
        (push (cons current line-text) result)
        ;; Skip to next non-multiple of 10
        (loop do (incf current 10)
              while (zerop (mod current 10)))))
    (nreverse result)))

(defun basic-transpile-to-assembly (basic-text &key (cpu :6502))
  "Transpile BASIC TEXT to assembly for CPU.
Returns assembly string or NIL on error."
  (handler-case
      (let* ((class-id "BasicProgram")
             (cobol-text (transpile-basic-to-cobol-string class-id basic-text))
             (tmp-file (uiop:with-temporary-file (:pathname tmp :suffix "basic-transpile.cob")))
             (result-file (uiop:with-temporary-file (:pathname result :suffix ".s"))))
        (with-open-file (out tmp-file :direction :output :if-exists :supersede :external-format :utf-8)
          (write-string cobol-text out))
        (let ((compile-result (compile-eightbol (list tmp-file)
                                              :cpus (list cpu)
                                              :output-file result-file)))
          (when (zerop compile-result)
            (with-open-file (in result-file :direction :input :element-type 'character)
              (let ((content (make-string (file-length in))))
                (read-sequence content in)
                content)))))
    (error (e)
      (format *error-output* "BASIC transpile error: ~A~%" e)
      nil)))

(defun basic-shell-run (basic-text &key (cpu :6502) (game-name *basic-default-game-name*))
  "Transpile BASIC TEXT and run it for CPU using Eightbol.
Returns compilation result (0 on success)."
  (handler-case
      (let* ((class-id "BasicProgram")
             (cobol-text (transpile-basic-to-cobol-string class-id basic-text :game-name game-name))
             (tmp-file (uiop:with-temporary-file (:pathname tmp :suffix "basic-shell-run.cob"))))
        (with-open-file (out tmp-file :direction :output :if-exists :supersede :external-format :utf-8)
          (write-string cobol-text out))
        (compile-eightbol (list tmp-file) :cpus (list cpu)))
    (error (e)
      (format *error-output* "BASIC shell run error: ~A~%" e)
      1)))
