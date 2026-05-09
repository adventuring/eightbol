;; basic-transpile.lisp — Dartmouth BASIC → EIGHTBOL COBOL text (front-end only)
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defparameter *basic-default-game-name* "Phantasia"
  "Default COPY book stem for @code{$(GAME)-Globals.cpy} when emitting class shells.")

(defun basic-date-yyyy-mm-dd (&optional (universal-time (get-universal-time)))
  "Format UNIVERSAL-TIME as @code{YYYY-MM-DD} for DATE-WRITTEN in generated COBOL."
  (local-time:format-timestring nil
                                (local-time:universal-to-timestamp universal-time)
                                :format '(:year #\- (:month 2) #\- (:day 2))))

(defun basic-line-number-regex ()
  "Return the scanner for optional leading BASIC line numbers (digits only)."
  ;; Leading digits, then body; METHOD etc. may appear after the number.
  "^\\s*(\\d+)\\s+(.*)\\s*$")

(defun parse-basic-source-lines (text)
  "Split TEXT into sorted @code{(line-number . body-stripped)} alist.
BODY is the remainder of the line after the leading line number, or the
whole trimmed line when no number is present (then @code{NIL} line number
for caller to assign)."
  (let ((lines (serapeum:lines text))
        (acc '())
        (rx (cl-ppcre:create-scanner (basic-line-number-regex)
                                     :case-insensitive-mode t)))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\; (char trimmed 0)))
          (multiple-value-bind (whole groups)
              (cl-ppcre:scan-to-strings rx trimmed)
            (declare (ignore whole))
            (if groups
                (push (cons (parse-integer (aref groups 0) :junk-allowed nil)
                            (string-trim '(#\Space #\Tab) (aref groups 1)))
                  acc)
                (push (cons nil trimmed) acc))))))
    (stable-sort (nreverse acc) #'< :key (lambda (x) (or (car x) 0)))))

(defun ensure-basic-line-numbers (lines)
  "Return LINES with @code{NIL} car replaced by synthetic numbers (10,20,…)."
  (let ((next 10)
        (out '()))
    (dolist (pair lines)
      (if (car pair)
          (push pair out)
          (progn
            (push (cons next (cdr pair)) out)
            (incf next 10))))
    (stable-sort (nreverse out) #'< :key #'car)))

(defun basic-paragraph-name (line-no)
  "Return a COBOL paragraph label for BASIC line number LINE-NO."
  (format nil "SEC-~D" line-no))

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
  "Partition sorted LINES into @code{((method-id-string . lines-in-method) …)}.
Lines before the first METHOD go into a synthetic @code{\"Class-P\"} method."
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

(defun basic-cobol-comment (text)
  "Format TEXT as a fixed-area COBOL comment line (asterisk in column 7)."
  (format nil "      * ~A" (string-trim '(#\Space #\Tab) text)))

(defun basic-transpile-expression (expr)
  "Map a BASIC expression string toward COBOL/EIGHTBOL token syntax (minimal).
Preserves hyphenated identifiers by turning spaces around @code{OF} into
COBOL qualification style where possible."
  (let ((e (string-trim '(#\Space #\Tab) expr)))
    ;; Normalize "field" OF SELF → Field OF Self (quotes stripped on first token)
    (setf e (cl-ppcre:regex-replace-all
             "(?i)\\bOF\\b" e "OF"))
    ;; Strip quotes around simple identifiers in expr: "HP" → HP
    (setf e (cl-ppcre:regex-replace-all "\"([^\"]+)\"" e "\\1"))
    e))

(defun basic-transpile-assignment (body)
  "Transpile @code{LET lhs = rhs} to COBOL SET … TO … or comment on failure."
  (let ((rx (cl-ppcre:create-scanner
             "^\\s*LET\\s+(.+?)\\s*=\\s*(.+)$"
             :case-insensitive-mode t
             :multi-line-mode t)))
    (multiple-value-bind (whole groups)
        (cl-ppcre:scan-to-strings rx body)
      (if (not whole)
          (basic-cobol-comment (format nil "UNTRANSLATED: ~A" body))
          (let ((lhs (basic-transpile-expression (strip-basic-quotes (aref groups 0))))
                (rhs (basic-transpile-expression (aref groups 1))))
            (format nil "           SET ~A TO ~A." lhs rhs))))))

(defun basic-transpile-statement-one-line (body)
  "Return one or more COBOL source lines (strings) for one BASIC line body BODY."
  (let ((b (string-trim '(#\Space #\Tab) body)))
    (cond
      ((zerop (length b))
       nil)
      ((basic-tokenize-method-name b)
       nil)
      ((cl-ppcre:scan "(?i)^ASSEMBLY\\s+ENTRY\\s+" b)
       (let* ((rest (cl-ppcre:regex-replace "(?i)^ASSEMBLY\\s+ENTRY\\s+" b ""))
              (tok (string-trim '(#\Space #\Tab) rest))
              (end (when (plusp (length tok))
                     (position-if (lambda (c)
                                    (member c '(#\Space #\Tab #\Return #\Linefeed)))
                                  tok))))
         (if (not (plusp (length tok)))
             (list (basic-cobol-comment "ASSEMBLY ENTRY (missing name)"))
             (list (format nil "           ASSEMBLY ENTRY ~A."
                           (string-upcase (subseq tok 0 (or end (length tok)))))))))
      ((cl-ppcre:scan "(?i)^REM\\b" b)
       (list (basic-cobol-comment (subseq b (min (length b) 4)))))
      ((cl-ppcre:scan "(?i)^END\\s*$" b)
       (list "           GOBACK."))
      ((cl-ppcre:scan "(?i)^RETURN\\s*$" b)
       (list "           EXIT."))
      ((cl-ppcre:scan "(?i)^LET\\b" b)
       (list (basic-transpile-assignment b)))
      ((cl-ppcre:scan "(?i)^GOTO\\s+" b)
       (let ((target (parse-integer (cl-ppcre:regex-replace "(?i)^GOTO\\s+" b "")
                                    :junk-allowed nil)))
         (list (format nil "           GO TO ~A." (basic-paragraph-name target)))))
      ((cl-ppcre:scan "(?i)^GO\\s+TO\\s+" b)
       (let ((target (parse-integer (cl-ppcre:regex-replace "(?i)^GO\\s+TO\\s+" b "")
                                    :junk-allowed nil)))
         (list (format nil "           GO TO ~A." (basic-paragraph-name target)))))
      ((cl-ppcre:scan "(?i)^GOSUB\\s+" b)
       (let ((target (parse-integer (cl-ppcre:regex-replace "(?i)^GOSUB\\s+" b "")
                                    :junk-allowed nil)))
         (list (format nil "           PERFORM ~A." (basic-paragraph-name target)))))
      ((cl-ppcre:scan "(?i)^RESTORE\\s+" b)
       (list (basic-cobol-comment (format nil "RESTORE ~A (not transpiled)"
                                           (string-trim '(#\Space #\Tab)
                                                        (cl-ppcre:regex-replace "(?i)^RESTORE\\s+" b ""))))))
      ((cl-ppcre:scan "(?i)^READ\\b" b)
       (list (basic-cobol-comment (format nil "READ … (not transpiled): ~A" b))))
      ((cl-ppcre:scan "(?i)^IF\\b" b)
       (basic-transpile-if b))
      ((cl-ppcre:scan "(?i)^FOR\\b" b)
       (list (basic-cobol-comment (format nil "FOR … (not transpiled): ~A" b))))
      ((cl-ppcre:scan "(?i)^NEXT\\b" b)
       (list (basic-cobol-comment (format nil "NEXT … (not transpiled): ~A" b))))
      (t
       (list (basic-cobol-comment (format nil "BASIC: ~A" b)))))))

(defun basic-transpile-if (body)
  "Transpile a one-line IF … THEN … [ELSE …] toward COBOL IF/END-IF (minimal)."
  ;; IF cond THEN label-or-stmt [ELSE label-or-stmt]
  (let ((rx (cl-ppcre:create-scanner
             "(?i)^IF\\s+(.+?)\\s+THEN\\s+(.+?)(?:\\s+ELSE\\s+(.+))?$"
             :case-insensitive-mode t)))
    (multiple-value-bind (whole groups)
        (cl-ppcre:scan-to-strings rx body)
      (unless whole
        (return-from basic-transpile-if
          (list (basic-cobol-comment (format nil "IF (unparsed): ~A" body)))))
      (let* ((cond-part (basic-transpile-expression (aref groups 0)))
             (then-part (string-trim '(#\Space #\Tab) (aref groups 1)))
             (else-part (when (and (> (length groups) 2) (aref groups 2))
                          (string-trim '(#\Space #\Tab) (aref groups 2)))))
        (flet ((branch-stmt (s)
                 (if (every #'digit-char-p s)
                     (format nil "GO TO ~A" (basic-paragraph-name (parse-integer s)))
                     "NEXT SENTENCE")))
          (let ((then-stmt (branch-stmt then-part))
                (else-stmt (when else-part (branch-stmt else-part))))
            (if else-stmt
                (list (format nil "           IF ~A THEN ~A ELSE ~A END-IF."
                              cond-part then-stmt else-stmt))
                (list (format nil "           IF ~A THEN ~A END-IF."
                              cond-part then-stmt)))))))))

(defun basic-collect-goto-targets (method-lines)
  "Return sorted list of line numbers referenced by GOTO/GO TO/GOSUB/IF branches."
  (let ((targets '()))
    (flet ((add (n) (pushnew n targets)))
      (dolist (pair method-lines)
        (let ((b (cdr pair)))
          (when (cl-ppcre:scan "(?i)^GOTO\\s+(\\d+)" b)
            (add (parse-integer (cl-ppcre:regex-replace "(?i)^GOTO\\s+" b "")
                                :junk-allowed nil)))
          (when (cl-ppcre:scan "(?i)^GO\\s+TO\\s+(\\d+)" b)
            (add (parse-integer (cl-ppcre:regex-replace "(?i)^GO\\s+TO\\s+" b "")
                                :junk-allowed nil)))
          (when (cl-ppcre:scan "(?i)^GOSUB\\s+(\\d+)" b)
            (add (parse-integer (cl-ppcre:regex-replace "(?i)^GOSUB\\s+" b "")
                                :junk-allowed nil)))
          (multiple-value-bind (whole groups)
              (cl-ppcre:scan-to-strings
               "(?i)^IF\\s+(.+?)\\s+THEN\\s+(.+?)(?:\\s+ELSE\\s+(.+))?$"
               b)
            (when whole
              (dolist (part (list (aref groups 1) (when (aref groups 2) (aref groups 2))))
                (when part
                  (let ((s (string-trim '(#\Space #\Tab) part)))
                    (when (every #'digit-char-p s)
                      (add (parse-integer s))))))))))
    (sort targets #'<)))

(defun basic-method-body-to-cobol (method-id method-lines)
  "Emit COBOL procedure text for one method from sorted (lineno . body) pairs."
  (let* ((filtered (remove-if (lambda (p)
                                (or (basic-tokenize-method-name (cdr p))
                                    (zerop (length (string-trim '(#\Space #\Tab) (cdr p))))))
                              method-lines))
         (out (make-string-output-stream)))
    (when (null filtered)
      (format out "          IDENTIFICATION DIVISION.~%")
      (format out "          METHOD-ID. \"~A\".~%" method-id)
      (format out "          PROCEDURE DIVISION.~%")
      (format out "           GOBACK.~%")
      (format out "          END METHOD \"~A\".~%~%" method-id)
      (return-from basic-method-body-to-cobol (get-output-stream-string out)))
    (format out "          IDENTIFICATION DIVISION.~%")
    (format out "          METHOD-ID. \"~A\".~%" method-id)
    (format out "          PROCEDURE DIVISION.~%")
    (loop for (line-no . body) in filtered
          for next-pair = (cdr (member line-no filtered :key #'car :test #'=))
          for next-no = (car (first next-pair))
          do
             (format out "       ~A.~%" (basic-paragraph-name line-no))
             (dolist (line (basic-transpile-statement-one-line body))
               (format out "~A~%" line))
             (let ((ends (or (cl-ppcre:scan "(?i)^(END|RETURN)\\s*$" (string-trim '(#\Space #\Tab) body))
                             (cl-ppcre:scan "(?i)^GOTO\\b" body)
                             (cl-ppcre:scan "(?i)^GO\\s+TO\\b" body)
                             (cl-ppcre:scan "(?i)^GOSUB\\b" body)
                             (cl-ppcre:scan "(?i)^IF\\b" body))))
               (unless ends
                 (when next-no
                   (format out "           GO TO ~A.~%" (basic-paragraph-name next-no)))))))
    (format out "          END METHOD \"~A\".~%~%" method-id)
    (get-output-stream-string out)))

(defun transpile-basic-to-cobol-string (class-id basic-text
                                      &key (game-name *basic-default-game-name*))
  "Transpile UTF-8 BASIC source BASIC-TEXT to one EIGHTBOL class file string.
CLASS-ID is the @code{CLASS-ID} stem (e.g. @code{\"Slime\"}). GAME-NAME selects
@code{COPY {GAME-NAME}-Globals.}  Within a method, a line @code{ASSEMBLY ENTRY Foo}
becomes COBOL @code{ASSEMBLY ENTRY FOO.} as the first procedure statement.

@table @asis
@item CLASS-ID
Header-case class identifier string.
@item BASIC-TEXT
Full program text (may omit line numbers on some lines; they are filled).
@end table

@subsection Outputs
A single string suitable for @code{parse-eightbol} from a string stream."
  (let* ((lines (ensure-basic-line-numbers (parse-basic-source-lines basic-text)))
         (methods (split-basic-into-methods lines))
         (globals-copy (format nil "~A-Globals" game-name))
         (out (make-string-output-stream)))
    (format out "** EIGHTBOL Dartmouth BASIC (generated)~%")
    (format out "        COPY Classes.~%")
    (format out "        COPY ~A.~%" globals-copy)
    (format out "        COPY Asset-IDs.~%~%")
    (format out "       IDENTIFICATION DIVISION.~%")
    (format out "       CLASS-ID. ~A.~%" class-id)
    (format out "       AUTHOR. BASIC-Shell.~%")
    (format out "       DATE-WRITTEN. ~A.~%" (basic-date-yyyy-mm-dd))
    (format out "       ENVIRONMENT DIVISION.~%")
    (format out "       OBJECT.~%")
    (format out "          PROCEDURE DIVISION.~%~%")
    (dolist (m methods)
      (write-string (basic-method-body-to-cobol (car m) (cdr m)) out))
    (format out "       END OBJECT.~%")
    (format out "       END CLASS ~A.~%" class-id)
    (get-output-stream-string out)))

(defun class-id-from-bas-pathname (pathname)
  "Derive EIGHTBOL @code{CLASS-ID} stem from a @code{.bas} pathname (no directory)."
  (header-case (pathname-name (pathname pathname))))
