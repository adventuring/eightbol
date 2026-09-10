;;; Basic AST emission — Direct BASIC → AST conversion (no COBOL transpile step)
;;; Dartmouth BASIC → EIGHTBOL canonical AST (not COBOL)
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :eightbol)

(defparameter *basic-default-game-name* "Phantasia"
  "Default COPY book stem for @code{$(GAME)-Globals.cpy} when emitting class shells.")

(defun basic-date-yyyy-mm-dd (&optional (universal-time (get-universal-time)))
  "Format UNIVERSAL-TIME as @code{YYYY-MM-DD} for DATE-WRITTEN in generated metadata."
  (local-time:format-timestring nil
                              (local-time:universal-to-timestamp universal-time)
                              :format '(:year #\- (:month 2) #\- (:day 2))))

(defun parse-basic-source-lines (text)
  "Parse BASIC TEXT into an alist of (line-no . body).
   Lines beginning with ';' (or empty) are ignored."
  (let ((lines (serapeum:lines text))
        (acc '()))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (unless (or (zerop (length trimmed))
                    (char= #\; (char trimmed 0))
                    (char= #\' (char trimmed 0))) ; REM in BASIC
          (let ((parts (split-sequence:split-sequence #\Space trimmed :remove-empty-subseqs t)))
            (when (first parts)
              (let* ((first-part (first parts))
                     (numeric-p (every #'digit-char-p first-part)))
                (when numeric-p
                  (let ((lineno (parse-integer first-part))
                        (body (string-trim '(#\Space #\Tab)
                                         (subseq trimmed (length first-part)))))
                    (when body
                      (push (cons lineno body) acc))))))))))
    (stable-sort (nreverse acc) #'< :key #'car)))

(defun basic-parse-expression-from-string (expr-str)
  "Parse a BASIC expression string into an AST expression node.
   Handles: identifiers, literals, operators, function calls."
  (let ((trimmed (string-trim '(#\Space #\Tab) expr-str)))
    (cond
      ;; Numeric literal
      ((every #'(lambda (c) (or (digit-char-p c) (char= c #\.)))
              trimmed)
       (read-from-string trimmed))
      
      ;; String literal
      ((and (> (length trimmed) 1)
            (char= #\" (char trimmed 0))
            (char= #\" (char trimmed (1- (length trimmed)))))
       (subseq trimmed 1 (1- (length trimmed))))
      
      ;; Variable/identifier
      ((every #'(lambda (c) (or (alphanumericp c) (char= c #\_) (char= c #$)))
              trimmed)
       (make-identifier trimmed))
      
      ;; Default: return as-is (will be validated later)
      (t trimmed))))

(defun basic-transpile-statement (stmt-text)
  "Convert a single BASIC statement text to an AST node.
   Returns NIL for unrecognized statements."
  (let ((stmt (string-trim '(#\Space #\Tab #\Return #\Linefeed) stmt-text)))
    (cond
      ;; LET assignment: LET A = B → :move node
      ((cl-ppcre:scan "^(?i)LET\\s+([A-Za-z_][A-Za-z0-9_$]*)\\s*=\\s*(.+)$" stmt)
       (multiple-value-bind (whole groups)
           (cl-ppcre:scan-to-strings "^(?i)LET\\s+([A-Za-z_][A-Za-z0-9_$]*)\\s*=\\s*(.+)$" stmt)
         (declare (ignore whole))
         (let ((target (aref groups 0))
               (expr (aref groups 1)))
           (make-move-node (basic-parse-expression-from-string expr)
                          (make-identifier target)))))
      
      ;; Assignment without LET: A = B → :move node
      ((cl-ppcre:scan "^([A-Za-z_][A-Za-z0-9_$]*)\\s*=\\s*(.+)$" stmt)
       (multiple-value-bind (whole groups)
           (cl-ppcre:scan-to-strings "^([A-Za-z_][A-Za-z0-9_$]*)\\s*=\\s*(.+)$" stmt)
         (declare (ignore whole))
         (let ((target (aref groups 0))
               (expr (aref groups 1)))
           (make-move-node (basic-parse-expression-from-string expr)
                          (make-identifier target)))))
      
      ;; GOSUB: GOSUB target → :perform node
      ((cl-ppcre:scan "^(?i)GOSUB\\s+(.+)$" stmt)
       (multiple-value-bind (whole groups)
           (cl-ppcre:scan-to-strings "^(?i)GOSUB\\s+(.+)$" stmt)
         (declare (ignore whole))
         (let ((target (string-trim '(#\Space #\Tab) (aref groups 0))))
           (make-perform-node target))))
      
      ;; RETURN → :goback node
      ((cl-ppcre:scan "^(?i)RETURN\\s*$" stmt)
       (make-goback-node))
      
      ;; FOR loop: FOR I = 1 TO 10 [STEP n] → :perform with :varying
      ((cl-ppcre:scan "^(?i)FOR\\s+([A-Za-z_][A-Za-z0-9_$]*)\\s*=\\s*(.+?)\\s+TO\\s+(.+?)(?:\\s+STEP\\s+(.+?))?\\s*$" stmt)
       (multiple-value-bind (whole groups)
           (cl-ppcre:scan-to-strings "^(?i)FOR\\s+([A-Za-z_][A-Za-z0-9_$]*)\\s*=\\s*(.+?)\\s+TO\\s+(.+?)(?:\\s+STEP\\s+(.+?))?\\s*$" stmt)
         (declare (ignore whole))
         (let ((var (aref groups 0))
               (start (basic-parse-expression-from-string (aref groups 1)))
               (end (basic-parse-expression-from-string (aref groups 2)))
               (step (when (aref groups 3) (basic-parse-expression-from-string (aref groups 3)))))
           (make-perform-node (format nil "FOR-~A" var)
                             :varying var
                             :from start
                             :by (or step 1)
                             :until (list :> (make-identifier var) end)))))
      
      ;; NEXT [var] → end of loop (implicit in AST structure)
      ((cl-ppcre:scan "^(?i)NEXT" stmt)
       nil)  ; NEXT doesn't generate AST in canonical form
      
      ;; IF...THEN...ELSE
      ((cl-ppcre:scan "^(?i)IF\\s+(.+?)\\s+THEN\\s+(.+?)(?:\\s+ELSE\\s+(.+?))?\\s*$" stmt)
       (multiple-value-bind (whole groups)
           (cl-ppcre:scan-to-strings "^(?i)IF\\s+(.+?)\\s+THEN\\s+(.+?)(?:\\s+ELSE\\s+(.+?))?\\s*$" stmt)
         (declare (ignore whole))
         (let ((condition (aref groups 0))
               (then-branch (aref groups 1))
               (else-branch (aref groups 2)))
           (make-if-node condition
                        (list (basic-transpile-statement then-branch))
                        (if else-branch
                            (list (basic-transpile-statement else-branch))
                            '())))))
      
      ;; PRINT statement
      ((cl-ppcre:scan "^(?i)PRINT\\s+(.*)$" stmt)
       (multiple-value-bind (whole groups)
           (cl-ppcre:scan-to-strings "^(?i)PRINT\\s+(.*)$" stmt)
         (declare (ignore whole))
         (let ((expr-list (split-sequence:split-sequence #\, (aref groups 0) :remove-empty-subseqs t)))
           (make-print-node (mapcar (lambda (e) (basic-parse-expression-from-string e))
                                   expr-list)))))
      
      ;; INPUT statement
      ((cl-ppcre:scan "^(?i)INPUT\\s+(.*)$" stmt)
       (multiple-value-bind (whole groups)
           (cl-ppcre:scan-to-strings "^(?i)INPUT\\s+(.*)$" stmt)
         (declare (ignore whole))
         (let ((var-list (split-sequence:split-sequence #\, (aref groups 0) :remove-empty-subseqs t)))
           (make-input-node (mapcar (lambda (v) (make-identifier (string-trim '(#\Space #\Tab) v)))
                                   var-list)))))
      
      ;; STOP statement
      ((cl-ppcre:scan "^(?i)STOP\\s*$" stmt)
       (make-stop-run-node))
      
      ;; Unrecognized statement
      (t nil))))

(defun basic-ast-from-source (text &key (class-id "BasicProgram"))
  "Parse BASIC source TEXT and emit canonical AST directly.
   Returns a :program AST node with methods containing the BASIC code."
  (let* ((lines (parse-basic-source-lines text))
         (statements (mapcar (lambda (pair)
                              (basic-transpile-statement (cdr pair)))
                            lines))
         (main-method (make-method-node "Main" :statements (remove nil statements))))
    (make-program-node class-id
                      :methods (list main-method)
                      :data '())))

(defun compile-basic-from-path (bas-path
                               &key (cpus '(:6502))
                                    ast-output-file)
  "Compile @code{.bas} at BAS-PATH to assembly via BASIC → AST → backend.
   
   Uses BASIC → AST compilation, directly emitting canonical AST nodes
   without COBOL transpilation.
   
   @table @asis
   @item BAS-PATH
   Pathname designator to UTF-8 BASIC source.
   @item CPUS
   List of target CPUs (default @code{(:6502)}).
   @item AST-OUTPUT-FILE
   Optional path for AST output.
   @end table
   
   @subsection Outputs
   Returns the compiled AST plist."
  (let* ((path (uiop:parse-native-namestring (namestring bas-path)))
         (text (with-open-file (in path :direction :input :element-type 'character
                                      :external-format :utf-8)
                 (with-output-to-string (out)
                   (loop for c = (read-char in nil nil)
                         while c do (write-char c out)))))
         (class-id (pathname-name path))
         (ast (basic-ast-from-source text :class-id class-id)))
    ;; Write AST to output file if requested
    (when ast-output-file
      (with-open-file (out (pathname ast-output-file)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
        (write-ast ast out)))
    ;; Compile AST directly (bypassing COBOL step)
    (compile-eightbol-from-ast ast :cpus cpus)
    ast))
