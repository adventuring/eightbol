(in-package :eightbol)

(defun split-line (line)
  "Split line into sequence number, continuation/comment marker, A zone, B zone"
  (let* ((sequence-number (and (< 6 (length line)) (subseq line 0 6)))
         (continuation-marker (and (< 7 (length line)) (char line 6))))
    (list :sequence-number sequence-number
          :continuation-marker continuation-marker
          :zone (if (or (> 12 (length line))
                        (some #'alphanumericp (subseq line 8 12)))
                    :a :b)
          :contents (and (> (length line) 8) (subseq line 7)))))

(defun continuation-line-p (line)
  (char= #\- (getf line :continuation-marker)))

(defun comment-line-p (line)
  (char= #\* (getf line :continuation-marker)))

(defun parse-keyword (token)
  "Parse keyword token"
  (list :type :keyword :value (intern (string-upcase token) :keyword)))

(defun minichar-code (char)
  (digit-char-p char 36))

(defun parse-eightbol-number (number-string)
  "Parse number token. Supports: x'9'|x'99'|x'9999'|x'9999,9999', o'...', d'...',
b'...', plain decimal. w'XXXX'|w\"XXXX\" produces string token for assembler.
Signals error on invalid digit sequences."
  (flet ((parse (string radix)
           (handler-case
               (parse-integer string :radix radix)
             (parse-error (e)
               (declare (ignore e))
               (error "Invalid number ~s for radix ~d" string radix)))))
    (cond
      ((< (length number-string) 1)
       (error "Empty number string"))
      ((and (char-equal (char number-string 0) #\d)
            (> (length number-string) 3))
       (parse (subseq number-string 2 (1- (length number-string))) 10))
      ((and (char-equal (char number-string 0) #\x)
            (> (length number-string) 3))
       (let ((digits (remove #\, (subseq number-string 2 (1- (length number-string))))))
         (parse digits 16)))
      ((and (char-equal (char number-string 0) #\o)
            (> (length number-string) 3))
       (parse (subseq number-string 2 (1- (length number-string))) 8))
      ((and (char-equal (char number-string 0) #\b)
            (> (length number-string) 3))
       (parse (subseq number-string 2 (1- (length number-string))) 2))
      ;; w'XXXX'|w"XXXX" — minifont string; assembler performs encoding. Not handled here.
      ;; Lexer produces (string "XXXX") for w format so backend emits .LogFault "XXXX".
      (t (parse number-string 10)))))

(defun parse-string (token)
  "Parse string token"
  (subseq token 1 (1- (length token))))

(defun constituent-char-p (char)
  (and (> (char-code char) #x20)
       (not (>= #x7f (char-code char) #x9f))
       (not (find char ".,'\"\\^`~|()" 
                  :test #'char-equal))))

(defun whitespace-char-p (char)
  (find char #(#\Space #\Newline #\Return #\Linefeed #\Page #\Tab)))

(defun whitespace-string-p (string)
  (every #'whitespace-char-p string))

(defun separator-punctuation-p (char)
  "True if CHAR must be emitted as its own token (never grouped with adjacent punctuation)."
  (find char "().,:;" :test #'char=))

(defun group-constituent-chars (line)
  "Group characters in line into sequences of constituent and non-constituent characters.
Separator punctuation ( ) . , : ;) is always emitted as single-char groups."
  (let (result
        current-group
        constituent-group-p
        in-string-p)
    (flet ((new-group ()
             (when current-group
               (push (reverse current-group) result)
               (setf current-group nil
                     in-string-p nil))))
      (dolist (char (coerce line 'list) result)
        (cond ((and in-string-p (char= char in-string-p))
               (push char current-group)
               (new-group))
              (in-string-p
               (push char current-group))
              ((or (char= char #\") (char= char #\apostrophe))
               (setf in-string-p char)
               (when (< 1 (length current-group))
                 (new-group))
               (push char current-group))
              ((char<= char (code-char #x20))
               (new-group))
              ((separator-punctuation-p char)
               (new-group)
               (push (list char) result)
               (setf current-group nil constituent-group-p nil))
              ((eql (if (constituent-char-p char) t nil)
                    (if constituent-group-p t nil))
               (push char current-group))
              (t (new-group)
                 (setf constituent-group-p (not constituent-group-p))
                 (push char current-group))))
      (new-group))
    (reverse (mapcar (lambda (x) (if (stringp x) x (coerce x 'string)))
                     result))))

(defun prefixed-literal-prefix-p (char)
  "True if CHAR is a prefix for a prefixed literal (x,d,o,b,w,z,u,p)."
  (member (char-downcase char) '(#\x #\d #\o #\b #\w #\z #\u #\p) :test #'char=))

(defun prefixed-string-format-p (char)
  "True if CHAR is a string-format prefix (z,u,p) — detected but unimplemented."
  (member (char-downcase char) '(#\z #\u #\p) :test #'char=))

(defun picture-sequence-p (token-str)
  "True if TOKEN-STR (by string value) can be interpreted as a COBOL picture sequence.
Used when after PIC/PICTURE: treat as picture-sequence instead of expanding as subscript."
  (and (stringp token-str)
       (plusp (length token-str))
       (let ((c0 (char token-str 0)))
         (or (digit-char-p c0)
             (find c0 "Xx9AaBbVvPpSsZzNn") ; common PIC type chars
             (char= c0 #\.)))
       (not (position #\: token-str)))) ; exclude ref-mod (start:length)

(defun merge-prefixed-literals (tokens)
  "Merge prefix letter + quoted string into single token. E.g. (\"b\" \"'1010'\") -> (\"b'1010'\")."
  (let (result)
    (loop while tokens do
      (let ((tok (first tokens))
            (nxt (cadr tokens)))
        (cond
          ((and nxt
                (= 1 (length tok))
                (prefixed-literal-prefix-p (char tok 0))
                (>= (length nxt) 2)
                (member (char nxt 0) '(#\' #\") :test #'char=)
                (char= (char nxt 0) (char nxt (1- (length nxt)))))
           (push (concatenate 'string tok nxt) result)
           (setf tokens (cddr tokens)))
          (t
           (push tok result)
           (setf tokens (rest tokens))))))
    (nreverse result)))

(defun expand-refmod-in-parens (token-str meta)
  "When TOKEN-STR is start:length (e.g. 1:64) after '(', expand to number : number tokens."
  (let ((colon-pos (position #\: token-str)))
    (when colon-pos
      (let* ((left-str  (string-trim " " (subseq token-str 0 colon-pos)))
             (right-str (string-trim " " (subseq token-str (1+ colon-pos))))
             (left-tok  (if (every #'digit-char-p left-str)
                            (list 'number (parse-eightbol-number left-str))
                            (list 'symbol left-str)))
             (right-tok (if (every #'digit-char-p right-str)
                             (list 'number (parse-eightbol-number right-str))
                             (list 'symbol right-str))))
        (list (append left-tok meta)
              (append (list '|:| ":") meta)
              (append right-tok meta))))))

(defun expand-subscripted-symbol (token-str meta)
  "If TOKEN-STR is name(subscript) or name(start:length) with letters in name,
   return list of tokens. Keeps PIC 9(2) etc. intact (no letters before '(')."
  (let ((lp (position #\( token-str))
        (rp (position #\) token-str)))
    (when (and lp rp (< lp rp)
               (some #'alpha-char-p (subseq token-str 0 lp)))
      (let ((name (subseq token-str 0 lp))
            (sub  (subseq token-str (1+ lp) rp)))
        (let ((colon-pos (position #\: sub)))
          (if colon-pos
              ;; Reference modification: name(start:length)
              (let* ((left-str  (string-trim " " (subseq sub 0 colon-pos)))
                     (right-str (string-trim " " (subseq sub (1+ colon-pos))))
                     (left-tok  (if (every #'digit-char-p left-str)
                                    (list 'number (parse-eightbol-number left-str))
                                    (list 'symbol left-str)))
                     (right-tok (if (every #'digit-char-p right-str)
                                    (list 'number (parse-eightbol-number right-str))
                                    (list 'symbol right-str))))
                (list (append (list 'symbol name) meta)
                      (append (list '|(| "(") meta)
                      (append left-tok meta)
                      (append (list '|:| ":") meta)
                      (append right-tok meta)
                      (append (list '|)| ")") meta))))
              ;; Subscript: name(subscript) — emit number when subscript is all digits (e.g. X(64))
              (list (append (list 'symbol name) meta)
                    (append (list '|(| "(") meta)
                    (append (if (every #'digit-char-p sub)
                                (list 'number (parse-eightbol-number sub))
                                (list 'symbol sub))
                            meta)
                    (append (list '|)| ")") meta)))))))

(defun tokenize-line (line)
  "Tokenize line based on current lexing context"
  (let* (result
         (tokens (merge-prefixed-literals (group-constituent-chars line)))
         (line-text (string-trim '(#\Space #\Tab #\Page #\Return)
                                 (if (stringp line) line (coerce line 'string)))))
    (dolist (token tokens (nreverse result))
      (let ((meta (list :source-file *source-file-pathname*
                        :source-line *source-line-number*
                        :source-sequence *source-sequence-number*
                        :zone *line-zone-start*
                        :source-line-text line-text))
            (prev-was-pic (and result
                               (let ((last (first result)))
                                 (member (first last) '(eightbol::pic eightbol::picture)))))
            (prev-was-open-paren (and result
                                      (let ((last (first result)))
                                        (eq (first last) '|(|)))))
        (dolist (tok (or (when (and (every #'constituent-char-p token)
                                    (stringp token))
                           (cond
                             ((and prev-was-pic (picture-sequence-p token))
                              (list (append (list 'picture-sequence token) meta)))
                             ((and prev-was-open-paren (position #\: token))
                              (expand-refmod-in-parens (coerce token 'string) meta))
                             (t (expand-subscripted-symbol (coerce token 'string) meta))))
                         (list (append (cond
                                         ((and prev-was-pic (picture-sequence-p token))
                                          (list 'picture-sequence token))
                                         ((member (string token) (mapcar #'string (token-list))
                                                  :test #'string-equal)
                                          (list (intern (string-upcase token) :eightbol) token))
                                         ((every #'digit-char-p token)
                                          (list 'number (parse-eightbol-number (coerce token 'string))))
                                         ((and (< 2 (length token)) (or (char= (char token 0) #\")
                                                                        (char= (char token 0) #\apostrophe))
                                               (char= (char token 0) (char token (1- (length token))))) 
                                          (list 'string (parse-string (coerce token 'string))))
                                         ((and (> (length token) 3)
                                               (prefixed-string-format-p (char token 0))
                                               (or (char= (char token 1) #\") (char= (char token 1) #\apostrophe))
                                               (char= (char token 1) (char token (1- (length token)))))
                                          (error "EIGHTBOL: prefixed string literal ~s (z/u/p) is not yet implemented"
                                                 token))
                                         ((and (> (length token) 3)
                                               (char-equal (char token 0) #\w)
                                               (or (char= (char token 1) #\") (char= (char token 1) #\apostrophe))
                                               (char= (char token 1) (char token (1- (length token)))))
                                          ;; w"XXXX" — pass string to assembler for minifont encoding
                                          (list 'string (subseq token 2 (1- (length token)))))
                                         ((and (> (length token) 3)
                                               (member (char token 0) '(#\d #\o #\b #\x) :test #'char-equal) 
                                               (or (char= (char token 1) #\") (char= (char token 1) #\apostrophe))
                                               (char= (char token 1) (char token (1- (length token)))))
                                          (list 'number (parse-eightbol-number token)))
                                         ((string= token ".")
                                          (list '|.| "."))
                                         ((every #'constituent-char-p token)
                                          (list 'symbol (coerce token 'string)))
                                         (t (list 'bareword token)))
                                       meta))))
          (format *trace-output* "~& // ~s" tok)
          (push tok result))))))

(defvar *copybook-paths* nil
  "Directories in which to search for copybooks")

(defvar *copybook-dependencies* nil
  "When bound to a list, expand-copy-tokens pushes each resolved copybook path
(truename) onto it. Used by compile-eightbol-class to emit Makefile .d files.")

(defvar *source-file-pathname* nil
  "Current input file being processed")

(defvar *source-line-number* nil
  "Current input file line being processed")

(defvar *source-sequence-number* nil
  "Current input file line source sequence “number” being processed.")

(defvar *line-zone-start* nil)

(defun lex-file (input-file)
  "Lex INPUT-FILE, returning a flat token list (no COPY expansion)."
  (let ((*source-file-pathname* (pathname-name input-file)))
    (with-open-file (stream input-file :direction :input)
      (lexer stream))))

(defun ensure-directory-pathname (p)
  "Coerce P to a directory pathname (trailing slash).  Accepts strings and pathnames."
  (let* ((pn (if (pathnamep p) p (pathname p)))
         (name (pathname-name pn))
         (type (pathname-type pn)))
    ;; If  the pathname  has  a  name/type component  it  was parsed  as
    ;; a file. Re-parse with a  trailing slash so merge-pathnames treats
    ;; it as a dir.
    (if (and (or name type)
             (not (equal name :unspecific))
             (not (null name)))
        (pathname (concatenate 'string (namestring pn) "/"))
        pn)))

(defun valid-copybook-name-p (name)
  "True if NAME is safe for use as a copybook identifier (no path traversal)."
  (and (stringp name)
       (plusp (length name))
       (every (lambda (ch)
                (or (char= ch #\-)
                    (alphanumericp ch)))
              name)
       (alpha-char-p (char name 0))
       (not (char= (last-elt name) #\-))
       (not (search "--" name))))

(defun find-copybook (name &optional library)
  "Search *COPYBOOK-PATHS* for NAME.cpy; return pathname or NIL.
When LIBRARY is specified (from COPY Name OF Library or COPY Name IN Library),
search first in {path}/{library}/ subdirectory, then in {path}/.
Rejects names containing path separators or leading dot.
COBOL is case-insensitive; if exact NAME.cpy is not found, tries case-insensitive
match against files in the directory (so COPY Basic-NPC-Slots finds Basic-Npc-Slots.cpy)."
  (unless (valid-copybook-name-p name)
    (error "Invalid COPY name ~s: must be Hyphenated-Alphanumeric" name))
  (when library
    (unless (valid-copybook-name-p (string library))
      (error "Invalid COPY library ~s: must be Hyphenamed-Alphanumeric" library)))
  (flet ((try-exact (dir-pn)
           (let ((path (merge-pathnames
                        (make-pathname :name name :type "cpy")
                        dir-pn)))
             (when (probe-file path) (return-from find-copybook path))))
         (try-case-insensitive (dir-pn)
           (let ((want (concatenate 'string name ".cpy")))
             (dolist (ent (directory (merge-pathnames
                                     (make-pathname :name :wild :type "cpy")
                                     dir-pn)))
               (when (string-equal (file-namestring ent) want)
                 (return-from find-copybook ent))))))
    (dolist (dir *copybook-paths*)
      (let ((dir-pn (ensure-directory-pathname dir)))
        (if library
            (let ((lib-dir (merge-pathnames
                            (make-pathname :directory `(:relative ,library))
                            dir-pn)))
              (try-exact lib-dir)
              (try-case-insensitive lib-dir))
            (progn
              (try-exact dir-pn)
              (try-case-insensitive dir-pn))))))
  nil)

(defun lex-with-copy-expansion (stream)
  "Lex STREAM, expanding any COPY tokens by inlining the referenced copybook.
   Returns a flat token list ready for the YACC parser."
  (let ((tokens (lexer stream)))
    (expand-copy-tokens tokens)))

(defun expand-copy-tokens (tokens)
  "Walk TOKENS; when a (COPY ...) sequence is found, replace it with the
   lexed contents of the referenced copybook file."
  (let (result)
    (loop while tokens
          for tok = (pop tokens)
          do
             (cond
               ;; COPY Name .  — expand the copybook inline
               ;; COPY Name OF Library .  or  COPY Name IN Library .
               ((and (listp tok) (eq (first tok) 'copy))
                (let* ((next (pop tokens))
                       (name (if (and (listp next)
                                      (member (first next) '(symbol bareword)))
                                 (second next)
                                 (progn (push next tokens) nil)))
                       ;; consume optional OF/IN library and the full stop
                       (after (pop tokens))
                       library)
                  (when (and (listp after)
                             (member (first after) '(of in)))
                    (let ((lib-tok (pop tokens)))
                      (setf library (and (listp lib-tok)
                                         (member (first lib-tok) '(symbol bareword))
                                         (second lib-tok)))
                      (setf after (pop tokens))))
                  (unless (and (listp after) (eq (first after) '|.|))
                    (push after tokens))
                  (if name
                      (let ((path (find-copybook name library)))
                        (if path
                            (progn
                              (when (boundp '*copybook-dependencies*)
                                (pushnew (truename path) *copybook-dependencies*
                                         :test #'equalp))
                              (let ((cb-tokens (lex-file path)))
                                (setf tokens (append (expand-copy-tokens cb-tokens) tokens))))
                            (error 'copybook-not-found
                                   :message "cannot find copybook; COPY is required"
                                   :copybook-name name
                                   :library library)))
                      (error "EIGHTBOL: COPY without a name"))))
               (t (push tok result))))
    (nreverse result)))

(defun lexer (stream)
  (loop for *source-line-number* from 1
        with body
        with last-source-sequence-number
        with last-line-zone-start = :a
        for line = (read-line stream nil nil)
        while line
        do (let* ((parsed-line (split-line line))
                  (*source-sequence-number*
                    (let ((s (when (getf parsed-line :sequence-number)
                               (string-trim #(#\Space #\Tab)
                                            (getf parsed-line :sequence-number)))))
                      (when (plusp (length s))
                        s)))
                  (*line-zone-start* (getf parsed-line :zone)))
             (when *source-sequence-number*
               (when (and last-source-sequence-number
                          (not (string-greaterp *source-sequence-number*
                                                last-source-sequence-number)))
                 (warn "EIGHTBOL: sequence number ~s (line ~:d) is ~
not greater than previous ~s — must be strictly increasing"
                       *source-sequence-number* *source-line-number* last-source-sequence-number))
               (setf last-source-sequence-number *source-sequence-number*))
             (when *line-zone-start*
               (unless (eql last-line-zone-start *line-zone-start*)
                 (setf last-line-zone-start *line-zone-start*)
                 (ecase *line-zone-start*
                   (:a (appendf body '((outdent))))
                   (:b (appendf body '((indent)))))))
             (cond
               ((null (getf parsed-line :contents))
                nil)
               ((zerop (length (string-trim #(#\Space #\Tab #\Page)
                                            (getf parsed-line :contents))))
                nil)
               ((continuation-line-p parsed-line)
                (mapcar (lambda (token) (appendf body (cons token nil)))
                        (tokenize-line (getf parsed-line :contents))))
               ((comment-line-p parsed-line)
                (appendf body (cons (list* 'comment (getf parsed-line :contents) parsed-line) nil)))
               (t
                (mapcar (lambda (token) (appendf body (cons token nil)))
                        (tokenize-line (getf parsed-line :contents))))))
        finally (return (remove-if (lambda (tok)
                                      ;; INDENT/OUTDENT zone markers were never
                                      ;; integrated into the grammar; strip them
                                      ;; before handing tokens to the YACC parser.
                                      (member (first tok) '(eightbol::indent eightbol::outdent)))
                                   body))))

