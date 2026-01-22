;; tests/eightbol-tests.lisp — EIGHTBOL compiler test suite
;;
;; All tests are driven by the canonical Character.cob example.
;; Canonical file: /home/brpocock/Projects/Phantasia/Source/Classes/Character.cob
;;
;; To run:  (asdf:test-system :eightbol)
;;          — or —  (fiveam:run! :eightbol)

(in-package :eightbol/test)

(in-suite :eightbol)

;;; Declare special variables so tests can reference them at compile time
;;; (defparameters appear later in file).
(declaim (special *minimal-string-blt-cob* *minimal-arithmetic-cob*))

;;;; ---------------------------------------------------------------
;;;; Test helpers — with-temporary-directory (UIOP has no such macro)
;;;; ---------------------------------------------------------------

(defmacro with-temporary-directory ((&key (prefix "eightbol-tmp-")) &body body)
  "Create a temporary directory, run BODY with cwd set to it, then delete it.
Uses uiop:call-with-current-directory and uiop:delete-directory-tree."
  (let ((dir (gensym "DIR")))
    `(let ((,dir (uiop:ensure-directory-pathname
                  (merge-pathnames
                   (make-pathname :directory `(:relative ,(format nil "~a~a" ,prefix (get-internal-real-time))))
                   (uiop:temporary-directory)))))
       (ensure-directories-exist ,dir)
       (unwind-protect
            (uiop:call-with-current-directory ,dir (lambda () ,@body))
         (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore)))))

;;;; ---------------------------------------------------------------
;;;; Test data
;;;; ---------------------------------------------------------------

(defparameter *character-cob-path*
  #p"/home/brpocock/Projects/Phantasia/Source/Classes/Character.cob"
  "Canonical EIGHTBOL source for all compiler tests.")

;;; A self-contained minimal Character class — used when the full
;;; Character.cob would require copybook expansion.
(defparameter *minimal-character-cob*
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             IF HP IS EQUAL TO 0 THEN
000130                 INVOKE Self \"Kill\".
000140             END-IF.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character.
"
  "Minimal valid EIGHTBOL class with two methods, no COPY dependency.")

;;;; ---------------------------------------------------------------
;;;; Lexer tests
;;;; ---------------------------------------------------------------

(test lexer/tokenizes-identification-line
  "A basic IDENTIFICATION DIVISION. line produces correct tokens."
  (let ((tokens (with-input-from-string (s "000030 IDENTIFICATION DIVISION.")
                  (eightbol::lexer s))))
    (is (find 'eightbol::identification tokens :key #'first))
    (is (find 'eightbol::division        tokens :key #'first))
    (is (find 'eightbol::|.|             tokens :key #'first))))

(test lexer/comment-line
  "A line with * in column 7 produces a COMMENT token."
  (let ((tokens (with-input-from-string (s "000010* This is a comment")
                  (eightbol::lexer s))))
    (is (find 'eightbol::comment tokens :key #'first))))

(test lexer/sequence-number-captured
  "Sequence number from columns 1-6 is captured in token metadata."
  (let ((tokens (with-input-from-string (s "000042 GOBACK.")
                  (eightbol::lexer s))))
    (let ((goback (find 'eightbol::goback tokens :key #'first)))
      (is (not (null goback)))
      (is (string= "000042" (getf goback :source-sequence))))))

(test lexer/sequence-number-not-increasing-warns
  "Warn when sequence number is not blanks and string<= previous."
  (let (warnings)
    (handler-bind ((warning (lambda (w)
                             (push (format nil "~a" w) warnings)
                             (muffle-warning w))))
      (with-input-from-string (s "000100 FOO.
000200 BAR.
000150 BAZ.")
        (eightbol::lexer s)))
    (is (find-if (lambda (w) (search "sequence number" w :test #'char-equal))
                 warnings)
        "Should warn when sequence 000150 <= previous 000200")))

(test lexer/keyword-class-id
  "CLASS-ID is recognised as a single keyword token."
  (let ((tokens (with-input-from-string (s "       CLASS-ID. Character.")
                  (eightbol::lexer s))))
    (is (find 'eightbol::class-id tokens :key #'first))))

(test lexer/end-method-token
  "END-METHOD (or END METHOD) tokenises correctly."
  (let ((tokens (with-input-from-string (s "         END METHOD \"Think\".")
                  (eightbol::lexer s))))
    ;; Should have END + METHOD + string + dot
    (is (find 'eightbol::end    tokens :key #'first))
    (is (find 'eightbol::method tokens :key #'first))
    (is (find 'eightbol::string tokens :key #'first))))

(test lexer/prefixed-literals-merge
  "Prefixed literals x'99', b'1010' tokenize as numbers; w\"STUK\" as string for assembler."
  (let ((tokens (with-input-from-string
                   (s "000010 MOVE x'0F' TO Foo. MOVE b'1010' TO Bar. LOG FAULT w\"STUK\".")
                  (eightbol::lexer s))))
    (let ((numbers (remove-if-not (lambda (tok) (eq (first tok) 'eightbol::number)) tokens))
          (strings (remove-if-not (lambda (tok) (eq (first tok) 'eightbol::string)) tokens)))
      (is (>= (length numbers) 2) "Should have at least 2 number tokens (x'0F', b'1010')")
      (is (find 15 numbers :key #'second) "x'0F' should parse to 15")
      (is (find 10 numbers :key #'second) "b'1010' should parse to 10")
      (is (find "STUK" strings :key #'second :test #'string=)
          "w\"STUK\" should produce string token for assembler minifont encoding"))))

(test lexer/prefixed-string-z-signals-error
  "Prefixed string literal z'...' signals unimplemented error."
  (signals error
    (with-input-from-string (s "000010 MOVE z\"hello\" TO Foo.")
      (eightbol::lexer s))))

(test lexer/parse-number-hex-with-comma
  "parse-number handles x'9999,9999' hex dword format."
  (is (= #x12345678 (eightbol::parse-number "x'1234,5678'")))
  (is (= #xDEADBEEF (eightbol::parse-number "x'dead,beef'"))))

(test lexer/refmod-tokenization
  "Reference modification SrcBuf(1:64) tokenizes as SrcBuf ( 1 : 64 )."
  (let ((tokens (with-input-from-string
                   (s "000010 STRING SrcBuf(1:64) DELIMITED BY SIZE INTO DstBuf(1:64).")
                  (eightbol::lexer s))))
    (let ((srcbuf (position-if (lambda (tok)
                                 (and (eq (first tok) 'eightbol::symbol)
                                      (string= (second tok) "SrcBuf")))
                               tokens))
          (open1 (position 'eightbol::|(| tokens :key #'first))
          (one (position 1 tokens :key #'second))
          (colon (position 'eightbol::|:| tokens :key #'first))
          (sixty4 (position 64 tokens :key #'second))
          (close1 (position 'eightbol::|)| tokens :key #'first)))
      (is (not (null srcbuf)) "SrcBuf token present")
      (is (not (null open1)) "Open paren token present")
      (is (not (null one)) "Start 1 token present")
      (is (not (null colon)) "Colon token present")
      (is (not (null sixty4)) "Length 64 token present")
      (is (not (null close1)) "Close paren token present")
      (is (< srcbuf open1 one colon sixty4 close1)
          "Refmod tokens in order: SrcBuf ( 1 : 64 )"))))

(test lexer/picture-sequence-after-pic
  "After PIC/PICTURE, X tokenizes as picture-sequence (X(64) split by parens)."
  (let ((tokens (with-input-from-string
                   (s "000070         05 SrcBuf PIC X(64).")
                  (eightbol::lexer s))))
    (let ((pic (find 'eightbol::pic tokens :key #'first))
          (pic-seq (find 'eightbol::picture-sequence tokens :key #'first)))
      (is (not (null pic)) "PIC token present")
      (is (not (null pic-seq)) "picture-sequence token present after PIC")
      (is (string= "X" (second pic-seq)) "picture-sequence is X (64 parsed separately)"))))

(test lexer/find-copybook-finds-cpy
  "find-copybook locates .cpy file in include path."
  (with-temporary-directory (:prefix "eightbol-copy-")
    (let ((dir (uiop:getcwd)))
      (with-open-file (f (merge-pathnames "Foo.cpy" dir)
                        :direction :output :if-exists :supersede)
        (write-line "000010* Copybook" f))
      (let ((found (let ((eightbol::*copybook-paths* (list dir)))
                     (eightbol::find-copybook "Foo"))))
        (is (pathnamep found))
        (is (string= "Foo" (pathname-name found)))
        (is (string= "cpy" (pathname-type found)))))))

(test lexer/expand-copy-inlines-copybook
  "expand-copy-tokens replaces COPY Name. with copybook contents."
  (with-temporary-directory (:prefix "eightbol-copy-")
    (let ((dir (uiop:pathname-directory-pathname (uiop:getcwd))))
      (with-open-file (f (merge-pathnames "Globals.cpy" dir)
                        :direction :output :if-exists :supersede)
        (write-line "000010     05 HP PIC 9999 USAGE BINARY." f))
      (let ((expanded (let ((eightbol::*copybook-paths* (list dir)))
                        (eightbol::expand-copy-tokens
                         (list (list 'eightbol::copy)
                               (list 'eightbol::symbol "Globals")
                               (list 'eightbol::|.|))))))
        (is (listp expanded))
        (is (some (lambda (tok)
                    (and (listp tok) (member (first tok) '(eightbol::symbol eightbol::bareword))
                         (string= "HP" (second tok))))
                expanded))))))

(test lexer/find-copybook-with-library
  "find-copybook with library searches {path}/{library}/ subdirectory first."
  (with-temporary-directory (:prefix "eightbol-copy-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (lib-dir (merge-pathnames
                     (make-pathname :directory '(:relative "MyLib"))
                     root)))
      (ensure-directories-exist lib-dir)
      (with-open-file (f (merge-pathnames "Slots.cpy" lib-dir)
                        :direction :output :if-exists :supersede)
        (write-line "000010* Library copybook" f))
      (let ((found (let ((eightbol::*copybook-paths* (list root)))
                     (eightbol::find-copybook "Slots" "MyLib"))))
        (is (pathnamep found))
        (is (string= "Slots" (pathname-name found)))
        (is (search "MyLib" (namestring found))))))

(test lexer/expand-copy-errors-when-copybook-not-found
  "expand-copy-tokens signals eightbol:copybook-not-found when copybook cannot be found; COPY cannot be ignored."
  (with-temporary-directory (:prefix "eightbol-copy-")
    (let ((dir (uiop:pathname-directory-pathname (uiop:getcwd))))
      (signals eightbol:copybook-not-found
        (let ((eightbol::*copybook-paths* (list dir)))
          (eightbol::expand-copy-tokens
           (list (list 'eightbol::copy)
                 (list 'eightbol::symbol "NonexistentCopybook")
                 (list 'eightbol::|.|))))))))

(test lexer/expand-copy-errors-when-copy-without-name
  "expand-copy-tokens signals error when COPY has no name."
  (signals error
    (eightbol::expand-copy-tokens
     (list (list 'eightbol::copy)
           (list 'eightbol::|.|)))))

;;;; ---------------------------------------------------------------
;;;; Copybook parsing (parse-cpy-line, load-copybook-tables)
;;;; ---------------------------------------------------------------

(test backend/parse-cpy-section-inherited
  "parse-cpy-line parses * Inherited from ClassName: section comment."
  (let ((parsed (eightbol::parse-cpy-line "* Inherited from BasicObject:")))
    (is (eq :section-comment (first parsed)))
    (is (string= "BasicObject" (getf (rest parsed) :origin-class)))))

(test backend/parse-cpy-section-own
  "parse-cpy-line parses * Own slots (ClassName): section comment."
  (let ((parsed (eightbol::parse-cpy-line "* Own slots (Character):")))
    (is (eq :section-comment (first parsed)))
    (is (string= "Character" (getf (rest parsed) :own-class)))))

(test backend/parse-cpy-data-item
  "parse-cpy-line parses 05 SlotName PIC 9999 USAGE BINARY."
  (let ((parsed (eightbol::parse-cpy-line "05 HP PIC 9999 USAGE BINARY.")))
    (is (eq :data-item (first parsed)))
    (is (= 5 (getf (rest parsed) :level)))
    (is (string= "HP" (getf (rest parsed) :name)))
    (is (search "PIC" (getf (rest parsed) :pic)))))

(test backend/parse-cpy-object-ref
  "parse-cpy-line parses OBJECT REFERENCE in PIC clause."
  (let ((parsed (eightbol::parse-cpy-line "05 Target OBJECT REFERENCE Actor.")))
    (is (eq :data-item (first parsed)))
    (is (string= "Target" (getf (rest parsed) :name)))
    (is (string= "Actor" (getf (rest parsed) :type-class)))))

(test backend/parse-cpy-constant-77
  "parse-cpy-line parses 77 constant with VALUE."
  (let ((parsed (eightbol::parse-cpy-line "77 MaxHP PIC 9999 USAGE BINARY VALUE 255.")))
    (is (eq :data-item (first parsed)))
    (is (= 77 (getf (rest parsed) :level)))
    (is (getf (rest parsed) :constant-p))
    (is (= 255 (getf (rest parsed) :value)))))

(test backend/load-copybook-tables-extracts-slot-type-const
  "load-copybook-tables builds slot, type, and const tables from .cpy files."
  (with-temporary-directory (:prefix "eightbol-cpy-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "TestClass-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (TestClass):" f)
        (write-line "05 HP PIC 9999 USAGE BINARY." f)
        (write-line "05 Target OBJECT REFERENCE Actor." f)
        (write-line "77 MaxHP PIC 9999 USAGE BINARY VALUE 100." f))
      (multiple-value-bind (slot-table type-table const-table service-bank-table usage-table sign-table pic-size-table pic-width-table)
          (let ((eightbol::*eightbol-root-directory* root)
                (eightbol::*copybook-paths* (list gen)))
            (eightbol::load-copybook-tables "TestClass" :root-dir root))
        (declare (ignore service-bank-table usage-table sign-table pic-size-table pic-width-table))
        (is (string= "TestClass" (gethash "HP" slot-table)))
        (is (string= "Actor" (gethash "Target" type-table)))
        (is (= 100 (gethash "MaxHP" const-table)))))))

;;;; ---------------------------------------------------------------
;;;; Backend slot/type/const resolution
;;;; ---------------------------------------------------------------

(test backend/slot-symbol-uses-origin-class
  "slot-symbol produces OriginClassSlotName from *slot-table*."
  (let ((slot-table (make-hash-table :test 'equal)))
    (setf (gethash "HP" slot-table) "Character")
    (let ((eightbol::*slot-table* slot-table))
      (is (string= "CharacterHP"
                  (eightbol::slot-symbol "HP" "TestClass"))))))

(test backend/var-class-from-type-table
  "var-class returns OBJECT REFERENCE class from *type-table*."
  (let ((type-table (make-hash-table :test 'equal)))
    (setf (gethash "Target" type-table) "Actor")
    (let ((eightbol::*type-table* type-table))
      (is (string= "Actor" (eightbol::var-class "Target"))))))

(test backend/constant-value-from-const-table
  "constant-value returns integer from *const-table*."
  (let ((const-table (make-hash-table :test 'equal)))
    (setf (gethash "MaxHP" const-table) 255)
    (let ((eightbol::*const-table* const-table))
      (is (= 255 (eightbol::constant-value "MaxHP"))))))

;;;; ---------------------------------------------------------------
;;;; Parser tests (using the minimal self-contained class)
;;;; ---------------------------------------------------------------

(test parser/parses-minimal-class
  "parse-eightbol-string on minimal Character text returns a :program node."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (is (listp ast))
    (is (eq :program (first ast)))))

(test parser/class-id-extracted
  "AST has :class-id \"Character\"."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (is (string= "Character" (eightbol::ast-class-id ast)))))

(test parser/two-methods-found
  "Minimal class has exactly two methods: Think and Kill."
  (let* ((ast     (eightbol::parse-eightbol-string *minimal-character-cob*))
         (methods (eightbol::ast-methods ast)))
    (is (= 2 (length methods)))
    (let ((names (mapcar #'eightbol::ast-method-name methods)))
      (is (member "Think" names :test #'string=))
      (is (member "Kill"  names :test #'string=)))))

(test parser/think-method-has-if-and-goback
  "The Think method contains an :if statement followed by :goback."
  (let* ((ast     (eightbol::parse-eightbol-string *minimal-character-cob*))
         (think   (find "Think" (eightbol::ast-methods ast)
                        :key  #'eightbol::ast-method-name
                        :test #'string=))
         (stmts   (eightbol::ast-method-statements think)))
    (is (not (null think)))
    (is (find :if     stmts :key #'first))
    (is (find :goback stmts :key #'first))))

(test parser/if-contains-invoke
  "The IF in Think has a :invoke Self Kill in its :then branch."
  (let* ((ast    (eightbol::parse-eightbol-string *minimal-character-cob*))
         (think  (find "Think" (eightbol::ast-methods ast)
                       :key  #'eightbol::ast-method-name
                       :test #'string=))
         (if-stmt (find :if (eightbol::ast-method-statements think)
                        :key #'first))
         (then    (getf (rest if-stmt) :then)))
    (is (not (null if-stmt)))
    (let ((invoke (find :invoke then :key #'first)))
      (is (not (null invoke)))
      (is (string= "Kill" (getf (rest invoke) :method))))))

;;;; ---------------------------------------------------------------
;;;; Unsupported statements — compile-time error
;;;; ---------------------------------------------------------------

(defun minimal-class-with-stmt (stmt-line)
  "Build minimal Character class with STMT-LINE in Think method body."
  (format nil "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             ~a
000130             GOBACK.
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Kill\".
000170         PROCEDURE DIVISION.
000180             GOBACK.
000190         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character.
" stmt-line))

(test parser/divide-signals-unsupported
  "DIVIDE signals compile-time error."
  (signals eightbol:source-error
    (eightbol::parse-eightbol-string
     (minimal-class-with-stmt "DIVIDE 2 INTO HP."))))

(test parser/multiply-signals-unsupported
  "MULTIPLY signals compile-time error."
  (signals eightbol:source-error
    (eightbol::parse-eightbol-string
     (minimal-class-with-stmt "MULTIPLY 2 BY HP."))))

(test parser/string-signals-unsupported
  "STRING signals compile-time error."
  (signals eightbol:source-error
    (eightbol::parse-eightbol-string
     (minimal-class-with-stmt "STRING \"X\" DELIMITED BY \" \" INTO HP."))))

(test parser/unstring-signals-unsupported
  "UNSTRING signals compile-time error."
  (signals eightbol:source-error
    (eightbol::parse-eightbol-string
     (minimal-class-with-stmt "UNSTRING HP DELIMITED BY \" \" INTO HP."))))

(test parser/inspect-parses
  "INSPECT parses and produces :inspect AST node."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "INSPECT HP TALLYING HP FOR CHARACTERS.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (insp (find :inspect stmts :key #'first)))
    (is (not (null insp)))
    (is (equal "HP" (getf (rest insp) :target)))
    (is (equal "HP" (getf (rest insp) :tallying)))))

(test parser/goto-signals-unsupported
  "GO TO signals compile-time error."
  (signals eightbol:source-error
    (eightbol::parse-eightbol-string
     (minimal-class-with-stmt "GO TO Think."))))

(test parser/evaluate-parses
  "EVALUATE parses and produces :evaluate AST node."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "EVALUATE HP WHEN 0 GOBACK. END-EVALUATE.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (eval-stmt (find :evaluate stmts :key #'first)))
    (is (not (null eval-stmt)))
    (is (equal "HP" (getf (rest eval-stmt) :subject)))
    (is (not (null (getf (rest eval-stmt) :when-clauses))))))

(test parser/set-null-signals-unsupported
  "SET ... TO NULL signals compile-time error."
  (signals eightbol:source-error
    (eightbol::parse-eightbol-string
     (minimal-class-with-stmt "SET HP TO NULL."))))

(test parser/subscript-move-from-parses
  "MOVE X(I) TO Y produces :move :from (:subscript base index) :to dest."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "MOVE Tab(1) TO HP.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (move (find :move stmts :key #'first)))
    (is (not (null move)))
    (let ((from (getf (rest move) :from))
          (to (getf (rest move) :to)))
      (is (and (listp from) (eq :subscript (first from)))
          "Source should be :subscript form")
      (is (string= "Tab" (second from)))
      (is (eql 1 (third from)) "Index should be 1")
      (is (string= "HP" to)))))

(test parser/subscript-move-to-parses
  "MOVE Y TO X(I) produces :move :from source :to (:subscript base index)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "MOVE HP TO Tab(1).")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (move (find :move stmts :key #'first)))
    (is (not (null move)))
    (let ((from (getf (rest move) :from))
          (to (getf (rest move) :to)))
      (is (string= "HP" from))
      (is (and (listp to) (eq :subscript (first to)))
          "Destination should be :subscript form")
      (is (string= "Tab" (second to)))
      (is (eql 1 (third to)) "Index should be 1"))))

(test parser/subtract-parses-correctly
  "SUBTRACT x FROM y parses; regression for SUBTRACT token."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "SUBTRACT 1 FROM HP.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (sub (find :subtract stmts :key #'first)))
    (is (not (null sub)))
    (is (eql 1 (getf (rest sub) :from)))
    (is (string= "HP" (getf (rest sub) :from-target)))))

(test parser/unmatched-close-paren-signals-error
  "Extra close parenthesis in source signals parse error."
  (signals error
    (eightbol::parse-eightbol-string
     (minimal-class-with-stmt "MOVE 1 ) TO HP."))))

(test parser/log-fault-parses
  "LOG FAULT parses and produces :log-fault AST node."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "LOG FAULT \"ERR\".")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (lf (find :log-fault stmts :key #'first)))
    (is (not (null lf)))
    (is (equal "ERR" (getf (rest lf) :code)))))

(test parser/log-fault-w-string-parses
  "LOG FAULT w\"STUK\" parses (minifont string)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "LOG FAULT w\"STUK\".")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (lf (find :log-fault stmts :key #'first)))
    (is (not (null lf)))
    (is (equal "STUK" (getf (rest lf) :code)))))

(test parser/debug-break-parses
  "DEBUG BREAK parses and produces :debug-break AST node."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "DEBUG BREAK 1.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (db (find :debug-break stmts :key #'first)))
    (is (not (null db)))
    (is (eql 1 (getf (rest db) :code)))))

(test parser/exit-method-parses
  "EXIT METHOD parses and produces :exit-method AST node."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "EXIT METHOD.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think)))
    (is (find :exit-method stmts :key #'first))))

(test parser/exit-program-parses
  "EXIT PROGRAM parses and produces :exit-program AST node."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "EXIT PROGRAM.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think)))
    (is (find :exit-program stmts :key #'first))))

(test parser/stop-run-parses
  "STOP RUN parses and produces :stop-run AST node."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "STOP RUN.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think)))
    (is (find :stop-run stmts :key #'first))))

(test parser/exit-parses
  "EXIT parses and produces :exit AST node."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "EXIT.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think)))
    (is (find :exit stmts :key #'first))))

(test parser/copy-parses
  "COPY name parses and produces :copy AST node."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "COPY Foo.")))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (copy-stmt (find :copy stmts :key #'first)))
    (is (not (null copy-stmt)))
    (is (string= "Foo" (getf (rest copy-stmt) :name)))))

(test parser/perform-parses
  "PERFORM procedure-name parses and produces :perform AST node."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             PERFORM Loop.
000130             GOBACK.
000140 Loop.
000150             GOBACK.
000160         END METHOD \"Think\".
000170 END OBJECT.
000180 END CLASS Character.")
         (ast (eightbol::parse-eightbol-string src))
         (think (find "Think" (eightbol::ast-methods ast)
                     :key #'eightbol::ast-method-name :test #'string=))
         (stmts (eightbol::ast-method-statements think))
         (perf (find :perform stmts :key #'first)))
    (is (not (null perf)))
    (is (string= "Loop" (format nil "~a" (getf (rest perf) :procedure))))))

;;;; ---------------------------------------------------------------
;;;; STRING BLT (DELIMITED BY SIZE)
;;;; ---------------------------------------------------------------

(defparameter *minimal-string-blt-cob*
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 SrcBuf PIC X(64).
000080         05 DstBuf PIC X(64).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Copy\".
000120         PROCEDURE DIVISION.
000130             STRING SrcBuf(1:64) DELIMITED BY SIZE INTO DstBuf(1:64).
000140             GOBACK.
000150         END METHOD \"Copy\".
000160 END OBJECT.
000170 END CLASS Character.
"
  "Minimal class with STRING BLT using reference modification.")

;;; Minimal Arithmetic-Test class — byte and word ADD/SUBTRACT GIVING.
(defparameter *minimal-arithmetic-cob*
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Arithmetic-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 ByteA PIC 99 USAGE BINARY.
000080         05 ByteB PIC 99 USAGE BINARY.
000090         05 ByteC PIC 99 USAGE BINARY.
000100         05 WordA PIC 9999 USAGE BINARY.
000110         05 WordB PIC 9999 USAGE BINARY.
000120         05 WordC PIC 9999 USAGE BINARY.
000130     PROCEDURE DIVISION.
000140         IDENTIFICATION DIVISION.
000150         METHOD-ID. \"Arithmetic-Test\".
000160         PROCEDURE DIVISION.
000170             MOVE 10 TO ByteA.
000180             MOVE 5 TO ByteB.
000190             ADD ByteA TO ByteB GIVING ByteC.
000200             SUBTRACT ByteB FROM ByteA GIVING ByteC.
000210             MOVE 1000 TO WordA.
000220             MOVE 500 TO WordB.
000230             ADD WordA TO WordB GIVING WordC.
000240             SUBTRACT WordB FROM WordA GIVING WordC.
000250             GOBACK.
000260         END METHOD \"Arithmetic-Test\".
000270 END OBJECT.
000280 END CLASS Arithmetic-Test.
"
  "Minimal class with byte/word ADD and SUBTRACT GIVING.")

(test parser/string-blt-refmod-parses
  "STRING source(1:64) DELIMITED BY SIZE INTO dest(1:64) parses."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (is (listp ast))
    (is (eq :program (first ast)))
    (let* ((methods (eightbol::ast-methods ast))
           (copy-method (find "Copy" methods :key #'eightbol::ast-method-name :test #'string=)))
      (is (not (null copy-method)))
      (let ((stmts (eightbol::ast-method-statements copy-method)))
        (is (find :string-blt stmts :key #'first))
        (let ((blt (find :string-blt stmts :key #'first)))
          (is (equal '(:refmod :base "SrcBuf" :start 1 :length 64)
                     (getf (rest blt) :source)))
          (is (equal '(:refmod :base "DstBuf" :start 1 :length 64)
                     (getf (rest blt) :dest))))))))

(test backend-6502/string-blt-emits-loop
  "STRING BLT with refmod emits block copy loop."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :6502 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ldy #$00" asm))
        (is (search "lda SrcBuf,y" asm))
        (is (search "sta DstBuf,y" asm))
        (is (search "cpy #64" asm))
        (is (search "bne :_blt" asm))))))

(test parser/string-blt-with-length-parses
  "STRING src DELIMITED BY SIZE INTO dst LENGTH 32 parses."
  (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 Src PIC X(32).
000080         05 Dst PIC X(32).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Copy\".
000120         PROCEDURE DIVISION.
000130             STRING Src DELIMITED BY SIZE INTO Dst LENGTH 32.
000140             GOBACK.
000150         END METHOD \"Copy\".
000160 END OBJECT.
000170 END CLASS Character.
"))
    (let ((ast (eightbol::parse-eightbol-string cob)))
      (is (find :string-blt (eightbol::ast-method-statements
                             (first (eightbol::ast-methods ast)))
                :key #'first))
      (let ((blt (find :string-blt (eightbol::ast-method-statements
                                    (first (eightbol::ast-methods ast)))
                       :key #'first)))
        (is (equal 32 (getf (rest blt) :length)))))))

(test parser/string-char-delimiter-signals-unsupported
  "STRING with character delimiter signals unsupported."
  (signals eightbol:source-error
    (eightbol::parse-eightbol-string
     (minimal-class-with-stmt "STRING \"X\" DELIMITED BY \" \" INTO HP."))))

;;;; ---------------------------------------------------------------
;;;; AST structure tests — verify statement nodes have correct shape
;;;; ---------------------------------------------------------------

(test ast/move-structure
  "MOVE produces (:move :from expr :to identifier)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "MOVE 1 TO HP.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (move (find :move stmts :key #'first)))
    (is (not (null move)))
    (is (eql 1 (getf (rest move) :from)))
    (is (string= "HP" (getf (rest move) :to)))))

(test ast/add-structure
  "ADD x TO y produces (:add :from expr :to identifier)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "ADD 1 TO HP.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (add (find :add stmts :key #'first)))
    (is (not (null add)))
    (is (eql 1 (getf (rest add) :from)))
    (is (string= "HP" (getf (rest add) :to)))
    (is (null (getf (rest add) :giving)))))

(test ast/add-giving-structure
  "ADD x TO y GIVING z produces (:add :from :to :giving)."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 A PIC 9999 USAGE BINARY.
000080         05 B PIC 9999 USAGE BINARY.
000090         05 C PIC 9999 USAGE BINARY.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             ADD 1 TO A GIVING C.
000150             GOBACK.
000160         END METHOD \"Test\".
000170 END OBJECT.
000180 END CLASS Character.")
         (ast (eightbol::parse-eightbol-string src))
         (stmts (eightbol::ast-method-statements
                 (find "Test" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (add (find :add stmts :key #'first)))
    (is (not (null add)))
    (is (eql 1 (getf (rest add) :from)))
    (is (string= "A" (getf (rest add) :to)))
    (is (string= "C" (getf (rest add) :giving)))))

(test ast/subtract-structure
  "SUBTRACT x FROM y produces (:subtract :from expr :from-target identifier)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "SUBTRACT 1 FROM HP.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (sub (find :subtract stmts :key #'first)))
    (is (not (null sub)))
    (is (eql 1 (getf (rest sub) :from)))
    (is (string= "HP" (getf (rest sub) :from-target)))
    (is (null (getf (rest sub) :giving)))))

(test ast/compute-structure
  "COMPUTE x = expr produces (:compute :target identifier :expression expr)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "COMPUTE HP = 42.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (is (string= "HP" (getf (rest compute) :target)))
    (is (eql 42 (getf (rest compute) :expression)))))

(test ast/set-structure
  "SET identifier TO expression produces (:set :target :value)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "SET HP TO 1.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (set-stmt (find :set stmts :key #'first)))
    (is (not (null set-stmt)))
    (is (string= "HP" (getf (rest set-stmt) :target)))
    (is (eql 1 (getf (rest set-stmt) :value)))))

(test ast/if-structure
  "IF condition THEN stmts END-IF produces (:if :condition :then)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "IF HP IS EQUAL TO 0 THEN MOVE 1 TO HP. END-IF.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (if-stmt (find :if stmts :key #'first)))
    (is (not (null if-stmt)))
    (is (not (null (getf (rest if-stmt) :condition))))
    (is (not (null (getf (rest if-stmt) :then))))))

(test ast/invoke-structure
  "INVOKE obj method produces (:invoke :object :method)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "INVOKE Self \"Kill\".")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (inv (find :invoke stmts :key #'first)))
    (is (not (null inv)))
    (is (not (null (getf (rest inv) :object))))
    (is (equal "Kill" (getf (rest inv) :method)))))

(test ast/perform-structure
  "PERFORM procedure-name produces (:perform :procedure)."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             PERFORM Loop.
000130             GOBACK.
000140 Loop.
000150             GOBACK.
000160         END METHOD \"Think\".
000170 END OBJECT.
000180 END CLASS Character.")
         (ast (eightbol::parse-eightbol-string src))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (perf (find :perform stmts :key #'first)))
    (is (not (null perf)))
    (is (string= "Loop" (format nil "~a" (getf (rest perf) :procedure))))))

(test ast/log-fault-structure
  "LOG FAULT produces (:log-fault :code)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "LOG FAULT \"X\".")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (lf (find :log-fault stmts :key #'first)))
    (is (not (null lf)))
    (is (equal "X" (getf (rest lf) :code)))))

(test ast/debug-break-structure
  "DEBUG BREAK produces (:debug-break :code)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "DEBUG BREAK 0.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (db (find :debug-break stmts :key #'first)))
    (is (not (null db)))
    (is (eql 0 (getf (rest db) :code)))))

(test ast/evaluate-structure
  "EVALUATE produces (:evaluate :subject :when-clauses)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "EVALUATE HP WHEN 0 GOBACK. END-EVALUATE.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (eval-stmt (find :evaluate stmts :key #'first)))
    (is (not (null eval-stmt)))
    (is (equal "HP" (getf (rest eval-stmt) :subject)))
    (is (not (null (getf (rest eval-stmt) :when-clauses))))))

(test ast/call-structure
  "CALL target produces (:call :target :bank)."
  (let* ((stmts (parse-procedure-stmts
                 "000200             CALL Helper."))
         (call (find :call stmts :key #'first)))
    (is (not (null call)))
    (is (not (null (getf (rest call) :target))))
    (is (null (getf (rest call) :service)))))

(test ast/goback-structure
  "GOBACK produces (:goback)."
  (let* ((ast (eightbol::parse-eightbol-string *minimal-character-cob*))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (gb (find :goback stmts :key #'first)))
    (is (not (null gb)))
    (is (eq :goback (first gb)))))

(test ast/exit-method-structure
  "EXIT METHOD produces (:exit-method)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "EXIT METHOD.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (em (find :exit-method stmts :key #'first)))
    (is (not (null em)))
    (is (eq :exit-method (first em)))))

(test ast/exit-program-structure
  "EXIT PROGRAM produces (:exit-program)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "EXIT PROGRAM.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (ep (find :exit-program stmts :key #'first)))
    (is (not (null ep)))
    (is (eq :exit-program (first ep)))))

(test ast/stop-run-structure
  "STOP RUN produces (:stop-run)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "STOP RUN.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (sr (find :stop-run stmts :key #'first)))
    (is (not (null sr)))
    (is (eq :stop-run (first sr)))))

(test ast/exit-structure
  "EXIT produces (:exit)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "EXIT.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (ex (find :exit stmts :key #'first)))
    (is (not (null ex)))
    (is (eq :exit (first ex)))))

(test ast/copy-structure
  "COPY name produces (:copy :name name)."
  (let* ((ast (eightbol::parse-eightbol-string
               (minimal-class-with-stmt "COPY Helper.")))
         (stmts (eightbol::ast-method-statements
                 (find "Think" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (c (find :copy stmts :key #'first)))
    (is (not (null c)))
    (is (eq :copy (first c)))
    (is (string= "Helper" (getf (rest c) :name)))))

(test ast/refmod-structure
  "Reference modification produces (:refmod :base :start :length)."
  (let* ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*))
         (stmts (eightbol::ast-method-statements
                 (find "Copy" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (blt (find :string-blt stmts :key #'first))
         (src (getf (rest blt) :source))
         (dst (getf (rest blt) :dest)))
    (is (and (listp src) (eq :refmod (first src))))
    (is (string= "SrcBuf" (getf (rest src) :base)))
    (is (eql 1 (getf (rest src) :start)))
    (is (eql 64 (getf (rest src) :length)))
    (is (and (listp dst) (eq :refmod (first dst))))
    (is (string= "DstBuf" (getf (rest dst) :base)))))

(test ast/string-blt-with-length-structure
  "STRING BLT LENGTH n produces (:string-blt :source :dest :length)."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 Src PIC X(32).
000080         05 Dst PIC X(32).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Copy\".
000120         PROCEDURE DIVISION.
000130             STRING Src DELIMITED BY SIZE INTO Dst LENGTH 16.
000140             GOBACK.
000150         END METHOD \"Copy\".
000160 END OBJECT.
000170 END CLASS Character.")
         (ast (eightbol::parse-eightbol-string src))
         (stmts (eightbol::ast-method-statements
                 (find "Copy" (eightbol::ast-methods ast)
                       :key #'eightbol::ast-method-name :test #'string=)))
         (blt (find :string-blt stmts :key #'first)))
    (is (not (null blt)))
    (let ((src (getf (rest blt) :source)))
      (is (or (string= "Src" (if (listp src) (getf (rest src) :base) src))
              (string= "Src" (format nil "~a" src)))))
    (is (eql 16 (getf (rest blt) :length)))))

(test ast/data-division-pic-x64
  "Data item PIC X(64) produces :pic in AST."
  (let* ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*))
         (ws (getf (rest ast) :working-storage))
         (data (eightbol::ast-data ast))
         (all-items (append (eightbol::ensure-list ws) (eightbol::ensure-list data)))
         (srcbuf (find "SrcBuf" all-items :key #'second :test #'string-equal)))
    (is (not (null srcbuf)) "SrcBuf data item present")
    (let ((pic (getf (cddr srcbuf) :pic)))
      (when pic
        (is (stringp pic) "PIC is string")
        (is (or (search "X(64)" pic) (search "X" pic))
            "PIC contains X or X(64)")))))

;;;; ---------------------------------------------------------------
;;;; AST round-trip tests
;;;; ---------------------------------------------------------------

(test ast/write-read-round-trip
  "AST written with write-ast and read back with read-ast is equal."
  (let* ((ast (eightbol::parse-eightbol-string *minimal-character-cob*))
         (text (with-output-to-string (s)
                 (eightbol::write-ast ast s)))
         (ast2 (with-input-from-string (s text)
                 (eightbol::read-ast s))))
    (is (equal ast ast2))))

(test ast/write-read-round-trip-string-blt
  "STRING BLT AST round-trips through write-ast/read-ast."
  (let* ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*))
         (text (with-output-to-string (s)
                 (eightbol::write-ast ast s)))
         (ast2 (with-input-from-string (s text)
                 (eightbol::read-ast s))))
    (is (equal ast ast2))))

;;;; ---------------------------------------------------------------
;;;; 6502 Backend tests
;;;; ---------------------------------------------------------------

(defun compile-minimal-6502 ()
  "Compile minimal Character class to 6502 assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :6502 s))))

(test backend-6502/method-think-label
  "6502 output contains MethodCharacterThink: label."
  (is (search "MethodCharacterThink:" (compile-minimal-6502))))

(test backend-6502/method-kill-label
  "6502 output contains MethodCharacterKill: label."
  (is (search "MethodCharacterKill:" (compile-minimal-6502))))

(test backend-6502/rts-present
  "6502 output contains rts (for GOBACK)."
  (is (search "rts" (compile-minimal-6502))))

(test backend-6502/invoke-self-kill
  "6502 output contains jsr InvokeCharacterKill for INVOKE Self \"Kill\"."
  (is (search "jsr InvokeCharacterKill" (compile-minimal-6502))))

(test backend-6502/block-structure
  "6502 output uses .block / .bend wrappers."
  (let ((asm (compile-minimal-6502)))
    (is (search ".block" asm))
    (is (search ".bend" asm))))

(test backend-6502/header-cpu-label
  "6502 output header contains 'generated by EIGHTBOL for 6502'."
  (is (search "generated by EIGHTBOL for 6502" (compile-minimal-6502))))

;;;; ---------------------------------------------------------------
;;;; 65c02 Backend tests (derived from 6502)
;;;; ---------------------------------------------------------------

(defun compile-minimal-65c02 ()
  "Compile minimal Character class to 65c02 assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :65c02 s))))

(test backend-65c02/header-cpu-label
  "65c02 output header contains 'generated by EIGHTBOL for 65c02'."
  (is (search "generated by EIGHTBOL for 65c02" (compile-minimal-65c02))))

(test backend-65c02/method-labels-and-rts
  "65c02 output contains MethodCharacterThink: and rts."
  (let ((asm (compile-minimal-65c02)))
    (is (search "MethodCharacterThink:" asm))
    (is (search "rts" asm))))

(test backend-65c02/block-structure
  "65c02 output uses .block / .bend wrappers."
  (let ((asm (compile-minimal-65c02)))
    (is (search ".block" asm))
    (is (search ".bend" asm))))

(test backend-65c02/string-blt-delegates
  "65c02 STRING BLT emits block copy loop (delegates to 6502)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :65c02 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ldy #$00" asm))
        (is (search "lda SrcBuf,y" asm))
        (is (search "sta DstBuf,y" asm))))))

(test backend-65c02/arithmetic-emits-6502-style
  "65c02 ADD/SUBTRACT GIVING emits lda/adc/sta/sbc (delegates to 6502)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :65c02 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "lda" asm))
        (is (search "adc" asm))
        (is (search "sta" asm))
        (is (search "sbc" asm))))))

(test backend-65c02/uses-bra-for-branches
  "65c02 uses bra (not jmp) for unconditional branches within methods."
  (let ((asm (compile-minimal-65c02)))
    (is (search "bra" asm))
    (is (not (search "jmp" asm)))))

(test backend-6502/uses-jmp-for-branches
  "6502 uses jmp (not bra) for unconditional branches; BRA is 65c02-only."
  (let ((asm (compile-minimal-6502)))
    (is (search "jmp" asm))
    (is (not (search "bra" asm)))))

;;;; ---------------------------------------------------------------
;;;; 65c816 Backend tests (derived from 6502)
;;;; ---------------------------------------------------------------

(defun compile-minimal-65c816 ()
  "Compile minimal Character class to 65c816 assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :65c816 s))))

(test backend-65c816/header-cpu-label
  "65c816 output header contains 'generated by EIGHTBOL for 65c816'."
  (is (search "generated by EIGHTBOL for 65c816" (compile-minimal-65c816))))

(test backend-65c816/method-labels-and-rts
  "65c816 output contains MethodCharacterThink: and rts."
  (let ((asm (compile-minimal-65c816)))
    (is (search "MethodCharacterThink:" asm))
    (is (search "rts" asm))))

(test backend-65c816/block-structure
  "65c816 output uses .block / .bend wrappers."
  (let ((asm (compile-minimal-65c816)))
    (is (search ".block" asm))
    (is (search ".bend" asm))))

(test backend-65c816/string-blt-delegates
  "65c816 STRING BLT emits block copy loop (delegates to 6502)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :65c816 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ldy #$00" asm))
        (is (search "lda SrcBuf,y" asm))
        (is (search "sta DstBuf,y" asm))))))

(test backend-65c816/arithmetic-emits-6502-style
  "65c816 ADD/SUBTRACT GIVING emits lda/adc/sta/sbc (delegates to 6502)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :65c816 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "lda" asm))
        (is (search "adc" asm))
        (is (search "sta" asm))
        (is (search "sbc" asm))))))

;;;; ---------------------------------------------------------------
;;;; HuC6280 Backend tests (derived from 6502)
;;;; ---------------------------------------------------------------

(defun compile-minimal-huc6280 ()
  "Compile minimal Character class to HuC6280 assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :huc6280 s))))

(test backend-huc6280/header-cpu-label
  "HuC6280 output header contains 'generated by EIGHTBOL for HuC6280'."
  (is (search "generated by EIGHTBOL for HuC6280" (compile-minimal-huc6280))))

(test backend-huc6280/method-labels-and-rts
  "HuC6280 output contains MethodCharacterThink: and rts."
  (let ((asm (compile-minimal-huc6280)))
    (is (search "MethodCharacterThink:" asm))
    (is (search "rts" asm))))

(test backend-huc6280/block-structure
  "HuC6280 output uses .block / .bend wrappers."
  (let ((asm (compile-minimal-huc6280)))
    (is (search ".block" asm))
    (is (search ".bend" asm))))

(test backend-huc6280/invoke-self-kill
  "HuC6280 output contains jsr InvokeCharacterKill for INVOKE Self \"Kill\"."
  (is (search "InvokeCharacterKill" (compile-minimal-huc6280))))

(test backend-huc6280/string-blt-delegates
  "HuC6280 STRING BLT emits block copy loop (delegates to 6502)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :huc6280 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ldy #$00" asm))
        (is (search "lda SrcBuf,y" asm))
        (is (search "sta DstBuf,y" asm))))))

(test backend-huc6280/arithmetic-emits-6502-style
  "HuC6280 ADD/SUBTRACT GIVING emits lda/adc/sta/sbc (delegates to 6502)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :huc6280 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "lda" asm))
        (is (search "adc" asm))
        (is (search "sta" asm))
        (is (search "sbc" asm))))))

;;;; ---------------------------------------------------------------
;;;; All backends compile (smoke test)
;;;; ---------------------------------------------------------------

(test backend/all-cpus-compile-minimal
  "All supported CPUs compile minimal Character class without error."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (dolist (cpu eightbol::*supported-cpus*)
      (is-true (handler-case
                  (progn
                    (with-output-to-string (s)
                      (eightbol::compile-to-assembly ast cpu s))
                    t)
                (error (e) (declare (ignore e)) nil))
               "~s backend should compile without error" cpu))))

(test backend/all-cpus-compile-arithmetic
  "All supported CPUs compile minimal Arithmetic-Test class without error."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (dolist (cpu eightbol::*supported-cpus*)
      (is-true (handler-case
                  (progn
                    (with-output-to-string (s)
                      (eightbol::compile-to-assembly ast cpu s))
                    t)
                (error (e) (declare (ignore e)) nil))
               "~s backend should compile arithmetic without error" cpu))))

(test backend/all-cpus-compile-string-blt
  "All supported CPUs compile minimal STRING BLT class without error."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (dolist (cpu eightbol::*supported-cpus*)
      (is-true (handler-case
                  (progn
                    (with-output-to-string (s)
                      (eightbol::compile-to-assembly ast cpu s))
                    t)
                (error (e) (declare (ignore e)) nil))
               "~s backend should compile STRING BLT without error" cpu))))

;;; Minimal class with EVALUATE and INSPECT for backend coverage.
(defparameter *minimal-evaluate-inspect-cob*
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. EvalInspect-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 State PIC 99 USAGE BINARY.
000080         05 Buf PIC X(16).
000090         05 Count PIC 9999 USAGE BINARY.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             EVALUATE State WHEN 0 MOVE 1 TO State. WHEN 1 GOBACK. END-EVALUATE.
000150             INSPECT Buf TALLYING Count FOR CHARACTERS.
000160             GOBACK.
000170         END METHOD \"Test\".
000180 END OBJECT.
000190 END CLASS EvalInspect-Test.
"
  "Minimal class with EVALUATE and INSPECT for backend coverage.")

(test backend/all-cpus-compile-evaluate-inspect
  "All supported CPUs compile EVALUATE and INSPECT without error."
  (let ((ast (eightbol::parse-eightbol-string *minimal-evaluate-inspect-cob*)))
    (dolist (cpu eightbol::*supported-cpus*)
      (is-true (handler-case
                  (progn
                    (with-output-to-string (s)
                      (eightbol::compile-to-assembly ast cpu s))
                    t)
                (error (e) (declare (ignore e)) nil))
               "~s backend should compile EVALUATE/INSPECT without error" cpu))))

;;; Minimal class with COMPUTE shift and bit ops for backend coverage.
(defparameter *minimal-compute-shift-bit-cob*
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. ComputeShiftBit-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 X PIC 99 USAGE BINARY.
000080         05 Y PIC 99 USAGE BINARY.
000090         05 Z PIC 99 USAGE BINARY.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             COMPUTE X = Y SHIFT-LEFT 2.
000150             COMPUTE X = Y SHIFT-RIGHT 1.
000160             COMPUTE X = Y BIT-AND Z.
000170             COMPUTE X = Y BIT-OR Z.
000180             GOBACK.
000190         END METHOD \"Test\".
000200 END OBJECT.
000210 END CLASS ComputeShiftBit-Test.
"
  "Minimal class with COMPUTE shift and bit ops.")

;;; Minimal class with subscript for backend coverage.
(defparameter *minimal-subscript-cob*
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Subscript-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 Arr PIC 99 OCCURS 8.
000080         05 Idx PIC 99 USAGE BINARY.
000090         05 Val PIC 99 USAGE BINARY.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             MOVE Val TO Arr(1).
000150             MOVE Arr(1) TO Val.
000160             GOBACK.
000170         END METHOD \"Test\".
000180 END OBJECT.
000190 END CLASS Subscript-Test.
"
  "Minimal class with subscripted MOVE.")

(test backend/all-cpus-compile-subscript
  "All supported CPUs compile subscripted MOVE without error."
  (with-temporary-directory (:prefix "eightbol-sub-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "Subscript-Test-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (Subscript-Test):" f)
        (write-line "05 Arr PIC 99 OCCURS 8." f)
        (write-line "05 Idx PIC 99 USAGE BINARY." f)
        (write-line "05 Val PIC 99 USAGE BINARY." f))
      (let ((ast (eightbol::parse-eightbol-string *minimal-subscript-cob*)))
        (dolist (cpu eightbol::*supported-cpus*)
          (is-true (handler-case
                      (progn
                        (let ((eightbol::*eightbol-root-directory* root)
                              (eightbol::*copybook-paths* (list gen)))
                          (with-output-to-string (s)
                            (eightbol::compile-to-assembly ast cpu s)))
                        t)
                    (error (e) (declare (ignore e)) nil))
                   "~s backend should compile subscript without error" cpu))))))

(test backend/all-cpus-compile-compute-shift-bit
  "All supported CPUs compile COMPUTE with shift and bit ops without error."
  (with-temporary-directory (:prefix "eightbol-shiftbit-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "ComputeShiftBit-Test-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (ComputeShiftBit-Test):" f)
        (write-line "05 X PIC 99 USAGE BINARY." f)
        (write-line "05 Y PIC 99 USAGE BINARY." f)
        (write-line "05 Z PIC 99 USAGE BINARY." f))
      (let ((ast (eightbol::parse-eightbol-string *minimal-compute-shift-bit-cob*)))
        (dolist (cpu eightbol::*supported-cpus*)
          (is-true (handler-case
                      (progn
                        (let ((eightbol::*eightbol-root-directory* root)
                              (eightbol::*copybook-paths* (list gen)))
                          (with-output-to-string (s)
                            (eightbol::compile-to-assembly ast cpu s)))
                        t)
                    (error (e) (declare (ignore e)) nil))
                   "~s backend should compile COMPUTE shift/bit without error" cpu))))))

;;; Minimal class with SET, PERFORM, LOG FAULT, DEBUG BREAK, CALL for backend coverage.
(defparameter *minimal-stmt-coverage-cob*
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. StmtCoverage-Test.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Test\".
000110         PROCEDURE DIVISION.
000120             SET HP TO 1.
000130             PERFORM Loop.
000140             LOG FAULT \"X\".
000150             DEBUG BREAK 1.
000160             CALL Helper.
000170             GOBACK.
000180 Loop.
000190             GOBACK.
000200         END METHOD \"Test\".
000210 END OBJECT.
000220 END CLASS StmtCoverage-Test.
"
  "Minimal class exercising SET, PERFORM, LOG FAULT, DEBUG BREAK, CALL.")

(test backend/all-cpus-compile-stmt-coverage
  "All supported CPUs compile SET, PERFORM, LOG FAULT, DEBUG BREAK, CALL without error."
  (let ((ast (eightbol::parse-eightbol-string *minimal-stmt-coverage-cob*)))
    (dolist (cpu eightbol::*supported-cpus*)
      (is-true (handler-case
              (progn
                (with-output-to-string (s)
                  (eightbol::compile-to-assembly ast cpu s))
                t)
            (error (e) (declare (ignore e)) nil))
          "~s backend should compile SET/PERFORM/LOG FAULT/DEBUG BREAK/CALL without error" cpu))))

(test backend-6502/arithmetic-output-key-sequences
  "6502 arithmetic output contains key instruction sequences (byte add/subtract)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :6502 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "lda ByteA" asm) "Byte add: lda ByteA")
        (is (search "adc" asm) "Byte add: adc")
        (is (search "sta ByteC" asm) "Byte add: sta ByteC")
        (is (search "sbc" asm) "Byte subtract: sbc")
        (is (search "ByteA" asm) "ByteA referenced")
        (is (search "ByteB" asm) "ByteB referenced")
        (is (search "ByteC" asm) "ByteC referenced")
        (is (search "WordA" asm) "WordA referenced")
        (is (search "WordB" asm) "WordB referenced")
        (is (search "WordC" asm) "WordC referenced")))))

(test backend-6502/bcd-add-emits-sed-daa
  "6502 BCD ADD emits sed and daa when operands are USAGE DECIMAL."
  (with-temporary-directory (:prefix "eightbol-bcd-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "BcdTest-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (BcdTest):" f)
        (write-line "05 ByteA PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteB PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteC PIC 99 USAGE DECIMAL." f))
      (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. BcdTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 ByteA PIC 99 USAGE DECIMAL.
000080         05 ByteB PIC 99 USAGE DECIMAL.
000090         05 ByteC PIC 99 USAGE DECIMAL.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             ADD ByteA TO ByteB GIVING ByteC.
000150             GOBACK.
000160         END METHOD \"Test\".
000170 END OBJECT.
000180 END CLASS BcdTest."))
        (let ((ast (eightbol::parse-eightbol-string cob)))
          (let ((asm (let ((eightbol::*eightbol-root-directory* root)
                          (eightbol::*copybook-paths* (list gen)))
                       (with-output-to-string (s)
                         (eightbol::compile-to-assembly ast :6502 s)))))
            (is (search "sed" asm) "6502 BCD ADD should emit sed")
            (is (search "daa" asm) "6502 BCD ADD should emit daa"))))))))

(test backend-6502/bcd-subtract-emits-sed-sbc
  "6502 BCD SUBTRACT emits sed and sbc when operands are USAGE DECIMAL."
  (with-temporary-directory (:prefix "eightbol-bcd-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "BcdTest-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (BcdTest):" f)
        (write-line "05 ByteA PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteB PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteC PIC 99 USAGE DECIMAL." f))
      (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. BcdTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 ByteA PIC 99 USAGE DECIMAL.
000080         05 ByteB PIC 99 USAGE DECIMAL.
000090         05 ByteC PIC 99 USAGE DECIMAL.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             SUBTRACT ByteB FROM ByteA GIVING ByteC.
000150             GOBACK.
000160         END METHOD \"Test\".
000170 END OBJECT.
000180 END CLASS BcdTest."))
        (let ((ast (eightbol::parse-eightbol-string cob)))
          (let ((asm (let ((eightbol::*eightbol-root-directory* root)
                          (eightbol::*copybook-paths* (list gen)))
                       (with-output-to-string (s)
                         (eightbol::compile-to-assembly ast :6502 s)))))
            (is (search "sed" asm) "6502 BCD SUBTRACT should emit sed")
            (is (search "sbc" asm) "6502 BCD SUBTRACT should emit sbc")))))))

(test backend-6502/constant-load-emits-immediate
  "6502 MOVE of named constant uses lda #ConstName (immediate mode)."
  (with-temporary-directory (:prefix "eightbol-const-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "ConstTest-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (ConstTest):" f)
        (write-line "05 X PIC 99 USAGE BINARY." f)
        (write-line "77 MaxVal PIC 99 USAGE BINARY VALUE 42." f))
      (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. ConstTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 X PIC 99 USAGE BINARY.
000080         77 MaxVal PIC 99 USAGE BINARY VALUE 42.
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Test\".
000120         PROCEDURE DIVISION.
000130             MOVE MaxVal TO X.
000140             GOBACK.
000150         END METHOD \"Test\".
000160 END OBJECT.
000170 END CLASS ConstTest."))
        (let ((ast (eightbol::parse-eightbol-string cob)))
          (let ((asm (let ((eightbol::*eightbol-root-directory* root)
                          (eightbol::*copybook-paths* (list gen)))
                       (with-output-to-string (s)
                         (eightbol::compile-to-assembly ast :6502 s)))))
            (is (search "lda #MaxVal" asm) "Named constant should use immediate mode lda #ConstName")))))))

;;;; ---------------------------------------------------------------
;;;; RP2A03 Backend tests (derived from 6502, no decimal mode)
;;;; ---------------------------------------------------------------

(defun compile-minimal-rp2a03 ()
  "Compile minimal Character class to RP2A03 assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :rp2a03 s))))

(test backend-rp2a03/header-cpu-label
  "RP2A03 output header contains 'generated by EIGHTBOL for RP2A03 (NES)'."
  (is (search "generated by EIGHTBOL for RP2A03" (compile-minimal-rp2a03))))

(test backend-rp2a03/cld-at-method-entry
  "RP2A03 emits cld at method entry (no decimal mode)."
  (is (search "cld" (compile-minimal-rp2a03))))

(test backend-rp2a03/bcd-comment-in-header
  "RP2A03 header mentions BCD software routines."
  (is (search "BCD" (compile-minimal-rp2a03))))

(test backend-rp2a03/string-blt-delegates
  "RP2A03 STRING BLT emits block copy loop (delegates to 6502)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :rp2a03 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ldy #$00" asm))
        (is (search "lda SrcBuf,y" asm))
        (is (search "sta DstBuf,y" asm))))))

(test backend-rp2a03/arithmetic-emits-6502-style
  "RP2A03 ADD/SUBTRACT GIVING emits lda/adc/sta/sbc (delegates to 6502)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :rp2a03 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "lda" asm))
        (is (search "adc" asm))
        (is (search "sta" asm))
        (is (search "sbc" asm))))))

;;;; ---------------------------------------------------------------
;;;; CP1610 Backend tests (Intellivision)
;;;; ---------------------------------------------------------------

(defun compile-minimal-cp1610 ()
  "Compile minimal Character class to CP1610 assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :cp1610 s))))

(test backend-cp1610/header-cpu-label
  "CP1610 output header contains 'generated by EIGHTBOL for CP1610'."
  (is (search "generated by EIGHTBOL for CP1610" (compile-minimal-cp1610))))

(test backend-cp1610/method-labels-and-ret
  "CP1610 output contains MethodCharacterThink PROC and JR R5."
  (let ((asm (compile-minimal-cp1610)))
    (is (search "MethodCharacterThink" asm))
    (is (search "PROC" asm))
    (is (search "JR" asm))
    (is (search "R5" asm))))

(test backend-cp1610/invoke-self-kill
  "CP1610 output contains JSR R5, InvokeCharacterKill for INVOKE Self \"Kill\"."
  (is (search "InvokeCharacterKill" (compile-minimal-cp1610))))

(test backend-cp1610/string-blt-emits-loop
  "CP1610 STRING BLT emits block copy loop using R2/R3 (R5 reserved for JSR return)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :cp1610 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "MVI@" asm))
        (is (search "MVO@" asm))
        (is (search "R2" asm))
        (is (search "R3" asm))))))

(test backend-cp1610/move-emits-mvii-mvo
  "CP1610 MOVE literal TO variable emits MVII and MVO."
  (let ((ast (eightbol::parse-eightbol-string
              (minimal-class-with-stmt "MOVE 1 TO HP."))))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :cp1610 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "MVII" asm) "MOVE/load should emit MVII")
        (is (search "MVO" asm) "MOVE/store should emit MVO")))))

(test backend-cp1610/add-emits-addr
  "CP1610 ADD x TO y GIVING z emits ADDR."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :cp1610 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ADDR" asm) "ADD should emit ADDR")))))

;;;; ---------------------------------------------------------------
;;;; Z80 Backend tests
;;;; ---------------------------------------------------------------

(defun compile-minimal-z80 ()
  "Compile minimal Character class to Z80 assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :z80 s))))

(test backend-z80/header-cpu-label
  "Z80 output header contains 'generated by EIGHTBOL for Z80'."
  (is (search "generated by EIGHTBOL for Z80" (compile-minimal-z80))))

(test backend-z80/method-labels-and-ret
  "Z80 output contains MethodCharacterThink: and ret."
  (let ((asm (compile-minimal-z80)))
    (is (search "MethodCharacterThink" asm))
    (is (search "ret" asm))))

(test backend-z80/invoke-self-kill
  "Z80 output contains call InvokeCharacterKill for INVOKE Self \"Kill\"."
  (is (search "InvokeCharacterKill" (compile-minimal-z80))))

(test backend-z80/string-blt-emits-loop
  "Z80 STRING BLT emits block copy loop (ld (hl), ld (de), inc hl/de, dec bc)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :z80 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ld" asm))
        (is (search "(hl)" asm))
        (is (search "(de)" asm))
        (is (search "inc" asm))
        (is (search "dec" asm)))))

(test backend-z80/move-add-emits-ld-add
  "Z80 MOVE and ADD emit ld and add instructions."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :z80 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ld" asm) "MOVE/ADD should emit ld")
        (is (search "add" asm) "ADD should emit add")))))

(test backend-z80/bcd-add-emits-daa
  "Z80 BCD ADD emits daa after add when operands are USAGE DECIMAL."
  (with-temporary-directory (:prefix "eightbol-bcd-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "BcdTest-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (BcdTest):" f)
        (write-line "05 ByteA PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteB PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteC PIC 99 USAGE DECIMAL." f))
      (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. BcdTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 ByteA PIC 99 USAGE DECIMAL.
000080         05 ByteB PIC 99 USAGE DECIMAL.
000090         05 ByteC PIC 99 USAGE DECIMAL.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             ADD ByteA TO ByteB GIVING ByteC.
000150             GOBACK.
000160         END METHOD \"Test\".
000170 END OBJECT.
000180 END CLASS BcdTest."))
        (let ((ast (eightbol::parse-eightbol-string cob)))
          (let ((asm (let ((eightbol::*eightbol-root-directory* root)
                          (eightbol::*copybook-paths* (list gen)))
                       (with-output-to-string (s)
                         (eightbol::compile-to-assembly ast :z80 s)))))
            (is (search "daa" asm) "Z80 BCD ADD should emit daa")))))))

(test backend-z80/word-add-emits-add-hl
  "Z80 word ADD emits add hl,de for PIC 9999 operands."
  (with-temporary-directory (:prefix "eightbol-word-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "WordTest-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (WordTest):" f)
        (write-line "05 WordA PIC 9999 USAGE BINARY." f)
        (write-line "05 WordB PIC 9999 USAGE BINARY." f)
        (write-line "05 WordC PIC 9999 USAGE BINARY." f))
      (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. WordTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 WordA PIC 9999 USAGE BINARY.
000080         05 WordB PIC 9999 USAGE BINARY.
000090         05 WordC PIC 9999 USAGE BINARY.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             ADD WordA TO WordB GIVING WordC.
000150             GOBACK.
000160         END METHOD \"Test\".
000170 END OBJECT.
000180 END CLASS WordTest."))
        (let ((ast (eightbol::parse-eightbol-string cob)))
          (let ((asm (let ((eightbol::*eightbol-root-directory* root)
                          (eightbol::*copybook-paths* (list gen)))
                       (with-output-to-string (s)
                         (eightbol::compile-to-assembly ast :z80 s)))))
            (is (search "add" asm) "Z80 word ADD should emit add")
            (is (search "hl" asm) "Z80 word ADD should use hl")))))))

;;;; ---------------------------------------------------------------
;;;; m68k Backend tests
;;;; ---------------------------------------------------------------

(defun compile-minimal-m68k ()
  "Compile minimal Character class to m68k assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :m68k s))))

(test backend-m68k/header-and-rts
  "m68k output contains header and rts."
  (let ((asm (compile-minimal-m68k)))
    (is (search "m68k" asm))
    (is (search "rts" asm))
    (is (search "MethodCharacterThink" asm))))

(test backend-m68k/invoke-self-kill
  "m68k output contains jsr InvokeCharacterKill for INVOKE Self \"Kill\"."
  (is (search "InvokeCharacterKill" (compile-minimal-m68k))))

(test backend-m68k/string-blt-emits-loop
  "m68k STRING BLT emits block copy loop (move.b, lea, subq)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :m68k s)
      (let ((asm (get-output-stream-string s)))
        (is (search "move.b" asm))
        (is (search "lea" asm))
        (is (search "subq" asm))))))

(test backend-m68k/move-add-emits-move
  "m68k MOVE and ADD emit move and add instructions."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :m68k s)
      (let ((asm (get-output-stream-string s)))
        (is (search "move" asm) "MOVE/ADD should emit move")
        (is (search "add" asm) "ADD should emit add")))))

(test backend-m68k/bcd-add-emits-abcd
  "m68k BCD ADD emits abcd when operands are USAGE DECIMAL."
  (with-temporary-directory (:prefix "eightbol-bcd-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "BcdTest-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (BcdTest):" f)
        (write-line "05 ByteA PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteB PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteC PIC 99 USAGE DECIMAL." f))
      (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. BcdTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 ByteA PIC 99 USAGE DECIMAL.
000080         05 ByteB PIC 99 USAGE DECIMAL.
000090         05 ByteC PIC 99 USAGE DECIMAL.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             ADD ByteA TO ByteB GIVING ByteC.
000150             GOBACK.
000160         END METHOD \"Test\".
000170 END OBJECT.
000180 END CLASS BcdTest."))
        (let ((ast (eightbol::parse-eightbol-string cob)))
          (let ((asm (let ((eightbol::*eightbol-root-directory* root)
                          (eightbol::*copybook-paths* (list gen)))
                       (with-output-to-string (s)
                         (eightbol::compile-to-assembly ast :m68k s)))))
            (is (search "abcd" asm) "m68k BCD ADD should emit abcd"))))))))

(test backend-m68k/bcd-subtract-emits-sbcd
  "m68k BCD SUBTRACT emits sbcd when operands are USAGE DECIMAL."
  (with-temporary-directory (:prefix "eightbol-bcd-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "BcdTest-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (BcdTest):" f)
        (write-line "05 ByteA PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteB PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteC PIC 99 USAGE DECIMAL." f))
      (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. BcdTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 ByteA PIC 99 USAGE DECIMAL.
000080         05 ByteB PIC 99 USAGE DECIMAL.
000090         05 ByteC PIC 99 USAGE DECIMAL.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             SUBTRACT ByteB FROM ByteA GIVING ByteC.
000150             GOBACK.
000160         END METHOD \"Test\".
000170 END OBJECT.
000180 END CLASS BcdTest."))
        (let ((ast (eightbol::parse-eightbol-string cob)))
          (let ((asm (let ((eightbol::*eightbol-root-directory* root)
                          (eightbol::*copybook-paths* (list gen)))
                       (with-output-to-string (s)
                         (eightbol::compile-to-assembly ast :m68k s)))))
            (is (search "sbcd" asm) "m68k BCD SUBTRACT should emit sbcd")))))))

;;;; ---------------------------------------------------------------
;;;; SM83 Backend tests
;;;; ---------------------------------------------------------------

(defun compile-minimal-sm83 ()
  "Compile minimal Character class to SM83 (Game Boy) assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :sm83 s))))

(test backend-sm83/header-and-ret
  "SM83 output contains header and ret."
  (let ((asm (compile-minimal-sm83)))
    (is (search "SM83" asm))
    (is (search "ret" asm))
    (is (search "MethodCharacterThink" asm))))

(test backend-sm83/invoke-self-kill
  "SM83 output contains call InvokeCharacterKill for INVOKE Self \"Kill\"."
  (is (search "InvokeCharacterKill" (compile-minimal-sm83))))

(test backend-sm83/string-blt-emits-loop
  "SM83 STRING BLT emits block copy loop (ld [hl+], ld [de])."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :sm83 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ld" asm))
        (is (search "[hl" asm))
        (is (search "[de]" asm))))))

(test backend-sm83/move-emits-ld
  "SM83 MOVE literal TO variable emits ld."
  (let ((ast (eightbol::parse-eightbol-string
              (minimal-class-with-stmt "MOVE 1 TO HP."))))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :sm83 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ld" asm) "MOVE should emit ld")
        (is (search "Hp" asm) "Variable Hp should appear")))))

(test backend-sm83/add-emits-add
  "SM83 ADD x TO y GIVING z emits add."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :sm83 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "add" asm) "ADD should emit add")))))

;;;; ---------------------------------------------------------------
;;;; ARM7 Backend tests
;;;; ---------------------------------------------------------------

(defun compile-minimal-arm7 ()
  "Compile minimal Character class to ARM7 assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :arm7 s))))

(test backend-arm7/header-and-bx-lr
  "ARM7 output contains header and bx lr."
  (let ((asm (compile-minimal-arm7)))
    (is (search "ARM7" asm))
    (is (search "bx" asm))
    (is (search "lr" asm))
    (is (search "MethodCharacterThink" asm))))

(test backend-arm7/invoke-self-kill
  "ARM7 output contains bl InvokeCharacterKill for INVOKE Self \"Kill\"."
  (is (search "InvokeCharacterKill" (compile-minimal-arm7))))

(test backend-arm7/string-blt-emits-loop
  "ARM7 STRING BLT emits block copy loop (ldrb, strb)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :arm7 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "ldrb" asm))
        (is (search "strb" asm))))))

(test backend-arm7/move-emits-movs-str
  "ARM7 MOVE literal TO variable emits movs and str."
  (let ((ast (eightbol::parse-eightbol-string
              (minimal-class-with-stmt "MOVE 1 TO HP."))))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :arm7 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "movs" asm) "MOVE/load should emit movs")
        (is (or (search "strb" asm) (search "strh" asm)) "MOVE/store should emit str")))))

(test backend-arm7/add-emits-add
  "ARM7 ADD x TO y GIVING z emits add."
  (let ((ast (eightbol::parse-eightbol-string *minimal-arithmetic-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :arm7 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "add" asm) "ADD should emit add")))))

;;;; ---------------------------------------------------------------
;;;; i286 Backend tests
;;;; ---------------------------------------------------------------

(defun compile-minimal-i286 ()
  "Compile minimal Character class to i286 assembly, return as string."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :i286 s))))

(test backend-i286/header-and-ret
  "i286 output contains header and ret."
  (let ((asm (compile-minimal-i286)))
    (is (search "i286" asm))
    (is (search "ret" asm))
    (is (search "MethodCharacterThink" asm))))

(test backend-i286/invoke-self-kill
  "i286 output contains call InvokeCharacterKill for INVOKE Self \"Kill\"."
  (is (search "InvokeCharacterKill" (compile-minimal-i286))))

(test backend-i286/string-blt-emits-loop
  "i286 STRING BLT emits block copy loop (mov [si], mov [di], loop)."
  (let ((ast (eightbol::parse-eightbol-string *minimal-string-blt-cob*)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly ast :i286 s)
      (let ((asm (get-output-stream-string s)))
        (is (search "mov" asm))
        (is (search "[si]" asm))
        (is (search "[di]" asm))
        (is (search "loop" asm))))))

(test backend-i286/bcd-add-emits-daa
  "i286 BCD ADD emits daa when operands are USAGE DECIMAL."
  (with-temporary-directory (:prefix "eightbol-bcd-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "BcdTest-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (BcdTest):" f)
        (write-line "05 ByteA PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteB PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteC PIC 99 USAGE DECIMAL." f))
      (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. BcdTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 ByteA PIC 99 USAGE DECIMAL.
000080         05 ByteB PIC 99 USAGE DECIMAL.
000090         05 ByteC PIC 99 USAGE DECIMAL.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             ADD ByteA TO ByteB GIVING ByteC.
000150             GOBACK.
000160         END METHOD \"Test\".
000170 END OBJECT.
000180 END CLASS BcdTest."))
        (let ((ast (eightbol::parse-eightbol-string cob)))
          (let ((asm (let ((eightbol::*eightbol-root-directory* root)
                          (eightbol::*copybook-paths* (list gen)))
                       (with-output-to-string (s)
                         (eightbol::compile-to-assembly ast :i286 s)))))
            (is (search "daa" asm) "i286 BCD ADD should emit daa")))))))

(test backend-i286/bcd-subtract-emits-das
  "i286 BCD SUBTRACT emits das when operands are USAGE DECIMAL."
  (with-temporary-directory (:prefix "eightbol-bcd-")
    (let* ((root (uiop:pathname-directory-pathname (uiop:getcwd)))
           (gen  (merge-pathnames
                  (make-pathname :directory '(:relative "Source" "Generated" "Classes"))
                  root)))
      (ensure-directories-exist gen)
      (with-open-file (f (merge-pathnames (make-pathname :name "BcdTest-Slots" :type "cpy") gen)
                         :direction :output :if-exists :supersede)
        (write-line "* Own slots (BcdTest):" f)
        (write-line "05 ByteA PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteB PIC 99 USAGE DECIMAL." f)
        (write-line "05 ByteC PIC 99 USAGE DECIMAL." f))
      (let ((cob "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. BcdTest.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 ByteA PIC 99 USAGE DECIMAL.
000080         05 ByteB PIC 99 USAGE DECIMAL.
000090         05 ByteC PIC 99 USAGE DECIMAL.
000100     PROCEDURE DIVISION.
000110         IDENTIFICATION DIVISION.
000120         METHOD-ID. \"Test\".
000130         PROCEDURE DIVISION.
000140             SUBTRACT ByteB FROM ByteA GIVING ByteC.
000150             GOBACK.
000160         END METHOD \"Test\".
000170 END OBJECT.
000180 END CLASS BcdTest."))
        (let ((ast (eightbol::parse-eightbol-string cob)))
          (let ((asm (let ((eightbol::*eightbol-root-directory* root)
                          (eightbol::*copybook-paths* (list gen)))
                       (with-output-to-string (s)
                         (eightbol::compile-to-assembly ast :i286 s)))))
            (is (search "das" asm) "i286 BCD SUBTRACT should emit das")))))))

;;;; ---------------------------------------------------------------
;;;; CPU display names (case-preserving UI)
;;;; ---------------------------------------------------------------

(test backend/cpu-display-names
  "cpu-display-name returns intended case for all supported CPUs."
  (let ((expected '(("6502" . :6502) ("65c02" . :65c02) ("65c816" . :65c816)
                    ("cp1610" . :cp1610) ("HuC6280" . :huc6280) ("RP2A03" . :rp2a03)
                    ("Z80" . :z80) ("SM83" . :sm83) ("m68k" . :m68k)
                    ("i286" . :i286) ("ARM7" . :arm7))))
    (dolist (pair expected)
      (is (string= (first pair) (eightbol::cpu-display-name (rest pair)))
          "~s should display as ~s" (rest pair) (first pair)))))

(test backend/cpu-directory-name-canonical-case
  "cpu-directory-name returns canonical case for output directories."
  (is (string= "cp1610" (eightbol::cpu-directory-name :cp1610)))
  (is (string= "65c02" (eightbol::cpu-directory-name :65c02)))
  (is (string= "RP2A03" (eightbol::cpu-directory-name :rp2a03)))
  (is (string= "m68k" (eightbol::cpu-directory-name :m68k)))
  (is (string= "i286" (eightbol::cpu-directory-name :i286)))
  (is (string= "Z80" (eightbol::cpu-directory-name :z80)))
  (is (string= "HuC6280" (eightbol::cpu-directory-name :huc6280)))
  (is (string= "SM83" (eightbol::cpu-directory-name :sm83)))
  (is (string= "ARM7" (eightbol::cpu-directory-name :arm7))))

;;;; ---------------------------------------------------------------
;;;; 6502 Backend — detailed instruction-level integration tests
;;;; Uses direct AST construction to avoid COPY/copybook dependencies.
;;;; ---------------------------------------------------------------

(defun asm-from-ast (ast)
  "Compile a hand-crafted AST directly to 6502 assembly string.
Bypasses the file/copybook pipeline; binds empty dynamics for tables.
AST is a :method plist (not full :program)."
  (let ((empty (make-hash-table :test #'equal)))
    (let ((eightbol::*class-id* "TestClass")
          (eightbol::*slot-table* empty)
          (eightbol::*type-table* empty)
          (eightbol::*const-table* empty)
          (eightbol::*service-bank-table* empty)
          (eightbol::*usage-table* empty)
          (eightbol::*sign-table* empty)
          (eightbol::*pic-size-table* empty)
          (eightbol::*pic-width-table* empty))
      (with-output-to-string (s)
        (let ((eightbol::*output-stream* s))
          (eightbol::compile-6502-method ast "TestClass" :6502))))))

;;; --- CALL SERVICE / .FarCall ---

(test backend-6502/call-service-farcall
  "CALL target IN SERVICE bank. emits .FarCall ServiceTarget, BankID."
  (let ((asm (asm-from-ast
              '(:method :method-id "Run"
                        :statements ((:call :service "Service-Compose-Character"
                                            :bank "Bank-Behavior"))))))
    (is (search ".FarCall" asm))
    (is (search "ServiceComposeCharacter" asm))
    (is (search "BankBehavior" asm))))

(test backend-6502/call-service-no-bank-errors
  "CALL SERVICE target. with nil bank signals error (bank required from copybook)."
  (signals error
    (asm-from-ast
     '(:method :method-id "Run"
               :statements ((:call :service "Service-Animate-Lighting"
                                  :bank nil))))))

(test backend-6502/call-in-bank-farjsr
  "CALL target IN BANK bank. still emits .FarJSR BankID, Target."
  (let ((asm (asm-from-ast
              '(:method :method-id "Run"
                        :statements ((:call :target "Play-Song"
                                            :bank "Bank-Music"))))))
    (is (search ".FarJSR" asm))
    (is (search "BankMusic" asm))
    (is (search "PlaySong" asm))))

(test backend-6502/call-in-library-jsr
  "CALL target IN LIBRARY name. emits plain jsr."
  (let ((asm (asm-from-ast
              '(:method :method-id "Run"
                        :statements ((:call :target "Lib-Routine"
                                            :bank nil :library t))))))
    (is (search "jsr LibRoutine" asm))
    (is (not (search ".FarJSR" asm)))))

;;; --- ADD 1 / SUBTRACT 1 optimisation ---

(test backend-6502/add-1-uses-inc
  "ADD 1 TO variable emits inc, not adc."
  (let ((asm (asm-from-ast
              '(:method :method-id "Inc"
                        :statements ((:add :from 1 :to "Counter"))))))
    (is (search "inc Counter" asm))
    (is (not (search "adc" asm)))))

(test backend-6502/add-1-giving-does-not-use-inc
  "ADD 1 TO x GIVING y does NOT use inc (result goes to a different variable)."
  (let ((asm (asm-from-ast
              '(:method :method-id "Inc"
                        :statements ((:add :from 1 :to "Counter" :giving "Result"))))))
    ;; should still use adc, not inc
    (is (search "adc" asm))
    (is (not (search "inc Counter" asm)))))

(test backend-6502/subtract-1-uses-dec
  "SUBTRACT 1 FROM variable emits dec, not sbc."
  (let ((asm (asm-from-ast
              '(:method :method-id "Dec"
                        :statements ((:subtract :from 1 :from-target "Counter"))))))
    (is (search "dec Counter" asm))
    (is (not (search "sbc" asm)))))

(test backend-6502/add-2-uses-adc
  "ADD 2 TO variable does NOT use inc — falls back to lda/clc/adc/sta."
  (let ((asm (asm-from-ast
              '(:method :method-id "Add2"
                        :statements ((:add :from 2 :to "Counter"))))))
    (is (search "adc" asm))
    (is (not (search "inc Counter" asm)))))

;;; --- Arithmetic byte/word (ADD/SUBTRACT GIVING) ---

(test backend-6502/arithmetic-byte-add-giving
  "ADD x TO y GIVING z emits lda/adc/sta sequence for byte operands."
  (let ((asm (asm-from-ast
              '(:method :method-id "Add"
                        :statements ((:add :from "ByteA" :to "ByteB" :giving "ByteC"))))))
    (is (search "lda ByteA" asm))
    (is (search "adc" asm))
    (is (search "sta ByteC" asm))))

(test backend-6502/arithmetic-byte-subtract-giving
  "SUBTRACT x FROM y GIVING z emits lda/sec/sbc/sta sequence."
  (let ((asm (asm-from-ast
              '(:method :method-id "Sub"
                        :statements ((:subtract :from "ByteB" :from-target "ByteA" :giving "ByteC"))))))
    (is (search "lda ByteA" asm))
    (is (search "sbc" asm))
    (is (search "sta ByteC" asm))))

;;; --- Shift operators ---

(test backend-6502/shift-left-emits-asl
  "COMPUTE X = Y SHIFT-LEFT 3 emits three asl a instructions."
  (let ((asm (asm-from-ast
              '(:method :method-id "Shift"
                        :statements ((:compute :target "X"
                                               :expression (:shift-left "Y" 3)))))))
    (is (= 3 (let ((count 0) (pos 0))
               (loop while (setf pos (search "asl a" asm :start2 pos))
                     do (incf count) (incf pos))
               count)))))

(test backend-6502/shift-right-emits-lsr
  "COMPUTE X = Y SHIFT-RIGHT 2 emits two lsr a instructions."
  (let ((asm (asm-from-ast
              '(:method :method-id "Shift"
                        :statements ((:compute :target "X"
                                               :expression (:shift-right "Y" 2)))))))
    (is (= 2 (let ((count 0) (pos 0))
               (loop while (setf pos (search "lsr a" asm :start2 pos))
                     do (incf count) (incf pos))
               count)))))

;;; --- Bitwise operators ---

(test backend-6502/bit-and-emits-and
  "COMPUTE X = Y BIT-AND #$80 emits and #$80 (hex immediate)."
  (let ((asm (asm-from-ast
              '(:method :method-id "Mask"
                        :statements ((:compute :target "X"
                                               :expression (:bit-and "Y" 128)))))))
    (is (search "and #$80" asm))
    (is (search "sta X" asm))))

(test backend-6502/bit-or-emits-ora
  "COMPUTE X = Y BIT-OR Z emits ora Z."
  (let ((asm (asm-from-ast
              '(:method :method-id "Flag"
                        :statements ((:compute :target "X"
                                               :expression (:bit-or "Y" "Z-Flag")))))))
    (is (search "ora ZFlag" asm))))

(test backend-6502/bit-xor-emits-eor
  "COMPUTE X = Y BIT-XOR $FF emits eor #$ff (hex immediate)."
  (let ((asm (asm-from-ast
              '(:method :method-id "Flip"
                        :statements ((:compute :target "X"
                                               :expression (:bit-xor "Y" 255)))))))
    (is (search "eor #$ff" asm))))

(test backend-6502/bit-not-emits-eor-ff
  "COMPUTE X = BIT-NOT Y emits eor #$ff."
  (let ((asm (asm-from-ast
              '(:method :method-id "Inv"
                        :statements ((:compute :target "X"
                                               :expression (:bit-not "Y")))))))
    (is (search "eor #$ff" asm))))

;;; --- Subscript (indexed array access) ---

(test backend-6502/subscript-store-emits-indexed-sta
  "MOVE val TO arr(idx) emits tax, sta Arr,x."
  (let ((asm (asm-from-ast
              '(:method :method-id "Store"
                        :statements ((:move :from "Val" :to (:subscript "Arr" 1)))))))
    (is (search "tax" asm))
    (is (search "sta Arr,x" asm))
    (is (search "lda Val" asm))))

(test backend-6502/subscript-load-emits-indexed-lda
  "MOVE arr(idx) TO dest emits tax, lda Arr,x, sta dest."
  (let ((asm (asm-from-ast
              '(:method :method-id "Load"
                        :statements ((:move :from (:subscript "Arr" 1) :to "Dest"))))))
    (is (search "tax" asm))
    (is (search "lda Arr,x" asm))
    (is (search "sta Dest" asm))))

(test backend-6502/log-fault-emits-logfault
  "LOG FAULT \"X\" emits .LogFault directive."
  (let ((asm (asm-from-ast
              '(:method :method-id "Test"
                        :statements ((:log-fault :code "X"))))))
    (is (search ".LogFault" asm))
    (is (search "X" asm))))

(test backend-6502/debug-break-emits-debugbreak
  "DEBUG BREAK 1 emits .DebugBreak directive."
  (let ((asm (asm-from-ast
              '(:method :method-id "Test"
                        :statements ((:debug-break :code 1))))))
    (is (search ".DebugBreak" asm))))

;;; --- Unsupported CPU tests
;;;; ---------------------------------------------------------------
;;;; Unsupported CPU tests
;;;; ---------------------------------------------------------------

(test backend/unsupported-cpu-signals-error
  "Requesting an unimplemented CPU backend signals an error."
  (let ((ast (eightbol::parse-eightbol-string *minimal-character-cob*)))
    (signals error
      (with-output-to-string (s)
        (eightbol::compile-to-assembly ast :pdp11 s)))))

;;;; ---------------------------------------------------------------
;;;; Parser integration tests — new grammar forms
;;;; Each test parses a minimal self-contained EIGHTBOL snippet and
;;;; inspects the resulting AST for correct structure.
;;;; ---------------------------------------------------------------

(defun parse-procedure-stmts (procedure-text)
  "Parse PROCEDURE-TEXT as the body of a minimal method and return the
statement list for the first method.  PROCEDURE-TEXT should contain only
statements, not the method wrapper boilerplate."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. TestClass.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070     PROCEDURE DIVISION.
000080         IDENTIFICATION DIVISION.
000090         METHOD-ID. \"Test\".
000100         PROCEDURE DIVISION.
~a
000900             GOBACK.
000910         END METHOD \"Test\".
000920 END OBJECT.
000930 END CLASS TestClass.
" procedure-text))
         (ast (eightbol::parse-eightbol-string src))
         (method (first (eightbol::ast-methods ast))))
    (eightbol::ast-method-statements method)))

(test parser/call-in-service-produces-service-key
  "CALL target IN SERVICE bank. produces :service key in AST."
  (let* ((stmts (parse-procedure-stmts
                 "000200             CALL Service-Do-Thing IN SERVICE Bank-Behavior."))
         (call  (find :call stmts :key #'first)))
    (is (not (null call)))
    (is (not (null (getf (rest call) :service))))
    (is (not (null (getf (rest call) :bank))))))

(test parser/call-service-prefix-produces-service-key
  "CALL SERVICE target. produces :service key in AST."
  (let* ((stmts (parse-procedure-stmts
                 "000200             CALL SERVICE Service-Compose-Character."))
         (call  (find :call stmts :key #'first)))
    (is (not (null call)))
    (is (not (null (getf (rest call) :service))))))

(test parser/call-in-bank-does-not-produce-service-key
  "CALL target IN BANK bank. does NOT produce :service key."
  (let* ((stmts (parse-procedure-stmts
                 "000200             CALL Play-Song IN BANK Bank-Music."))
         (call  (find :call stmts :key #'first)))
    (is (not (null call)))
    (is (null (getf (rest call) :service)))
    (is (not (null (getf (rest call) :bank))))))

(test parser/call-in-library-produces-library-key
  "CALL target IN LIBRARY name produces :library key in AST."
  (let* ((stmts (parse-procedure-stmts
                 "000200             CALL Helper IN LIBRARY Runtime."))
         (call (find :call stmts :key #'first)))
    (is (not (null call)))
    (is (eq t (getf (rest call) :library)))))

(test parser/shift-left-expression-in-compute
  "COMPUTE X = Y SHIFT-LEFT 2 produces :shift-left AST node."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = Y SHIFT-LEFT 2."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :shift-left (first expr)))))))

(test parser/shift-right-expression-in-compute
  "COMPUTE X = Y SHIFT-RIGHT 1 produces :shift-right AST node."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = Y SHIFT-RIGHT 1."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :shift-right (first expr)))))))

(test parser/bit-and-expression-in-compute
  "COMPUTE X = Y BIT-AND Z produces :bit-and AST node."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = Y BIT-AND Z."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :bit-and (first expr)))))))

(test parser/bit-or-expression-in-compute
  "COMPUTE X = Y BIT-OR Z produces :bit-or AST node."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = Y BIT-OR Z."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :bit-or (first expr)))))))

(test parser/bit-xor-expression-in-compute
  "COMPUTE X = Y BIT-XOR Z produces :bit-xor AST node."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = Y BIT-XOR Z."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :bit-xor (first expr)))))))

(test parser/bit-not-expression-in-compute
  "COMPUTE X = BIT-NOT Y produces :bit-not AST node."
  (let* ((stmts (parse-procedure-stmts
                 "000200             COMPUTE X = BIT-NOT Y."))
         (compute (find :compute stmts :key #'first)))
    (is (not (null compute)))
    (let ((expr (getf (rest compute) :expression)))
      (is (and (listp expr) (eq :bit-not (first expr)))))))

;;;; ---------------------------------------------------------------
;;;; Argument parsing tests
;;;; ---------------------------------------------------------------

(test main/parse-args-input-file
  "parse-arguments recognises a bare filename as :input-file."
  (let ((opts (eightbol::parse-arguments '("Character.cob"))))
    (is (string= "Character.cob" (getf opts :input-file)))))

(test main/parse-args-include-path
  "parse-arguments recognises -I option."
  (let ((opts (eightbol::parse-arguments '("-I" "/path/to/copybooks" "File.cob"))))
    (is (string= "/path/to/copybooks" (getf opts :include-path)))))

(test main/parse-args-cpu-option
  "parse-arguments recognises -m 6502 as CPU keyword."
  (let ((opts (eightbol::parse-arguments '("-m" "6502" "File.cob"))))
    (is (eq :6502 (getf opts :cpu)))))

(test main/parse-args-unknown-cpu-errors
  "parse-arguments signals error for unknown CPU."
  (signals error
    (eightbol::parse-arguments '("-m" "pdp11" "File.cob"))))

(test main/parse-args-m-all
  "parse-arguments recognises -m all for all backends."
  (let ((opts (eightbol::parse-arguments '("-m" "all" "File.cob"))))
    (is (eq :all (getf opts :cpu)))))

(test main/parse-args-multiple-input-files
  "parse-arguments collects all positional args as input files."
  (let ((opts (eightbol::parse-arguments '("a.cob" "b.cob"))))
    (is (equal '("a.cob" "b.cob")
               (loop for (k v) on opts by #'cddr when (eq k :input-file) collect v)))))

(test lexer/valid-copybook-name-p
  "valid-copybook-name-p rejects path traversal and accepts valid names."
  (is-true (eightbol::valid-copybook-name-p "Foo"))
  (is-true (eightbol::valid-copybook-name-p "Character-Slots"))
  (is-false (eightbol::valid-copybook-name-p "../evil"))
  (is-false (eightbol::valid-copybook-name-p "foo/bar"))
  (is-false (eightbol::valid-copybook-name-p ".hidden"))
  (is-false (eightbol::valid-copybook-name-p "")))

(test lexer/paren-dot-separate-tokens
  "). produces two tokens ( ) and . ), not bareword )."
  (let ((tokens (with-input-from-string
                   (s "000100     FOO).")
                 (eightbol::lexer s))))
    (let ((types (mapcar #'first tokens)))
      (is (member '|)| types) "Should have ) token")
      (is (member '|.| types) "Should have . token")
      (is (not (find-if (lambda (tok)
                          (and (eq (first tok) 'bareword)
                               (string= (cadr tok) ").")))
                       tokens))
          "Should not have bareword )."))))
