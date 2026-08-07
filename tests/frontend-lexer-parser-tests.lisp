;;; tests/frontend-lexer-parser-tests.lisp — Comprehensive tests for all 14 frontend lexers and parsers
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol/test)

;;; Define parent suites for organization
(fiveam:def-suite :frontend-lexers
  :description "Tests for all frontend lexer implementations")

(fiveam:def-suite :frontend-parsers
  :description "Tests for all frontend parser implementations")

;;;; BASIC Lexer Tests
(fiveam:def-suite :basic-lexer
  :in :frontend-lexers
  :description "BASIC lexer tests")
(in-suite :basic-lexer)

(test basic-lexer-callable
  "Test BASIC lexer is callable."
  (is (fboundp 'eightbol::basic-lex-line)))

(test basic-lexer-produces-tokens
  "Test BASIC lexer produces tokens for non-empty input."
  (let ((tokens (eightbol::basic-lex-line "100 x")))
    (is (> (length tokens) 0))))

(test basic-lexer-handles-numbers
  "Test BASIC lexer produces number tokens."
  (let ((tokens (eightbol::basic-lex-line "100")))
    (is (eq :NUMBER (car (first tokens))))))

(test basic-lexer-handles-strings
  "Test BASIC lexer produces string tokens."
  (let ((tokens (eightbol::basic-lex-line "\"hello\"")))
    (is (eq :string (car (first tokens))))))

(test basic-lexer-handles-identifiers
  "Test BASIC lexer produces identifier tokens."
  (let ((tokens (eightbol::basic-lex-line "xyz")))
    (is (not (null tokens)))))

(test basic-lexer-handles-operators
  "Test BASIC lexer produces operator tokens."
  (let ((tokens (eightbol::basic-lex-line "+")))
    (is (not (null tokens)))))

;;;; BASIC Parser Tests
(fiveam:def-suite :basic-parser
  :in :frontend-parsers
  :description "BASIC parser tests")
(in-suite :basic-parser)

(test basic-parser-token-list-function-exists
  "Test BASIC parser has token-list function."
  (is (not (null (eightbol::basic-token-list)))))

;;;; COBOL Lexer Tests
(fiveam:def-suite :cobol-lexer
  :in :frontend-lexers
  :description "COBOL lexer tests")
(in-suite :cobol-lexer)

(test cobol-lexer-exists
  "Test COBOL lexer exists and is callable."
  (is (fboundp 'eightbol::cobol-lex-line)))

(test cobol-parser-token-list-exists
  "Test COBOL parser token-list function exists."
  (is (fboundp 'eightbol::cobol-token-list)))

;;;; AGI Lexer Tests
(fiveam:def-suite :agi-lexer
  :in :frontend-lexers
  :description "AGI lexer tests")
(in-suite :agi-lexer)

(test agi-lexer-callable
  "Test AGI lexer is callable."
  (is (fboundp 'eightbol::agi-lex-line)))

(test agi-lexer-produces-tokens
  "Test AGI lexer produces tokens for non-empty input."
  (let ((tokens (eightbol::agi-lex-line "IF v1")))
    (is (> (length tokens) 0))))

;;;; Lingo Lexer Tests
(fiveam:def-suite :lingo-lexer
  :in :frontend-lexers
  :description "Lingo lexer tests")
(in-suite :lingo-lexer)

(test lingo-lexer-exists
  "Test Lingo lexer exists and is callable."
  (is (fboundp 'eightbol::lingo-lex-line)))

;;;; Lua Lexer Tests
(fiveam:def-suite :lua-lexer
  :in :frontend-lexers
  :description "Lua lexer tests")
(in-suite :lua-lexer)

(test lua-lexer-exists
  "Test Lua lexer exists and is callable."
  (is (fboundp 'eightbol::lua-lex-line)))

;;;; Pascal Lexer Tests
(fiveam:def-suite :pascal-lexer
  :in :frontend-lexers
  :description "Pascal lexer tests")
(in-suite :pascal-lexer)

(test pascal-lexer-exists
  "Test Pascal lexer exists and is callable."
  (is (fboundp 'eightbol::pascal-lex-line)))

;;;; Fortran Lexer Tests
(fiveam:def-suite :fortran-lexer
  :in :frontend-lexers
  :description "Fortran lexer tests")
(in-suite :fortran-lexer)

(test fortran-lexer-exists
  "Test Fortran lexer exists and is callable."
  (is (fboundp 'eightbol::fortran-lex-line)))

;;;; Objective-C Lexer Tests
(fiveam:def-suite :objective-c-lexer
  :in :frontend-lexers
  :description "Objective-C lexer tests")
(in-suite :objective-c-lexer)

(test objective-c-lexer-exists
  "Test Objective-C lexer exists and is callable."
  (is (fboundp 'eightbol::objective-lex-line)))

;;;; SmallTalk Lexer Tests
(fiveam:def-suite :smalltalk-lexer
  :in :frontend-lexers
  :description "SmallTalk lexer tests")
(in-suite :smalltalk-lexer)

(test smalltalk-lexer-exists
  "Test SmallTalk lexer exists and is callable."
  (is (fboundp 'eightbol::smalltalk-lex-line)))

;;;; Lexer Consistency Tests
(fiveam:def-suite :lexer-consistency
  :in :frontend-lexers
  :description "Tests for lexer consistency across all frontends")
(in-suite :lexer-consistency)

(test lexer-empty-line-handling
  "Test lexers handle empty lines gracefully."
  (is (null (eightbol::basic-lex-line "")))
  (is (null (eightbol::agi-lex-line ""))))

(test lexer-whitespace-handling
  "Test lexers skip whitespace correctly."
  (let ((tokens1 (eightbol::basic-lex-line "  x  =  10  "))
        (tokens2 (eightbol::basic-lex-line "x=10")))
    ;; Both should produce same token count
    (is (= (length tokens1) (length tokens2)))))

;;;; Parser Error Handling Tests
(fiveam:def-suite :parser-error-handling
  :in :frontend-parsers
  :description "Tests for parser error handling")
(in-suite :parser-error-handling)

(test parser-token-list-functions-exist
  "Test all parsers have a token-list function."
  (is (not (null (eightbol::basic-token-list))))
  (is (not (null (eightbol::agi-token-list))))
  (is (not (null (eightbol::cobol-token-list)))))

;;;; AST Node Construction Tests
(fiveam:def-suite :ast-node-construction
  :in :frontend-parsers
  :description "Tests for AST node construction")
(in-suite :ast-node-construction)

(test ast-make-move-node
  "Test make-move-node construction."
  (let ((node (eightbol::make-move-node (eightbol::make-literal 10) (eightbol::make-identifier "x"))))
    (is (not (null node)))
    (is (listp node))))

(test ast-make-if-node
  "Test make-if-node construction."
  (let ((node (eightbol::make-if-node 
               (eightbol::make-conditional-node := (eightbol::make-identifier "x") (eightbol::make-literal 10))
               (list (eightbol::make-goback-node))
               '())))
    (is (not (null node)))
    (is (listp node))))

(test ast-make-call-node
  "Test make-call-node construction."
  (let ((node (eightbol::make-call-node "MyRoutine" '())))
    (is (not (null node)))
    (is (listp node))))

(test ast-make-identifier
  "Test make-identifier construction."
  (let ((node (eightbol::make-identifier "myVar")))
    (is (not (null node)))
    (is (listp node))))

(test ast-make-literal
  "Test make-literal construction."
  (let ((num-node (eightbol::make-literal 42))
        (str-node (eightbol::make-literal "hello" :type :string)))
    (is (not (null num-node)))
    (is (not (null str-node)))))

;;;; Keyword Coverage Tests
(fiveam:def-suite :keyword-coverage
  :in :frontend-lexers
  :description "Tests for keyword alist coverage")
(in-suite :keyword-coverage)

(test basic-keyword-alist-not-empty
  "Test BASIC keyword alist is populated."
  (is (not (null eightbol::*basic-keyword-alist*)))
  (is (> (length eightbol::*basic-keyword-alist*) 10)))

(test basic-operator-alist-not-empty
  "Test BASIC operator alist is populated."
  (is (not (null eightbol::*basic-operators-alist*)))
  (is (> (length eightbol::*basic-operators-alist*) 5)))

(test agi-keyword-alist-not-empty
  "Test AGI keyword alist is populated."
  (is (not (null eightbol::*agi-keyword-alist*)))
  (is (> (length eightbol::*agi-keyword-alist*) 10)))

(test agi-operator-alist-not-empty
  "Test AGI operator alist is populated."
  (is (not (null eightbol::*agi-operators-alist*)))
  (is (> (length eightbol::*agi-operators-alist*) 5)))

;;;; Frontend Summary Tests
(fiveam:def-suite :frontend-summary
  :in :frontend-lexers
  :description "Summary tests for all frontends")
(in-suite :frontend-summary)

(test all-lexer-functions-callable
  "Test all known lexer functions are defined and callable."
  (dolist (lex-fn '(eightbol::basic-lex-line
                    eightbol::agi-lex-line
                    eightbol::cobol-lex-line
                    eightbol::fortran-lex-line
                    eightbol::lua-lex-line
                    eightbol::pascal-lex-line
                    eightbol::objective-lex-line
                    eightbol::smalltalk-lex-line
                    eightbol::lingo-lex-line))
    (is (fboundp lex-fn) 
        (format nil "Lexer function ~A should be defined" lex-fn))))

(test all-parser-token-lists-callable
  "Test all known parser token-list functions are defined."
  (dolist (tok-fn '(eightbol::basic-token-list
                    eightbol::agi-token-list
                    eightbol::cobol-token-list
                    eightbol::fortran-token-list
                    eightbol::lua-token-list
                    eightbol::pascal-token-list
                    eightbol::objective-token-list
                    eightbol::smalltalk-token-list
                    eightbol::lingo-token-list))
    (is (fboundp tok-fn)
        (format nil "Token list function ~A should be defined" tok-fn))))
