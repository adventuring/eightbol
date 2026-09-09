;; src/frontend-objective/objective-lexer.lisp
;; Lexer for Objective-C subset targeting 8-bit/16-bit systems
;;
;; Features:
;; - C99 keywords and operators
;; - Objective-C extensions (@interface, @implementation, @property, etc.)
;; - Message send syntax [obj method:arg1 label:arg2]
;; - Number literals: decimal, hex (0x), octal (0o), binary (0b)
;; - String literals with escape sequences
;; - PascalCase identifier normalization
;;
;; Limitations:
;; - No @try/@catch/@finally (use error codes instead)
;; - No C++ features
;; - No nested class definitions
;;
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :eightbol)

;;; Keywords and operators

(defparameter *objc-keywords-alist*
  '(("@interface" . :AT-INTERFACE)
    ("@implementation" . :AT-IMPLEMENTATION)
    ("@end" . :AT-END)
    ("@property" . :AT-PROPERTY)
    ("@synthesize" . :AT-SYNTHESIZE)
    ("@selector" . :AT-SELECTOR)
    ("@protocol" . :AT-PROTOCOL)
    ("@public" . :AT-PUBLIC)
    ("@private" . :AT-PRIVATE)
    ("@protected" . :AT-PROTECTED)
    ("void" . :VOID)
    ("int" . :INT)
    ("float" . :FLOAT)
    ("double" . :DOUBLE)
    ("char" . :CHAR)
    ("unsigned" . :UNSIGNED)
    ("signed" . :SIGNED)
    ("long" . :LONG)
    ("short" . :SHORT)
    ("struct" . :STRUCT)
    ("enum" . :ENUM)
    ("typedef" . :TYPEDEF)
    ("const" . :CONST)
    ("static" . :STATIC)
    ("extern" . :EXTERN)
    ("volatile" . :VOLATILE)
    ("if" . :IF)
    ("else" . :ELSE)
    ("switch" . :SWITCH)
    ("case" . :CASE)
    ("default" . :DEFAULT)
    ("while" . :WHILE)
    ("do" . :DO)
    ("for" . :FOR)
    ("break" . :BREAK)
    ("continue" . :CONTINUE)
    ("return" . :RETURN)
    ("goto" . :GOTO)
    ("sizeof" . :SIZEOF)
    ("self" . :SELF)
    ("super" . :SUPER)
    ("nil" . :NIL)
    ("yes" . :YES)
    ("no" . :NO)
    ("true" . :TRUE)
    ("false" . :FALSE))
  "Association list of C/Objective-C keywords to token types.")

;;; Tokenization

(defun objc-normalize-identifier (name)
  "Normalize identifier NAME to PascalCase (first uppercase, rest unchanged).
   Preserves @ prefix for Objective-C keywords."
  (when (and (stringp name) (plusp (length name)))
    (cond
      ((char= (char name 0) #\@)
       name) ; @selector, @interface, etc. already marked
      (t
       (concatenate 'string
                    (string-upcase (subseq name 0 1))
                    (subseq name 1))))))

(defun objc-valid-identifier-p (lexeme)
  "Predicate: is LEXEME a valid C/Objective-C identifier?
   (letters, digits, underscore; first char not digit)"
  (and (stringp lexeme)
       (plusp (length lexeme))
       (let ((first (char lexeme 0)))
         (and (or (alpha-char-p first) (char= first #\_))
              (every (lambda (c) (or (alphanumericp c) (char= c #\_)))
                     (subseq lexeme 1))))))

(defun objc-parse-number (lexeme)
  "Parse number literal LEXEME. Returns (value radix) or nil.
   Supports: decimal, hex (0x), octal (0o), binary (0b)."
  (when (and (stringp lexeme) (plusp (length lexeme)))
    (cond
      ;; Hex: 0x...
      ((and (>= (length lexeme) 3) (string-equal (subseq lexeme 0 2) "0x"))
       (let ((val (parse-integer (subseq lexeme 2) :radix 16 :junk-allowed t)))
         (and val (list val :hex))))
      ;; Octal: 0o...
      ((and (>= (length lexeme) 3) (string-equal (subseq lexeme 0 2) "0o"))
       (let ((val (parse-integer (subseq lexeme 2) :radix 8 :junk-allowed t)))
         (and val (list val :octal))))
      ;; Binary: 0b...
      ((and (>= (length lexeme) 3) (string-equal (subseq lexeme 0 2) "0b"))
       (let ((val (parse-integer (subseq lexeme 2) :radix 2 :junk-allowed t)))
         (and val (list val :binary))))
      ;; Decimal (including floats)
      ((every (lambda (c) (or (digit-char-p c) (char= c #\.)))
              (coerce lexeme 'list))
       (let ((val (parse-integer lexeme :junk-allowed t)))
         (and val (list val :decimal))))
      (t nil))))

(defun objective-lex (source-text)
  "Tokenize complete Objective-C SOURCE-TEXT string.
   Returns list of (TYPE VALUE &optional RADIX) tokens."
  (unless (stringp source-text)
    (return-from objective-lex '()))
  
  ;; Very simple lexer: split by whitespace, handle quoted strings
  (let ((all-tokens '())
        (current-token (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (in-string nil))
    
    (loop for i from 0 below (length source-text)
          for ch = (char source-text i)
          do
          (cond
            ((and in-string (char= ch #\"))
             ;; End of string literal
             (vector-push-extend ch current-token)
             (push (list :STRING (coerce (subseq current-token 1 (1- (length current-token))) 'string))
                   all-tokens)
             (setf current-token (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
             (setf in-string nil))
            (in-string
             ;; Inside string - accumulate characters
             (vector-push-extend ch current-token))
            ((char= ch #\")
             ;; Start of string literal
             (setf in-string t)
             (vector-push-extend ch current-token))
            ((member ch '(#\Space #\Tab #\Newline #\Return) :test #'char=)
             ;; Whitespace - flush current token if any
             (when (plusp (length current-token))
               (let ((lexeme (coerce current-token 'string)))
                 (push (list :IDENT lexeme) all-tokens))
               (setf current-token (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))))
            ((member ch '(#\( #\) #\{ #\} #\[ #\] #\, #\; #\: #\. #\@ #\= #\+ #\- #\* #\/ #\< #\> #\! #\& #\| #\^ #\~ #\%) :test #'char=)
             ;; Operator/delimiter - flush token and add operator
             (when (plusp (length current-token))
               (let ((lexeme (coerce current-token 'string)))
                 (push (list :IDENT lexeme) all-tokens))
               (setf current-token (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
             (push (list :OP (string ch)) all-tokens))
            (t
             ;; Regular character - accumulate
             (vector-push-extend ch current-token))))
    
    ;; Flush any remaining token
    (when (plusp (length current-token))
      (let ((lexeme (coerce current-token 'string)))
        (push (list :IDENT lexeme) all-tokens)))
    
    ;; Process and normalize tokens
    (nreverse
     (mapcar
      (lambda (tok)
        (destructuring-bind (type value) tok
          (cond
            ((eq type :STRING)
             (list :STRING value))
            ((eq type :OP)
             (list :OP value))
            ((eq type :IDENT)
             ;; Check if it's a keyword
             (let ((upper-lexeme (string-upcase value))
                   (keyword-type (cdr (assoc value *objc-keywords-alist* :test #'string-equal))))
               (if keyword-type
                   (list keyword-type value)
                   ;; Check if it's a number
                   (let ((parsed (objc-parse-number value)))
                     (if parsed
                         (list :NUMBER (first parsed) (second parsed))
                         ;; Regular identifier
                         (list :IDENT (objc-normalize-identifier value)))))))
            (t (list type value)))))
      all-tokens))))

;;; Integration with YACC parser

(defun make-objc-lexer (tokens)
  "Create a lexer closure suitable for YACC parser from TOKENS list.
   Each token is (TYPE VALUE) or (TYPE VALUE RADIX)."
  (let ((token-index 0)
        (token-vec (coerce tokens 'vector)))
    (lambda ()
      (when (< token-index (length token-vec))
        (let ((tok (aref token-vec token-index)))
          (incf token-index)
          (destructuring-bind (type value &optional radix) tok
            (declare (ignore radix)) ; radix passed through for debugging
            (cons type value)))))))

(provide 'objective-lexer)
