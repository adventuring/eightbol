#!/usr/bin/sbcl --script
;;; Test program to create a minimal :program AST and compile it to 6502 assembly
;;; Using quicklisp to load external dependencies

;;; Set up quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init))
  (unless (find-package :quicklisp)
    (error "Quicklisp not found at ~a" quicklisp-init)))

;;; Load required external libraries via quicklisp
(ql:quickload :alexandria :silent t)
(ql:quickload :cl-change-case :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :local-time :silent t)
(ql:quickload :serapeum :silent t)
(ql:quickload :split-sequence :silent t)
(ql:quickload :uiop :silent t)
;;; Note: yacc might not be available via quicklisp, but let's see if we can proceed without it for now

;;; Now load our local files in dependency order
(load "src/package.lisp")
(load "src/conditions.lisp")
(load "src/lexer.lisp")
(load "src/ast.lisp")
(load "src/ast-optimize.lisp")
(load "src/ast-validate.lisp")
;;; Skip parser for now since it needs yacc - we'll create AST manually
(load "src/backend.lisp")

;;; Load 6502 backend files
(load "src/backend-6502/backend-6502-part1.lisp")
(load "src/backend-6502/backend-6502-part2.lisp")
(load "src/backend-6502/backend-6502-part3.lisp")
(load "src/backend-6502/backend-6502-part4.lisp")
(load "src/backend-6502/backend-6502-part5.lisp")
(load "src/backend-6502/backend-6502-part6.lisp")

;;; Load copybook load and eightbol compile
(load "src/copybook-load.lisp")
(load "src/eightbol-compile.lisp")

;;; Now we're in the :eightbol package
;;; Create a minimal :program AST

(defparameter *minimal-program-ast*
  ;; A minimal program with one class containing one method that just returns
  `(:program
    :class-id "TestClass"
    :data ()  ; No data variables
    ;; One method named "Main" that just does GOBACK
    (methods (:method
              :method-id "Main"
              :statements ((:goback))))))

;;; Function to compile a program AST to assembly string for a specific CPU
(defun compile-program-ast-to-string (ast cpu)
  "Compile a :program AST to assembly string for the given CPU.
Returns the assembly as a string."
  (with-output-to-string (out-stream)
    (let* ((*eightbol-root-directory* (truename "."))
           (*copybook-paths* ())) ; Empty copybook paths for simplicity
      ;; Bind the dynamic variables that compile-to-assembly expects
      (let ((*class-id* (getf (rest ast) :class-id))
            (*program-id* nil) ; Not used in this minimal example
            (*slot-table* (make-hash-table :test 'equalp))
            (*type-table* (make-hash-table :test 'equalp))
            (*const-table* (make-hash-table :test 'equalp))
            (*service-bank-table* (make-hash-table :test 'equalp))
            (*usage-table* (make-hash-table :test 'equalp))
            (*sign-table* (make-hash-table :test 'equalp))
            (*pic-size-table* (make-hash-table :test 'equalp))
            (*pic-width-table* (make-hash-table :test 'equalp))
            (*pic-frac-bits-table* (make-hash-table :test 'equalp))
            (*pic-nybble-semantics-table* (make-hash-table :test 'equalp))
            (*working-storage* (make-hash-table :test 'equalp)))
        ;; Actually compile the AST to assembly
        (eightbol::compile-to-assembly ast cpu out-stream)))))

;;; Main execution
(format t "~%Minimal AST:~%")
(write *minimal-program-ast* :pretty t)
(terpri)

(format t "~%Compiling to 6502 assembly...~%")
(let* ((asm-string (compile-program-ast-to-string *minimal-program-ast* :6502)))
  (format t "~%Generated assembly (~a bytes):~%" (length asm-string))
  (format t "~a~%" asm-string)))

;;; Verify it looks like reasonable 6502 assembly
(let* ((asm (compile-program-ast-to-string *minimal-program-ast* :6502)))
  (when (search "Main" asm)
    (format t "~%SUCCESS: Found method label in assembly~%"))
  (when (search "rts" asm :test #'string-equal :start2 (- (length asm) 4))
    (format t "~%SUCCESS: Found RTS instruction~%")))