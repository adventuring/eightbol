#!/usr/bin/env clisp
;;; Test program to create a minimal :program AST and compile it to 6502 assembly

(load "src/eightbol-compile.lisp")
(load "src/backend.lisp")
(load "src/backend-6502/backend-6502-part1.lisp")
(load "src/backend-6502/backend-6502-part2.lisp")

;;; Create a minimal :program AST
;;; Based on the AST definition in src/ast.lisp:
;;; Program:   (:program  :class-id "Character" :data (...) :methods (...))

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
  (format t "~%Generated assembly:~%")
  (format t "~a~%" asm-string)))

;;; Verify it looks like reasonable 6502 assembly
(when (search "Main" (compile-program-ast-to-string *minimal-program-ast* :6502))
  (format t "~%SUCCESS: Found method label in assembly~%"))
(when (search "rts" (compile-program-ast-to-string *minimal-program-ast* :6502) :test #'string-equal :start2 (- (length (compile-program-ast-to-string *minimal-program-ast* :6502)) 4))
  (format t "~%SUCCESS: Found RTS instruction~%"))