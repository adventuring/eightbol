;; src/backend-forth/backend-forth-tables.lisp — Forth bytecode symbol tables
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;
;; Symbol table management for variable and constant resolution.
;; During bytecode emission, references are marked as placeholders
;; and resolved by a linker pass.
(in-package :eightbol)

;;; Runtime symbol table for Forth bytecode
(defstruct forth-symbol
  "Forth bytecode symbol (variable or constant).

OFFSET: Memory offset or constant value.
TYPE:   :variable or :constant.
SIZE:   Byte width (1 or 2).
NAME:   Symbol name (PascalCase)."
  name
  offset
  type
  size)

(defvar *forth-symbol-table* (make-hash-table :test 'equal)
  "Global symbol table for Forth bytecode backend.")

;;; Symbol table initialization
(defun forth-init-symbol-table ()
  "Initialize the Forth symbol table for the current class.

Populates from *slot-table*, *const-table*, and *pic-width-table*."
  (clrhash *forth-symbol-table*)
  
  ;; Add variables from *slot-table*
  (when *slot-table*
    (maphash (lambda (name slot-info)
               (let ((size (or (gethash name *pic-width-table*) 2)))
                 (setf (gethash name *forth-symbol-table*)
                       (make-forth-symbol :name (forth-symbol name)
                                         :offset 0  ; TBD by linker
                                         :type :variable
                                         :size size))))
             *slot-table*))
  
  ;; Add constants from *const-table*
  (when *const-table*
    (maphash (lambda (name const-value)
               (setf (gethash name *forth-symbol-table*)
                     (make-forth-symbol :name (forth-symbol name)
                                       :offset const-value
                                       :type :constant
                                       :size 2)))
             *const-table*)))

;;; Symbol lookup
(defun forth-lookup-symbol (name)
  "Look up symbol NAME in table.

Returns forth-symbol structure or NIL if not found."
  (gethash name *forth-symbol-table*))

(defun forth-lookup-variable (name)
  "Look up variable NAME.

Returns offset or signals error if not found or not a variable."
  (let ((symbol (forth-lookup-symbol name)))
    (unless symbol
      (error "EIGHTBOL/FORTH: variable ~s not found" name))
    (unless (eq (forth-symbol-type symbol) :variable)
      (error "EIGHTBOL/FORTH: ~s is not a variable" name))
    (forth-symbol-offset symbol)))

(defun forth-lookup-constant (name)
  "Look up constant NAME.

Returns value or signals error if not found or not a constant."
  (let ((symbol (forth-lookup-symbol name)))
    (unless symbol
      (error "EIGHTBOL/FORTH: constant ~s not found" name))
    (unless (eq (forth-symbol-type symbol) :constant)
      (error "EIGHTBOL/FORTH: ~s is not a constant" name))
    (forth-symbol-offset symbol)))

;;; Symbol allocation
(defvar *forth-next-variable-offset* 0
  "Next available memory offset for Forth variables.")

(defun forth-allocate-variable (name &optional (size 2))
  "Allocate memory for variable NAME with given SIZE.

Returns offset. Updates *forth-next-variable-offset*."
  (let ((offset *forth-next-variable-offset*))
    (setf (gethash name *forth-symbol-table*)
          (make-forth-symbol :name (forth-symbol name)
                            :offset offset
                            :type :variable
                            :size size))
    (incf *forth-next-variable-offset* size)
    offset))

(defun forth-define-constant (name value)
  "Define constant NAME with VALUE.

Adds to symbol table without allocating memory."
  (setf (gethash name *forth-symbol-table*)
        (make-forth-symbol :name (forth-symbol name)
                          :offset value
                          :type :constant
                          :size 2)))

;;; Symbol table introspection
(defun forth-all-variables ()
  "Return list of all variable symbols."
  (let ((vars '()))
    (maphash (lambda (name sym)
               (when (eq (forth-symbol-type sym) :variable)
                 (push sym vars)))
             *forth-symbol-table*)
    vars))

(defun forth-all-constants ()
  "Return list of all constant symbols."
  (let ((consts '()))
    (maphash (lambda (name sym)
               (when (eq (forth-symbol-type sym) :constant)
                 (push sym consts)))
             *forth-symbol-table*)
    consts))

(defun forth-symbol-table-size ()
  "Return total memory required for all variables."
  *forth-next-variable-offset*)

;;; Symbol table export for linker
(defun forth-emit-symbol-table ()
  "Emit symbol table as comments for documentation."
  (format *output-stream* "~%;; Symbol table:~%")
  (dolist (var (forth-all-variables))
    (format *output-stream*
            ";;   ~a: offset ~d (size ~d)~%"
            (forth-symbol-name var)
            (forth-symbol-offset var)
            (forth-symbol-size var)))
  (dolist (const (forth-all-constants))
    (format *output-stream*
            ";;   ~a = ~d~%"
            (forth-symbol-name const)
            (forth-symbol-offset const))))
