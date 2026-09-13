;; src/backend-wasm/backend-wasm.lisp — WebAssembly bytecode backend
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defun wasm-symbol (name)
  "Convert EIGHTBOL identifier to WebAssembly symbol (PascalCase)."
  (pascal-case (format nil "~a" name)))

(defun paragraph-label (name)
  "Return WebAssembly label for paragraph NAME."
  (wasm-symbol (format nil "~a" name)))

;;; Top-level entry point

(defmethod compile-to-assembly (ast (cpu (eql :wasm)) output-stream)
  (unless (and (listp ast) (eq (first ast) :program))
    (error "EIGHTBOL/WASM: expected :program AST node, got ~s" (first ast)))
  (let* ((class-id (getf (rest ast) :class-id))
         (methods (getf (rest ast) :methods)))
    (multiple-value-bind (slot-table type-table const-table service-bank-table usage-table sign-table
                          pic-size-table pic-width-table pic-frac-bits-table pic-nybble-semantics-table)
        (load-copybook-tables class-id)
      (let ((*output-stream* output-stream)
            (*class-id* class-id)
            (*slot-table* slot-table)
            (*type-table* type-table)
            (*const-table* const-table)
            (*service-bank-table* service-bank-table)
            (*usage-table* usage-table)
            (*sign-table* sign-table)
            (*pic-size-table* pic-size-table)
            (*pic-width-table* pic-width-table)
            (*pic-frac-bits-table* pic-frac-bits-table)
            (*pic-nybble-semantics-table* pic-nybble-semantics-table))
        (format output-stream "(module~%")
        (format output-stream "  (memory $mem 1)~%")
        (dolist (method (ensure-list methods))
          (when (and (listp method) (eq (first method) :method))
            (compile-wasm-method method)))
        (format output-stream ")~%")))))

;;; Method compilation

(defun compile-wasm-method (method)
  (let ((*method-id* (getf (rest method) :method-id)))
    (declare (special *method-id*))
    (let* ((method-id *method-id*)
           (method-name (wasm-symbol (format nil "~a" method-id)))
           (custom-entry (nth-value 0 (split-method-leading-assembly-entry
                                        (getf (rest method) :statements))))
           (stmts (nth-value 1 (split-method-leading-assembly-entry
                                  (getf (rest method) :statements)))))
      (format *output-stream* "  (func $~a (export \"~a\") (result i32)~%" method-name method-name)
      (format *output-stream* "    (local $result i32)~%")
      (dolist (stmt stmts)
        (compile-statement :wasm (first stmt) (rest stmt)))
      (format *output-stream* "    (local.get $result)~%")
      (format *output-stream* "  )~%~%"))))

;;; Statement dispatch

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :goback)) ast-node-data)
  (format *output-stream* "    (return i32.const 0)~%"))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :exit-method)) ast-node-data)
  (format *output-stream* "    (return i32.const 0)~%"))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :exit-program)) ast-node-data)
  (format *output-stream* "    (return i32.const 0)~%"))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :move)) ast-node-data)
  (let ((from (getf ast-node-data :from))
        (to (getf ast-node-data :to)))
    (compile-wasm-load from)
    (compile-wasm-store to)))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :+)) ast-node-data)
  (let ((from (getf ast-node-data :from))
        (to (getf ast-node-data :to))
        (giving (getf ast-node-data :giving)))
    (compile-wasm-load to)
    (compile-wasm-load from)
    (format *output-stream* "    i32.add~%")
    (compile-wasm-store (or giving to))))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :-)) ast-node-data)
  (let ((from (getf ast-node-data :from))
        (to (getf ast-node-data :to))
        (giving (getf ast-node-data :giving)))
    (compile-wasm-load to)
    (compile-wasm-load from)
    (format *output-stream* "    i32.sub~%")
    (compile-wasm-store (or giving to))))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :compute)) ast-node-data)
  (let ((target (getf ast-node-data :target))
        (expression (getf ast-node-data :expression)))
    (compile-wasm-load expression)
    (compile-wasm-store target)))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :set)) ast-node-data)
  (let ((target (getf ast-node-data :target))
        (value (getf ast-node-data :value)))
    (compile-wasm-load value)
    (compile-wasm-store target)))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :if)) ast-node-data)
  (let ((condition (getf ast-node-data :condition))
        (then-stmts (getf ast-node-data :then))
        (else-stmts (getf ast-node-data :else))
        (else-label (gensym "ELSE"))
        (end-label (gensym "END")))
    (compile-wasm-condition condition else-label)
    (dolist (s (ensure-list then-stmts))
      (compile-statement :wasm (first s) (rest s)))
    (format *output-stream* "    br ~a~%" end-label)
    (format *output-stream* "~a:~%" else-label)
    (dolist (s (ensure-list else-stmts))
      (compile-statement :wasm (first s) (rest s)))
    (format *output-stream* "~a:~%" end-label)))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :call)) ast-node-data)
  (let ((target (getf ast-node-data :target)))
    (format *output-stream* "    call $~a~%" (wasm-symbol (format nil "~a" target)))))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :invoke)) ast-node-data)
  (let ((method (getf ast-node-data :method)))
    (format *output-stream* "    call $Invoke~a~%" (wasm-symbol (format nil "~a" method)))))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :goto)) ast-node-data)
  (let ((target (getf ast-node-data :target)))
    (format *output-stream* "    br ~a~%" (wasm-symbol (format nil "~a" target)))))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :procedure)) ast-node-data)
  (let ((name (first ast-node-data)))
    (when name
      (format *output-stream* "; Procedure: ~a~%" name))))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :comment)) ast-node-data)
  (let ((text (first ast-node-data)))
    (format *output-stream* "; ~a~%" (if (listp text) (princ-to-string text) text))))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :log-fault)) ast-node-data)
  (format *output-stream* "; LOG FAULT ~s~%" (getf ast-node-data :code)))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :debug-break)) ast-node-data)
  (format *output-stream* "; DEBUG BREAK ~s~%" (getf ast-node-data :code)))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :perform)) ast-node-data)
  (let ((procedure (getf ast-node-data :procedure))
        (body (getf ast-node-data :body)))
    (format *output-stream* "; PERFORM ~a~%" procedure)
    (dolist (s (ensure-list body))
      (compile-statement :wasm (first s) (rest s)))))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :evaluate)) ast-node-data)
  (format *output-stream* "; EVALUATE not yet implemented for WASM~%"))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :inspect)) ast-node-data)
  (format *output-stream* "; INSPECT not yet implemented for WASM~%"))

(defmethod compile-statement ((cpu (eql :wasm)) (stmt-type (eql :string-blt)) ast-node-data)
  (format *output-stream* "; STRING BLT not yet implemented for WASM~%"))

;;; Expression loading

(defun compile-wasm-load (expression)
  (cond
    ((numberp expression)
     (format *output-stream* "    i32.const ~d~%" expression))
    ((stringp expression)
     (format *output-stream* "    i32.const 0~%"))
    ((and (listp expression) (eq (first expression) :of))
     (format *output-stream* "    i32.const 0~%"))
    ((and (listp expression) (eq (first expression) :on))
     (format *output-stream* "    i32.const 0~%"))))

(defun compile-wasm-store (destination)
  (cond
    ((stringp destination)
     (format *output-stream* "    i32.store~%"))
    (t
     (format *output-stream* "; Unsupported store ~s for WASM" destination))))

(defun compile-wasm-condition (condition false-label)
  (cond
    ((and (listp condition) (member (first condition) '(= equal) :test #'eq))
     (compile-wasm-load (second condition))
     (compile-wasm-load (third condition))
     (format *output-stream* "    i32.eq~%")
     (format *output-stream* "    i32.eqz~%")
     (format *output-stream* "    br_if ~a~%" false-label))
    ((and (listp condition) (eq (first condition) :is-zero))
     (compile-wasm-load (second condition))
     (format *output-stream* "    i32.eqz~%")
     (format *output-stream* "    br_if ~a~%" false-label))
    ((and (listp condition) (eq (first condition) :is-not-zero))
     (compile-wasm-load (second condition))
     (format *output-stream* "    i32.eqz~%")
     (format *output-stream* "    br_if ~a~%" false-label))
    ((and (listp condition) (member (first condition) '(< less > greater) :test #'eq))
     (compile-wasm-load (second condition))
     (compile-wasm-load (third condition))
     (ecase (first condition)
       ((< less)  (format *output-stream* "    i32.ge_s~%i32.eqz~%br_if ~a~%" false-label))
       ((> greater) (format *output-stream* "    i32.le_s~%i32.eqz~%br_if ~a~%" false-label))))
    ((and (listp condition) (member (first condition) '(:≠ :≤ :≥) :test #'eq))
     (compile-wasm-load (second condition))
     (compile-wasm-load (third condition))
     (ecase (first condition)
       ((:≠) (format *output-stream* "    i32.ne~%i32.eqz~%br_if ~a~%" false-label))
       ((:≤) (format *output-stream* "    i32.gt_s~%i32.eqz~%br_if ~a~%" false-label))
       ((:≥) (format *output-stream* "    i32.lt_s~%i32.eqz~%br_if ~a~%" false-label))))
    (t
     (format *output-stream* "; Unsupported condition ~s for WASM" condition)
     (format *output-stream* "    br ~a~%" false-label))))
