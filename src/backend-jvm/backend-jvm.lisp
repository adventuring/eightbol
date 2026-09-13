;; src/backend-jvm/backend-jvm.lisp — Java Virtual Machine bytecode backend
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defun jvm-symbol (name)
  "Convert EIGHTBOL identifier to JVM class/method symbol (PascalCase)."
  (pascal-case (format nil "~a" name)))

(defun paragraph-label (name)
  "Return JVM method descriptor for paragraph NAME."
  (jvm-symbol (format nil "~a" name)))

;;; Top-level entry point

(defmethod compile-to-assembly (ast (cpu (eql :jvm)) output-stream)
  (unless (and (listp ast) (eq (first ast) :program))
    (error "EIGHTBOL/JVM: expected :program AST node, got ~s" (first ast)))
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
        (format output-stream ".class public ~a~%" class-id)
        (format output-stream ".super java/lang/Object~%~%")
        (format output-stream ".method public <init>()V~%")
        (format output-stream "  aload_0~%")
        (format output-stream "  invokespecial java/lang/Object.<init>()V~%")
        (format output-stream "  return~%")
        (format output-stream ".end method~%~%")
        (dolist (method (ensure-list methods))
          (when (and (listp method) (eq (first method) :method))
            (compile-jvm-method method)))))))

;;; Method compilation

(defun compile-jvm-method (method)
  (let ((*method-id* (getf (rest method) :method-id)))
    (declare (special *method-id*))
    (let* ((method-id *method-id*)
           (method-name (jvm-symbol (format nil "~a" method-id)))
           (custom-entry (nth-value 0 (split-method-leading-assembly-entry
                                       (getf (rest method) :statements))))
           (stmts (nth-value 1 (split-method-leading-assembly-entry
                                (getf (rest method) :statements)))))
      (format *output-stream* ".method public static ~a()V~%" method-name)
      (dolist (stmt stmts)
        (compile-statement :jvm (first stmt) (rest stmt)))
      (format *output-stream* "  return~%")
      (format *output-stream* ".end method~%~%"))))

;;; Statement dispatch

(defmethod compile-statement ((cpu (eql :jvm)) stmt-type ast-node-data)
  (let ((statement (cons stmt-type ast-node-data)))
    (declare (ignorable statement))
    (emit-statement-source-comment *output-stream* statement)
    (call-next-method)))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :goback)) ast-node-data)
  (format *output-stream* "  return~%"))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :exit-method)) ast-node-data)
  (format *output-stream* "  return~%"))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :exit-program)) ast-node-data)
  (format *output-stream* "  return~%"))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :move)) ast-node-data)
  (let ((from (getf ast-node-data :from))
        (to (getf ast-node-data :to)))
    (compile-jvm-load from)
    (compile-jvm-store to)))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :+)) ast-node-data)
  (let ((from (getf ast-node-data :from))
        (to (getf ast-node-data :to))
        (giving (getf ast-node-data :giving)))
    (compile-jvm-load to)
    (compile-jvm-load from)
    (format *output-stream* "  iadd~%")
    (compile-jvm-store (or giving to))))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :-)) ast-node-data)
  (let ((from (getf ast-node-data :from))
        (to (getf ast-node-data :to))
        (giving (getf ast-node-data :giving)))
    (compile-jvm-load to)
    (compile-jvm-load from)
    (format *output-stream* "  isub~%")
    (compile-jvm-store (or giving to))))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :compute)) ast-node-data)
  (let ((target (getf ast-node-data :target))
        (expression (getf ast-node-data :expression)))
    (compile-jvm-load expression)
    (compile-jvm-store target)))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :set)) ast-node-data)
  (let ((target (getf ast-node-data :target))
        (value (getf ast-node-data :value)))
    (compile-jvm-load value)
    (compile-jvm-store target)))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :if)) ast-node-data)
  (let ((condition (getf ast-node-data :condition))
        (then-stmts (getf ast-node-data :then))
        (else-stmts (getf ast-node-data :else))
        (else-label (gensym "ELSE"))
        (end-label (gensym "END")))
    (compile-jvm-condition condition else-label)
    (dolist (s (ensure-list then-stmts))
      (compile-statement :jvm (first s) (rest s)))
    (format *output-stream* "  goto ~a~%" end-label)
    (format *output-stream* "~a:~%" else-label)
    (dolist (s (ensure-list else-stmts))
      (compile-statement :jvm (first s) (rest s)))
    (format *output-stream* "~a:~%" end-label)))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :call)) ast-node-data)
  (let ((target (getf ast-node-data :target)))
    (format *output-stream* "  invokestatic ~a/~a()V~%" (jvm-symbol *class-id*) (jvm-symbol target))))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :invoke)) ast-node-data)
  (let ((method (getf ast-node-data :method)))
    (format *output-stream* "  invokestatic ~a/Invoke~a()V~%"
            (jvm-symbol *class-id*) (jvm-symbol method))))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :goto)) ast-node-data)
  (let ((target (getf ast-node-data :target)))
    (format *output-stream* "  goto ~a~%" (jvm-symbol (format nil "~a" target)))))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :procedure)) ast-node-data)
  (let ((name (first ast-node-data)))
    (when name
      (format *output-stream* "~%; Procedure: ~a~%" name))))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :comment)) ast-node-data)
  (let ((text (first ast-node-data)))
    (format *output-stream* "; ~a~%" (if (listp text) (princ-to-string text) text))))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :log-fault)) ast-node-data)
  (format *output-stream* "; LOG FAULT ~s~%" (getf ast-node-data :code)))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :debug-break)) ast-node-data)
  (format *output-stream* "; DEBUG BREAK ~s~%" (getf ast-node-data :code)))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :perform)) ast-node-data)
  (let ((procedure (getf ast-node-data :procedure))
        (body (getf ast-node-data :body)))
    (format *output-stream* "; PERFORM ~a~%" procedure)
    (dolist (s (ensure-list body))
      (compile-statement :jvm (first s) (rest s)))))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :evaluate)) ast-node-data)
  (format *output-stream* "; EVALUATE not yet implemented for JVM~%"))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :inspect)) ast-node-data)
  (format *output-stream* "; INSPECT not yet implemented for JVM~%"))

(defmethod compile-statement ((cpu (eql :jvm)) (stmt-type (eql :string-blt)) ast-node-data)
  (format *output-stream* "; STRING BLT not yet implemented for JVM~%"))

;;; Expression loading

(defun compile-jvm-load (expression)
  (cond
    ((numberp expression)
     (format *output-stream* "  ldc ~d~%" expression))
    ((stringp expression)
     (format *output-stream* "  ldc \"~a\"~%" expression))
    ((and (listp expression) (eq (first expression) :of))
     (format *output-stream* "; OF expression~%"))
    (t
     (format *output-stream* "  ldc 0~%"))))

(defun compile-jvm-store (destination)
  (cond
    ((stringp destination)
     (format *output-stream* "  putstatic ~a/~a I~%" (jvm-symbol *class-id*) (jvm-symbol destination)))
    (t
     (format *output-stream* "; Unsupported store ~s for JVM" destination))))

(defun compile-jvm-condition (condition false-label)
  (cond
    ((and (listp condition) (member (first condition) '(= equal) :test #'eq))
     (compile-jvm-load (second condition))
     (compile-jvm-load (third condition))
     (format *output-stream* "  if_icmpne ~a~%" false-label))
    ((and (listp condition) (eq (first condition) :is-zero))
     (compile-jvm-load (second condition))
     (format *output-stream* "  ifne ~a~%" false-label))
    ((and (listp condition) (eq (first condition) :is-not-zero))
     (compile-jvm-load (second condition))
     (format *output-stream* "  ifeq ~a~%" false-label))
    (t
     (format *output-stream* "; Unsupported condition ~s for JVM" condition)
     (format *output-stream* "  goto ~a~%" false-label))))