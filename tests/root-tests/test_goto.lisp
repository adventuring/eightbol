(asdf:load-system :eightbol)
(in-package :eightbol)

(let ((src "000010 IDENTIFICATION DIVISION.
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
000120             GO TO Done.
000130 Loop.
000140             GOBACK.
000150 Done.
000160             GOBACK.
000170         END METHOD \"Think\".
000180         IDENTIFICATION DIVISION.
000190         METHOD-ID. \"Kill\".
000200         PROCEDURE DIVISION.
000210             GOBACK.
000220         END METHOD \"Kill\".
000230 END OBJECT.
000240 END CLASS Character."))
  (format t "Source:~%~a~%~%" src)
  (let ((ast (parse-eightbol-string src)))
    (format t "AST:~%~a~%" ast)
    (let ((method (first (getf (rest ast) :methods))))
      (format t "Method:~%~a~%" method)
      (let ((stmts (getf (rest method) :statements)))
        (format t "Statements:~%")
        (dolist (stmt stmts)
          (format t "  ~a~%" stmt))))))