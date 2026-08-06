(in-package :eightbol)

(defun lingo-make-parser (parser-name)
  "Create a parser function for Lingo with YACC"
  (yacc:make-parser parser-name *lingo-parser*))