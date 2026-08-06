(in-package :eightbol)

(defun smalltalk-make-parser (parser-name)
  "Create a parser function for SmallTalk with YACC"
  (yacc:make-parser parser-name *smalltalk-parser*))
