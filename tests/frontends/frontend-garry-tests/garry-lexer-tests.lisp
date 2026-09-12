;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/garry -*-
;;;
;;; Garry frontend lexer tests
;;;

(in-package :eightbol/test/garry)

(test garry-lexer-keywords
  "Garry lexer recognizes GameMaker keywords."
  (let ((tokens (eightbol:garry-lex-line "sprite 3 is pipe")))
    (is (find :sprite (mapcar #'first tokens)))
    (is (find :is (mapcar #'first tokens)))
    (is (find :atom (mapcar #'first tokens)))))

(test garry-lexer-numbers
  "Garry lexer recognizes numbers."
  (let ((tokens (eightbol:garry-lex-line "movement speed = 39")))
    (is (find :number (mapcar #'first tokens)))
    (is (find "39" (mapcar #'rest tokens) :test #'string=))))

(test garry-lexer-operators
  "Garry lexer recognizes operators."
  (let ((tokens (eightbol:garry-lex-line "volume = 8")))
    (is (find :equal (mapcar #'first tokens)))))

(test garry-lexer-comments
  "Garry lexer skips REM comments."
  (let ((tokens (eightbol:garry-lex-line "REM this is a comment")))
    (is (null tokens))))
