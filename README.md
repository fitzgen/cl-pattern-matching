CL-Pattern-Matching
===================

`let-match`
===========

    CL-USER> (let-match ((1 2 ?three) '(1 2 3))
               (* ?three ?three))
    9
    T
    CL-USER> (let-match ((1 2 ?three) '(3 2 1))
               (* ?three ?three))
    NIL
    NIL
    CL-USER> (let-match ((1 2 ?three) '(1 2 3))
               nil)
    NIL
    T

`cond-match`
============

    CL-USER> (defvar form nil)
    FORM
    CL-USER> (cond-match form
               (nil "Nil!")
               (1 "One!")
               (?x ?x))
    "Nil!"
    CL-USER> (setf form 1)
    1
    CL-USER> (cond-match form
               (nil "Nil!")
               (1 "One!")
               (?x ?x))
    "One!"
    CL-USER> (setf form "Yo mama!")
    "Yo mama!"
    CL-USER> (cond-match form
               (nil "Nil!")
               (1 "One!")
               (?x ?x))
    "Yo mama!"

`defun-match`
=============

    CL-USER> (defun-match desc-args
               (nil "No args!")
               ((?x) "One argument!")
               ((?x ?y) "Two arguments!")
               ((?x ?y . ?z) "More than two arguments!"))
    DESC-ARGS
    CL-USER> (desc-args)
    "No args!"
    CL-USER> (desc-args 1)
    "One argument!"
    CL-USER> (desc-args 1 2)
    "Two arguments!"
    CL-USER> (desc-args 1 2 3)
    "More than two arguments!"
    CL-USER> (defun-match my-map
               ((?fn nil) nil)
               ((?fn (?car . ?cdr)) (cons (funcall ?fn ?car)
                                          (my-map ?fn ?cdr))))
    MY-MAP
    CL-USER> (my-map #'(lambda (x) (* x x))
                     '(1 2 3 4 5 6))
    (1 4 9 16 25 36)
    CL-USER> (defun-match factorial
               ((0) 1)
               ((?x) (* ?x (factorial (- ?x 1)))))
    FACTORIAL
    CL-USER> (factorial 1)
    1
    CL-USER> (factorial 3)
    6
    CL-USER> (factorial 5)
    120
    CL-USER> (factorial 7)
    5040
