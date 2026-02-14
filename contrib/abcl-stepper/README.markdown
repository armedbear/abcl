ABCL-STEPPER
============

ABCL-STEPPER provides a working implementation of an stepper as a replacement for the empty cl:step, you can get more documentation in the related paper presented in the European Lisp Symposium (ELS) 2023, see https://zenodo.org/record/7815887

Some characteristics:

- For intepreted code, it won't step into compiled functions

- It is ready to use from a plain REPL and from SLIME.

- In general it doesn't handle unexpected conditions in the code to step, if the the code to step fails the stepper will fail too

':?' will print a minimal help (you can type :help)

':i' can inspect variables and symbols (case-insensitive when inspecting, you can also type :inspect)

':c' will resume the evaluation until the end without the stepper (you can type :continue)

':s' will resume the evaluation until the next form to be analyzed (you can type :step)

':sn' will to step to the next form

':l' will show the local bindings for variables and functions
in the current environment passed to the current form to evaluate (you can type :locals)

':b' will add a breakpoint to a symbol to use with next (n) (you can type :br+ or :add-breakpoint)

':r' will remove an existent symbol breakpoint to use with next (n) (you can type :br- or :remove-breakpoint)

':d' will remove all existent symbol breakpoints to use with next (n) (you can type :br! or :delete-breakpoints)

':w' (or :watch) allows to pin binding to see in all steps

':u' (or :unwatch) allows to remove the bindings established by :watch

':bt' (or :backtrace) shows the current backtrace

':q': The quit q feature will abort the evaluation in the stepper
and return NIL. This is useful to avoid running the remaining (you can type :quit)
forms in the code when the user wants to leave the
stepper, specially if the rest of the program is doing costly
operations.

:'n' allows to jump the next (n) symbol:
The next n feature allow to stop the stepper only when the
interpreter is analyzing one of the symbols specified in the
list of stepper::*stepper-stop-symbols* or any of the exported
symbols presented in any of the list of packages specified in
stepper::*stepper-stop-packages*. These variables will have
initially the value NIL and if they are not modified, next will
behave exactly as continue. It is useful when we want to
step large or complex code and avoid stepping every form in
order to jump only to the interested ones.

Usage:

Attaching a sample session to illustrate the use of the stepper

```
CL-USER(1): (require :asdf)
NIL
CL-USER(2): (require :abcl-contrib)
NIL
CL-USER(3): (require :abcl-stepper)
NIL
CL-USER(4): (defparameter *some-var* 1)
*SOME-VAR*
CL-USER(5): (defun test ()
  (let ((*some-var* nil)
        (x 3))
    (list *some-var* 3)))
TEST
CL-USER(6): (stepper:step (test))
We are in the stepper mode
Evaluating step 1 -->
(TEST)
Type ':?' for a list of options
:i
Type the name of the symbol: *some-var*
1
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 2 -->
(BLOCK TEST
  (LET ((*SOME-VAR* NIL) (X 3))
    (LIST *SOME-VAR* 3)))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 3 -->
(LET ((*SOME-VAR* NIL) (X 3))
  (LIST *SOME-VAR* 3))
Type ':?' for a list of options
:?
Type ':l' to see the values of bindings on the local environment
Type ':c' to resume the evaluation until the end without the stepper
Type ':n' to resume the evaluation until the next form previously selected to step in
Type ':s' to step into the form
Type ':i' to inspect the current value of a variable or symbol
Type ':b' to add a symbol as a breakpoint to use with next (n)
Type ':r' to remove a symbol used as a breakpoint with next (n)
Type ':d' to remove all breakpoints used with next (n)
Type ':w' to print the value of a binding in all the steps (watch)
Type ':u' to remove a watched binding (unwatch)
Type ':bt' to show the backtrace
Type ':q' to quit the evaluation and return NIL
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 4 -->
(LIST *SOME-VAR* 3)
Type ':?' for a list of options
:l
Showing the values of variable bindings.
From inner to outer scopes:
X=3
*SOME-VAR*=NIL
Showing the values of function bindings.
From inner to outer scopes:
Type ':?' for a list of options
:i
Type the name of the symbol: x
3
Type ':?' for a list of options
:i
Type the name of the symbol: *some-var*
NIL
Type ':?' for a list of options
:s
step 4 ==> value: (NIL 3)
step 3 ==> value: (NIL 3)
step 2 ==> value: (NIL 3)
step 1 ==> value: (NIL 3)
(NIL 3)
CL-USER(7): (stepper:step (flet ((flet1 (n) (+ n n)))
        (flet ((flet2 (n) (+ 2 (flet1 n))))
          (flet2 2))))
We are in the stepper mode
Evaluating step 1 -->
(FLET ((FLET1 (N) (+ N N)))
  (FLET ((FLET2 (N) (+ 2 (FLET1 N)))) (FLET2 2)))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 2 -->
(FLET ((FLET2 (N) (+ 2 (FLET1 N)))) (FLET2 2))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 3 -->
((FLET FLET2) 2)
Type ':?' for a list of options
:l
Showing the values of variable bindings.
From inner to outer scopes:
Showing the values of function bindings.
From inner to outer scopes:
FLET2=#<FUNCTION #<(FLET FLET2) {152C83E7}> {152C83E7}>
FLET1=#<FUNCTION #<(FLET FLET1) {7AA67C0B}> {7AA67C0B}>
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 4 -->
(BLOCK FLET2 (+ 2 (FLET1 N)))
Type ':?' for a list of options
:l
Showing the values of variable bindings.
From inner to outer scopes:
N=2
Showing the values of function bindings.
From inner to outer scopes:
FLET1=#<FUNCTION #<(FLET FLET1) {7AA67C0B}> {7AA67C0B}>
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 5 -->
(+ 2 (FLET1 N))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 6 -->
((FLET FLET1) N)
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 7 -->
(BLOCK FLET1 (+ N N))
Type ':?' for a list of options
:c
step 7 ==> value: 4
step 6 ==> value: 4
step 5 ==> value: 6
step 4 ==> value: 6
step 3 ==> value: 6
step 2 ==> value: 6
step 1 ==> value: 6
6
CL-USER(8): (stepper:step (progn
        ((lambda (c d) (list c d)) 3 7)))
We are in the stepper mode
Evaluating step 1 -->
(PROGN ((LAMBDA (C D) (LIST C D)) 3 7))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 2 -->
(#<FUNCTION #<FUNCTION (LAMBDA (C D)) {22A1D243}> {22A1D243}> 3 7)
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 3 -->
(LIST C D)
Type ':?' for a list of options
:i
Type the name of the symbol: c
3
Type ':?' for a list of options
:i
Type the name of the symbol: d
7
Type ':?' for a list of options
:l
Showing the values of variable bindings.
From inner to outer scopes:
D=7
C=3
Showing the values of function bindings.
From inner to outer scopes:
Type ':?' for a list of options
:s
step 3 ==> value: (3 7)
step 2 ==> value: (3 7)
step 1 ==> value: (3 7)
(3 7)
CL-USER(9): (stepper:step (let ((a 1))  ;; for skip(q) feature, it should return NIl anyhow
        (block whatever (list 1 2))
        a))
We are in the stepper mode
Evaluating step 1 -->
(LET ((A 1))
  (BLOCK WHATEVER (LIST 1 2))
  A)
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 2 -->
(BLOCK WHATEVER (LIST 1 2))
Type ':?' for a list of options
:l
Showing the values of variable bindings.
From inner to outer scopes:
A=1
Showing the values of function bindings.
From inner to outer scopes:
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 3 -->
(LIST 1 2)
Type ':?' for a list of options
:q
NIL
CL-USER(10): (stepper:step (let ((a 1))  ;; for skip(q) feature, it should return NIl anyhow
        (block whatever (list 1 2))
        a))
We are in the stepper mode
Evaluating step 1 -->
(LET ((A 1))
  (BLOCK WHATEVER (LIST 1 2))
  A)
Type ':?' for a list of options
:c
step 1 ==> value: 1
1
CL-USER(11): (stepper:step (let ((a 1))
        (let ((a 2) (b 1))
          (- a b)) ;; <-- list locals
        (+ a 3 7)))
We are in the stepper mode
Evaluating step 1 -->
(LET ((A 1))
  (LET ((A 2) (B 1))
    (- A B))
  (+ A 3 7))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 2 -->
(LET ((A 2) (B 1))
  (- A B))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 3 -->
(- A B)
Type ':?' for a list of options
:l
Showing the values of variable bindings.
From inner to outer scopes:
B=1
A=2
A=1
Showing the values of function bindings.
From inner to outer scopes:
Type ':?' for a list of options
:s
step 3 ==> value: 1
step 2 ==> value: 1
We are in the stepper mode
Evaluating step 4 -->
(+ A 3 7)
Type ':?' for a list of options
:l
Showing the values of variable bindings.
From inner to outer scopes:
A=1
Showing the values of function bindings.
From inner to outer scopes:
Type ':?' for a list of options
:s
step 4 ==> value: 11
step 1 ==> value: 11
11
CL-USER(12): (stepper:step (progn (defparameter *azf* 1)))
We are in the stepper mode
Evaluating step 1 -->
(PROGN (DEFPARAMETER *AZF* 1))
Type ':?' for a list of options
:c
step 1 ==> value: *AZF*
*AZF*
CL-USER(13): (assert (= *azf* 1))
NIL
CL-USER(14): (defpackage step-next (:use :cl))
#<PACKAGE STEP-NEXT>
CL-USER(15): (in-package :step-next)
#<PACKAGE STEP-NEXT>
STEP-NEXT(16): (defun loop-1 (a b)
  (loop :for i :below a
        :collect (list a b)))
LOOP-1
STEP-NEXT(17): (defun loop-2 (a)
  (loop :for i :below a
        :collect i))
LOOP-2
STEP-NEXT(18): (defun loop-3 (n &optional (times 1))
  (loop :for i :below times
        :collect times))
LOOP-3
STEP-NEXT(19): (defun test-next (n)
  (loop-1 (1+ n) n)
  (loop-2 (1- n))
  (loop-3 n 3)
  ;; quit (q) here
  (defparameter *test-next-var*
    (loop :for i :below (expt 10 6)
          :collect i)))
TEST-NEXT
STEP-NEXT(20): (push 'loop-1 stepper::*stepper-stop-symbols*)
(LOOP-1)
STEP-NEXT(21): (export 'loop-3)
T
STEP-NEXT(22): (push 'step-next stepper::*stepper-stop-packages*)
(STEP-NEXT)
STEP-NEXT(23): (stepper:step (test-next 7))
We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:n
We are in the stepper mode
Evaluating step 2 -->
(LOOP-1 (1+ N) N)
Type ':?' for a list of options
:n
step 2 ==> value: ((8 7) (8 7) (8 7) (8 7) (8 7) (8 7) (8 7) (8 7))
We are in the stepper mode
Evaluating step 3 -->
(LOOP-3 N 3)
Type ':?' for a list of options
:q
NIL
STEP-NEXT(24): (assert (not (boundp '*test-next-var*)))
NIL
STEP-NEXT(25): (stepper:step (test-next 7))
We are in the stepper mode
Evaluating step 1 -->
(TEST-NEXT 7)
Type ':?' for a list of options
:r
Type the name of the breakpoint symbol to remove: loop-1
Type ':?' for a list of options
:b
Type the name of the symbol to use as a breakpoint with next (n): loop-2
Type ':?' for a list of options
:n
We are in the stepper mode
Evaluating step 2 -->
(LOOP-2 (1- N))
Type ':?' for a list of options
:n
step 2 ==> value: (0 1 2 3 4 5)
We are in the stepper mode
Evaluating step 3 -->
(LOOP-3 N 3)
Type ':?' for a list of options
:c
step 3 ==> value: (3 3 3)
step 1 ==> value: *TEST-NEXT-VAR*
*TEST-NEXT-VAR*
STEP-NEXT(26): (defun test-watch ()
  (let ((x 1))
    (dotimes (i 7)
      (incf x))
    x))
TEST-WATCH
STEP-NEXT(27): (stepper:step (test-watch))
We are in the stepper mode
Evaluating step 1 -->
(TEST-WATCH)
Type ':?' for a list of options
:w
Type the name of the symbol to watch: x
Type ':?' for a list of options
Watched bindings:
Couldn't find a value for symbol X
:s
We are in the stepper mode
Evaluating step 2 -->
(BLOCK TEST-WATCH
  (LET ((X 1))
    (DOTIMES (I 7) (SETQ X (+ X 1)))
    X))
Type ':?' for a list of options
Watched bindings:
Couldn't find a value for symbol X
:s
We are in the stepper mode
Evaluating step 3 -->
(LET ((X 1))
  (DOTIMES (I 7) (SETQ X (+ X 1)))
  X)
Type ':?' for a list of options
Watched bindings:
Couldn't find a value for symbol X
:s
We are in the stepper mode
Evaluating step 4 -->
(DOTIMES (I 7) (SETQ X (+ X 1)))
Type ':?' for a list of options
Watched bindings:
X=1
:s
We are in the stepper mode
Evaluating step 5 -->
(SETQ X (+ X 1))
Type ':?' for a list of options
Watched bindings:
X=1
:s
We are in the stepper mode
Evaluating step 6 -->
(+ X 1)
Type ':?' for a list of options
Watched bindings:
X=1
:s
step 6 ==> value: 2
step 5 ==> value: 2
We are in the stepper mode
Evaluating step 7 -->
(SETQ X (+ X 1))
Type ':?' for a list of options
Watched bindings:
X=2
:s
We are in the stepper mode
Evaluating step 8 -->
(+ X 1)
Type ':?' for a list of options
Watched bindings:
X=2
:s
step 8 ==> value: 3
step 7 ==> value: 3
We are in the stepper mode
Evaluating step 9 -->
(SETQ X (+ X 1))
Type ':?' for a list of options
Watched bindings:
X=3
:s
We are in the stepper mode
Evaluating step 10 -->
(+ X 1)
Type ':?' for a list of options
Watched bindings:
X=3
:s
step 10 ==> value: 4
step 9 ==> value: 4
We are in the stepper mode
Evaluating step 11 -->
(SETQ X (+ X 1))
Type ':?' for a list of options
Watched bindings:
X=4
:s
We are in the stepper mode
Evaluating step 12 -->
(+ X 1)
Type ':?' for a list of options
Watched bindings:
X=4
:s
step 12 ==> value: 5
step 11 ==> value: 5
We are in the stepper mode
Evaluating step 13 -->
(SETQ X (+ X 1))
Type ':?' for a list of options
Watched bindings:
X=5
:u
Type the name of the symbol to (un)watch : x
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 14 -->
(+ X 1)
Type ':?' for a list of options
:s
step 14 ==> value: 6
step 13 ==> value: 6
We are in the stepper mode
Evaluating step 15 -->
(SETQ X (+ X 1))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 16 -->
(+ X 1)
Type ':?' for a list of options
:s
step 16 ==> value: 7
step 15 ==> value: 7
We are in the stepper mode
Evaluating step 17 -->
(SETQ X (+ X 1))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 18 -->
(+ X 1)
Type ':?' for a list of options
:s
step 18 ==> value: 8
step 17 ==> value: 8
step 4 ==> value: NIL
step 3 ==> value: 8
step 2 ==> value: 8
step 1 ==> value: 8
8
STEP-NEXT(28): (defun test-backtrace (x)
  (labels ((f1 (x) (f2 (1+ x)))
           (f2 (x) (f3 (* x 3)))
           (f3 (x) (+ x 10)))
    (f1 x)))
TEST-BACKTRACE
STEP-NEXT(29): (stepper:step (test-backtrace 3))
We are in the stepper mode
Evaluating step 1 -->
(TEST-BACKTRACE 3)
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 2 -->
(BLOCK TEST-BACKTRACE
  (LABELS ((F1 (X) (F2 (1+ X)))
           (F2 (X) (F3 (* X 3)))
           (F3 (X) (+ X 10)))
    (F1 X)))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 3 -->
(LABELS ((F1 (X) (F2 (1+ X)))
         (F2 (X) (F3 (* X 3)))
         (F3 (X) (+ X 10)))
  (F1 X))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 4 -->
((LABELS F1) X)
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 5 -->
(BLOCK F1 (F2 (1+ X)))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 6 -->
((LABELS F2) (1+ X))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 7 -->
(1+ X)
Type ':?' for a list of options
:s
step 7 ==> value: 4
We are in the stepper mode
Evaluating step 8 -->
(BLOCK F2 (F3 (* X 3)))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 9 -->
((LABELS F3) (* X 3))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 10 -->
(* X 3)
Type ':?' for a list of options
:s
step 10 ==> value: 12
We are in the stepper mode
Evaluating step 11 -->
(BLOCK F3 (+ X 10))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 12 -->
(+ X 10)
Type ':?' for a list of options
:bt

(#<LISP-STACK-FRAME ((LABELS F3) 12) {3839E350}>
 #<LISP-STACK-FRAME ((LABELS F2) 4) {42EF4336}>
 #<LISP-STACK-FRAME ((LABELS F1) 3) {71E556E2}>
 #<LISP-STACK-FRAME (TEST-BACKTRACE 3) {2D1E31F3}>
 #<LISP-STACK-FRAME (SYSTEM::%EVAL (ABCL-STEPPER:STEP (TEST-BACKTRACE 3))) {5ACA7463}>
 #<LISP-STACK-FRAME (EVAL (ABCL-STEPPER:STEP (TEST-BACKTRACE 3))) {62846AFF}>
 #<LISP-STACK-FRAME (SYSTEM:INTERACTIVE-EVAL (ABCL-STEPPER:STEP (TEST-BACKTRACE 3))) {390D720B}>
 #<LISP-STACK-FRAME (TOP-LEVEL::REPL) {65405D70}>
 #<LISP-STACK-FRAME (TOP-LEVEL::TOP-LEVEL-LOOP) {6CA054D7}>)
Type ':?' for a list of options
:c
step 12 ==> value: 22
step 11 ==> value: 22
step 9 ==> value: 22
step 8 ==> value: 22
step 6 ==> value: 22
step 5 ==> value: 22
step 4 ==> value: 22
step 3 ==> value: 22
step 2 ==> value: 22
step 1 ==> value: 22
22
STEP-NEXT(30): (stepper:step (values (list (cons 1 3) (cons 1 7))
                      (list (cons 2 4) (cons 2 8))))
We are in the stepper mode
Evaluating step 1 -->
(VALUES (LIST (CONS 1 3) (CONS 1 7)) (LIST (CONS 2 4) (CONS 2 8)))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 2 -->
(LIST (CONS 1 3) (CONS 1 7))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 3 -->
(CONS 1 3)
Type ':?' for a list of options
:s
step 3 ==> value: (1 . 3)
We are in the stepper mode
Evaluating step 4 -->
(CONS 1 7)
Type ':?' for a list of options
:s
step 4 ==> value: (1 . 7)
step 2 ==> value: ((1 . 3) (1 . 7))
We are in the stepper mode
Evaluating step 5 -->
(LIST (CONS 2 4) (CONS 2 8))
Type ':?' for a list of options
:sn
step 5 ==> value: ((2 . 4) (2 . 8))
step 1 ==> value: ((1 . 3) (1 . 7))
step 1 ==> value: ((2 . 4) (2 . 8))
((1 . 3) (1 . 7))
((2 . 4) (2 . 8))
STEP-NEXT(31):
```

For steps with ASDF systems we can use asdf:load-source-op

```
CL-USER(1): (require :asdf)
NIL
CL-USER(2): (require :abcl-contrib)
NIL
CL-USER(3): (require :abcl-stepper)
NIL
CL-USER(4): (asdf:load-system :quicklisp-abcl)
T
CL-USER(5): (ql:quickload :alexandria)
To load "alexandria":
  Load 1 ASDF system:
    alexandria
; Loading "alexandria"

(:ALEXANDRIA)
CL-USER(6): (asdf:operate 'asdf:load-source-op :alexandria)
#<ASDF/LISP-ACTION:LOAD-SOURCE-OP >
#<ASDF/PLAN:SEQUENTIAL-PLAN {34FAF8EA}>
CL-USER(7): (stepper:step (alexandria:plist-hash-table '(:a 1 :b 2 :c 3)))
We are in the stepper mode
Evaluating step 1 -->
(ALEXANDRIA:PLIST-HASH-TABLE '(:A 1 :B 2 :C 3))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 2 -->
'(:A 1 :B 2 :C 3)
Type ':?' for a list of options
:s
step 2 ==> value: (:A 1 :B 2 :C 3)
We are in the stepper mode
Evaluating step 3 -->
(BLOCK ALEXANDRIA:PLIST-HASH-TABLE
  (LET ((ALEXANDRIA::TABLE
         (APPLY #'MAKE-HASH-TABLE ALEXANDRIA::HASH-TABLE-INITARGS)))
    (DO ((ALEXANDRIA::TAIL ALEXANDRIA::PLIST
          (CDDR ALEXANDRIA::TAIL)))
        ((NOT ALEXANDRIA::TAIL))
      (LET ((#:KEY29386 (CAR ALEXANDRIA::TAIL))
            (#:HASH-TABLE29387 ALEXANDRIA::TABLE))
        (MULTIPLE-VALUE-BIND (#:VALUE29388 #:PRESENTP29389)
            (GETHASH #:KEY29386 #:HASH-TABLE29387)
          (IF #:PRESENTP29389
              (VALUES #:VALUE29388 #:PRESENTP29389)
              (VALUES (SYSTEM:PUTHASH #:KEY29386
                                      #:HASH-TABLE29387
                                      (CADR ALEXANDRIA::TAIL))
                      NIL)))))
    ALEXANDRIA::TABLE))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 4 -->
(LET ((ALEXANDRIA::TABLE
       (APPLY #'MAKE-HASH-TABLE ALEXANDRIA::HASH-TABLE-INITARGS)))
  (DO ((ALEXANDRIA::TAIL ALEXANDRIA::PLIST (CDDR ALEXANDRIA::TAIL)))
      ((NOT ALEXANDRIA::TAIL))
    (LET ((#:KEY29386 (CAR ALEXANDRIA::TAIL))
          (#:HASH-TABLE29387 ALEXANDRIA::TABLE))
      (MULTIPLE-VALUE-BIND (#:VALUE29388 #:PRESENTP29389)
          (GETHASH #:KEY29386 #:HASH-TABLE29387)
        (IF #:PRESENTP29389
            (VALUES #:VALUE29388 #:PRESENTP29389)
            (VALUES (SYSTEM:PUTHASH #:KEY29386
                                    #:HASH-TABLE29387
                                    (CADR ALEXANDRIA::TAIL))
                    NIL)))))
  ALEXANDRIA::TABLE)
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 5 -->
(APPLY #'MAKE-HASH-TABLE ALEXANDRIA::HASH-TABLE-INITARGS)
Type ':?' for a list of options
:l
Showing the values of variable bindings.
From inner to outer scopes:
HASH-TABLE-INITARGS=NIL
PLIST=(A 1 B 2 C 3)
HASH-TABLE-INITARGS=NIL
PLIST=(A 1 B 2 C 3)
Showing the values of function bindings.
From inner to outer scopes:
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 6 -->
#'MAKE-HASH-TABLE
Type ':?' for a list of options
:s
step 6 ==> value: #<MAKE-HASH-TABLE {646E548B}>
step 5 ==> value: #<EQL HASH-TABLE 0 entries, 11 buckets {733688E9}>
We are in the stepper mode
Evaluating step 7 -->
(DO ((ALEXANDRIA::TAIL ALEXANDRIA::PLIST (CDDR ALEXANDRIA::TAIL)))
    ((NOT ALEXANDRIA::TAIL))
  (LET ((#:KEY29386 (CAR ALEXANDRIA::TAIL))
        (#:HASH-TABLE29387 ALEXANDRIA::TABLE))
    (MULTIPLE-VALUE-BIND (#:VALUE29388 #:PRESENTP29389)
        (GETHASH #:KEY29386 #:HASH-TABLE29387)
      (IF #:PRESENTP29389
          (VALUES #:VALUE29388 #:PRESENTP29389)
          (VALUES (SYSTEM:PUTHASH #:KEY29386
                                  #:HASH-TABLE29387
                                  (CADR ALEXANDRIA::TAIL))
                  NIL)))))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 8 -->
(NOT ALEXANDRIA::TAIL)
Type ':?' for a list of options
:s
step 8 ==> value: NIL
We are in the stepper mode
Evaluating step 9 -->
(LET ((#:KEY29386 (CAR ALEXANDRIA::TAIL))
      (#:HASH-TABLE29387 ALEXANDRIA::TABLE))
  (MULTIPLE-VALUE-BIND (#:VALUE29388 #:PRESENTP29389)
      (GETHASH #:KEY29386 #:HASH-TABLE29387)
    (IF #:PRESENTP29389
        (VALUES #:VALUE29388 #:PRESENTP29389)
        (VALUES (SYSTEM:PUTHASH #:KEY29386
                                #:HASH-TABLE29387
                                (CADR ALEXANDRIA::TAIL))
                NIL))))
Type ':?' for a list of options
:s
We are in the stepper mode
Evaluating step 10 -->
(CAR ALEXANDRIA::TAIL)
Type ':?' for a list of options
:c
step 10 ==> value: :A
step 9 ==> value: 1
step 7 ==> value: NIL
step 4 ==> value: #<EQL HASH-TABLE 3 entries, 11 buckets {733688E9}>
step 3 ==> value: #<EQL HASH-TABLE 3 entries, 11 buckets {733688E9}>
step 1 ==> value: #<EQL HASH-TABLE 3 entries, 11 buckets {733688E9}>
#<EQL HASH-TABLE 3 entries, 11 buckets {733688E9}>
CL-USER(8):
```

There is also a protocol (with a reference implementation) for create external(s) GUI(s) for the stepper process.
See https://gitlab.com/cl-projects/abcl-visual-stepper
