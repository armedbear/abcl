(in-package #:abcl.test.lisp)

;;; The pretty printer's LOOP printer lost a form's last token when it
;;; began a clause: (LOOP NIL) printed as (LOOP ), (LOOP FOR X IN Y DO)
;;; as (LOOP FOR X IN Y ).  The ANSI test PRINT.BACKQUOTE.RANDOM.14 failed
;;; whenever its random forms contained such a list.

(defun pprint-loop-string (form &rest bindings)
  (progv (list* '*package* '*print-pretty* '*print-right-margin*
                (mapcar #'first bindings))
      (list* (find-package '#:abcl.test.lisp) t 200 (mapcar #'second bindings))
    (prin1-to-string form)))

(defparameter *pprint-loop-forms*
  '((loop nil) (loop foo) (loop for x in y do) (loop for x in y collect)
    (loop for x in y collect nil) (loop repeat 3) (loop named foo)
    (loop for x in y when x collect x else collect nil end)
    (loop for x in y if x do) (loop with a = 1 do (f a) (g a))
    (loop for x in y finally) (loop nil nil) (loop (nil))
    ;; dotted tails, after an expression and after a clause word
    (loop for x in y . 3) (loop for x . 3) (loop do . 3) (loop on nil (nil) 3.5 . 3.5)))

(deftest pprint-loop.readable
    (remove-if (lambda (form)
                 (equal form (let ((*package* (find-package '#:abcl.test.lisp)))
                               (read-from-string
                                (pprint-loop-string form '(*print-readably* t))))))
               *pprint-loop-forms*)
  nil)

(deftest pprint-loop.last-token
    (list (pprint-loop-string '(loop nil))
          (pprint-loop-string '(loop for x in y do))
          (pprint-loop-string '(loop for x in y . 3))
          (pprint-loop-string '(loop do . 3)))
  ("(LOOP NIL)" "(LOOP FOR X IN Y DO)" "(LOOP FOR X IN Y . 3)" "(LOOP DO . 3)"))

(deftest pprint-loop.abbreviated
    (let ((form '(loop for x in (a (b (c))) when x collect (f x) else collect nil end)))
      (list (pprint-loop-string form '(*print-level* 2))
            (pprint-loop-string form '(*print-length* 2))))
  ("(LOOP FOR X IN # WHEN X # # ELSE COLLECT NIL END)" "(LOOP ...)"))
