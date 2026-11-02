(in-package #:abcl.test.lisp)

;;; ATOM is (NOT CONS).  SUBTYPEP answered NIL, T for ATOM against any
;;; type but T itself -- (NOT CONS), and unions containing ATOM,
;;; included -- and T, T for most types against ATOM, T, (NOT SYMBOL)
;;; and (MEMBER (A)) included.

(defun subtypep-both (a b)
  (multiple-value-list (subtypep a b)))

(deftest subtypep-atom.left
    (mapcar (lambda (b) (subtypep-both 'atom b))
            '((not cons) (or atom symbol) (or symbol atom) (or symbol (not cons))
              (not list) (mod 5) (and symbol atom) symbol))
  ((t t) (t t) (t t) (t t) (nil t) (nil t) (nil t) (nil t)))

(deftest subtypep-atom.right
    (mapcar (lambda (a) (subtypep-both a 'atom))
            '(t (or symbol cons) (not symbol) (member (a)) (eql (a)) list sequence
              (not cons) (member a 1) (and list symbol) null symbol fixnum
              (vector t) (or symbol atom)))
  ((nil t) (nil t) (nil t) (nil t) (nil t) (nil t) (nil t)
   (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(deftest subtypep-atom.unions-reordered
    (list (subtypep-both '(or atom symbol) '(or symbol atom))
          (subtypep-both '(or atom cons) '(or cons atom))
          (subtypep-both '(or atom division-by-zero built-in-class)
                         '(or built-in-class division-by-zero atom)))
  ((t t) (t t) (t t)))

(deftest subtypep-atom.satisfies-uncertain
    (list (first (subtypep-both '(satisfies consp) 'atom))
          (first (subtypep-both 'atom '(satisfies atom))))
  (nil nil))

(deftype subtypep-atom-list-alias () 'list)
(deftype subtypep-atom-alias () 'atom)
(defstruct subtypep-atom-struct a)
(defclass subtypep-atom-class () ())

(defparameter *subtypep-atom-types*
  (list* 'atom 'cons 'list 'null 'symbol 't '(not cons) '(not symbol) '(not list)
         '(or symbol cons) '(or atom symbol) '(member (a)) '(member a 1) '(eql (a))
         'sequence 'fixnum '(and list symbol) '(or cons atom)
         ;; type specifiers the ATOM clauses do not name: compound CONS
         ;; types, aliases, KEYWORD, (MEMBER NIL), structure and standard
         ;; classes, and class objects themselves
         '(cons integer) '(or null cons) '(and list (not null)) 'keyword '(member nil)
         'subtypep-atom-list-alias 'subtypep-atom-alias
         'subtypep-atom-struct 'subtypep-atom-class 'standard-object 'hash-table
         (mapcar #'find-class '(cons list t symbol null subtypep-atom-class))))

(defparameter *subtypep-atom-objects*
  (list nil t 'a :k 1 -7 (expt 2 40) 1.5 #\x "s" #(1 2) '(a) '(1 . 2)
        (make-hash-table) #'car (make-subtypep-atom-struct)
        (make-instance 'subtypep-atom-class)))

(deftest subtypep-atom.sound
    ;; Wherever SUBTYPEP is sure of a subtype, no object belongs to the
    ;; first type and not to the second.  (Pairs not involving ATOM are
    ;; checked too; the battery has none that master gets wrong.)
    (let ((wrong '()))
      (dolist (a *subtypep-atom-types*)
        (dolist (b *subtypep-atom-types*)
          (multiple-value-bind (sub sure) (subtypep a b)
            (when (and sub sure)
              (dolist (x *subtypep-atom-objects*)
                (when (and (typep x a) (not (typep x b)))
                  (push (list a b x) wrong)))))))
      wrong)
  nil)
