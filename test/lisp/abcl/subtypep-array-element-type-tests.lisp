(in-package #:abcl.test.lisp)

;;; An array type with an unspecified element type (*) is the union of
;;; the arrays of every element type, so SUBTYPEP must not put it within
;;; a type that names one: VECTOR was within (VECTOR T), a string
;;; included, and (VECTOR *) within (VECTOR DOUBLE-FLOAT).

(defun subtypep-both-values (a b)
  (multiple-value-list (subtypep a b)))

(deftest subtypep-array-element-type.star-not-within
    (mapcar (lambda (pair) (subtypep-both-values (first pair) (second pair)))
            '((vector (vector t)) (vector (array t (*))) (array (array t))
              ((vector *) (vector t)) ((vector *) (vector double-float))
              ((simple-array * (*)) simple-vector) ((simple-array * (*)) (simple-array t (*)))
              ((simple-array * 2) (array t 2)) ((array * (*)) (vector character))))
  ((nil t) (nil t) (nil t) (nil t) (nil t) (nil t) (nil t) (nil t) (nil t)))

(deftest subtypep-array-element-type.bit-vectors
    ;; an array type whose element type upgrades to BIT, rank one, is a
    ;; bit vector; a simple one when it is a SIMPLE-ARRAY type
    (mapcar (lambda (pair) (subtypep-both-values (first pair) (second pair)))
            '(((simple-array bit (*)) simple-bit-vector) ((array bit (*)) bit-vector)
              ((simple-array bit (3)) (simple-bit-vector 3)) ((simple-array bit (3)) (bit-vector 3))
              ((simple-array (integer 0 1) (*)) simple-bit-vector)
              ((array bit (*)) simple-bit-vector) ((simple-array bit (4)) (simple-bit-vector 3))
              ((simple-array bit 2) bit-vector) ((simple-array * (*)) bit-vector)
              ((simple-array character (*)) bit-vector)))
  ((t t) (t t) (t t) (t t) (t t) (nil t) (nil t) (nil t) (nil t) (nil t)))

(deftest subtypep-array-element-type.class-simple-vector
    ;; the class SIMPLE-VECTOR as a type specifier: (SIMPLE-ARRAY T (*))
    (let ((sv (find-class 'simple-vector)))
      (list (subtypep-both-values '(simple-array t (*)) sv)
            (subtypep-both-values '(simple-array t (3)) sv)
            (subtypep-both-values '(simple-array character (*)) sv)
            (subtypep-both-values '(simple-array * (*)) sv)
            (subtypep-both-values '(simple-array t 2) sv)))
  ((t t) (t t) (nil t) (nil t) (nil t)))

(deftest subtypep-array-element-type.still-within
    ;; what was right stays right
    (mapcar (lambda (pair) (subtypep-both-values (first pair) (second pair)))
            '(((vector t) vector) (simple-vector (vector t)) ((vector t 3) (vector t))
              ((simple-array t (*)) simple-vector) ((vector *) vector) (string vector)
              ((array t (*)) (array * (*))) ((vector character) string)
              (simple-bit-vector bit-vector)))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(defparameter *subtypep-array-types*
  (append
   '(array vector simple-vector string simple-string base-string bit-vector
     simple-bit-vector simple-array
     (array *) (array t) (array t (*)) (array t 2) (array * (*)) (array character (*))
     (vector *) (vector t) (vector character) (vector bit) (vector (unsigned-byte 8))
     (vector double-float) (simple-array t (*)) (simple-array * (*))
     (simple-array character (*)) (simple-array * 2) (simple-array bit (*))
     (array bit (*)) (bit-vector 3) (simple-bit-vector 3))
   (mapcar #'find-class '(simple-vector vector array string bit-vector))))

(defparameter *subtypep-array-objects*
  ;; simple arrays only: whether an adjustable array is of a SIMPLE-ARRAY
  ;; type is TYPEP's question, not SUBTYPEP's
  (list (vector 1 2 3) (make-array 3) "abc" (make-string 3 :element-type 'base-char)
        #*101 (make-array 3 :element-type '(unsigned-byte 8))
        (make-array 3 :element-type 'double-float :initial-element 0d0)
        (make-array '(2 2)) (make-array 0) ""))

(deftest subtypep-array-element-type.sound
    ;; wherever SUBTYPEP is sure of a subtype, no object of the first type
    ;; is outside the second
    (let ((wrong '()))
      (dolist (a *subtypep-array-types*)
        (dolist (b *subtypep-array-types*)
          (multiple-value-bind (sub sure) (subtypep a b)
            (when (and sub sure)
              (dolist (x *subtypep-array-objects*)
                (when (and (typep x a) (not (typep x b)))
                  (push (list a b (type-of x)) wrong)))))))
      wrong)
  nil)

(deftest subtypep-array-element-type.upgraded
    ;; two specified element types relate as their upgraded types do
    (let ((wrong '())
          (elements '(t character base-char bit fixnum (unsigned-byte 8) double-float
                      single-float (integer 0 1) symbol)))
      (dolist (e1 elements)
        (dolist (e2 elements)
          (let ((same (equal (upgraded-array-element-type e1)
                             (upgraded-array-element-type e2))))
            (unless (equal (subtypep-both-values `(vector ,e1) `(vector ,e2))
                           (list same t))
              (push (list e1 e2) wrong)))))
      wrong)
  nil)
