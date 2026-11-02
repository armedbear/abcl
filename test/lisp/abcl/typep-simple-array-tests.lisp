(in-package #:abcl.test.lisp)

;;; An expressly adjustable array is not a simple array (CLHS
;;; SIMPLE-ARRAY), and TYPEP must say so for every compound SIMPLE-ARRAY
;;; type, as it already did for the symbol SIMPLE-ARRAY: an adjustable
;;; vector was of (SIMPLE-ARRAY T (3)) and (SIMPLE-VECTOR 3).

(defparameter *typep-not-simple-arrays*
  (list (make-array 3 :adjustable t)
        (make-array 3 :fill-pointer 2)
        (make-array 3 :displaced-to (make-array 5))
        (make-array 3 :element-type 'character :adjustable t)
        (make-array 3 :element-type 'character :fill-pointer 1)
        (make-array 3 :element-type 'bit :adjustable t)
        (make-array 3 :element-type '(unsigned-byte 8) :adjustable t)
        (make-array '(2 2) :adjustable t)
        (make-array '(2 2) :displaced-to (make-array 4))))

(defparameter *typep-simple-array-types*
  '(simple-array (simple-array *) (simple-array * (*)) (simple-array t (*))
    (simple-array t (3)) (simple-array t 1) (simple-array t 2) (simple-array t (2 2))
    (simple-array * 2) simple-vector (simple-vector 3) simple-string (simple-string 3)
    (simple-array character (*)) simple-bit-vector (simple-bit-vector 3)
    (simple-array (unsigned-byte 8) (*))))

(deftest typep-simple-array.not-simple
    ;; interpreted and compiled TYPEP alike
    (let ((compiled (compile nil '(lambda (x type) (typep x type))))
          (wrong '()))
      (dolist (object *typep-not-simple-arrays*)
        (dolist (type *typep-simple-array-types*)
          (when (or (typep object type) (funcall compiled object type))
            (push (list (array-dimensions object) (array-element-type object)
                        (adjustable-array-p object) type)
                  wrong))))
      wrong)
  nil)

(deftest typep-simple-array.literal-type-compiled
    ;; a constant type compiled in place
    (let ((f (compile nil '(lambda (x) (list (typep x '(simple-array t (3)))
                                             (typep x '(simple-array * (*)))
                                             (typep x '(simple-vector 3)))))))
      (list (funcall f (make-array 3 :adjustable t))
            (funcall f (make-array 3))))
  ((nil nil nil) (t t t)))

(deftest typep-simple-array.simple-still-simple
    (list (typep (make-array 3) '(simple-array t (3)))
          (typep (make-array 3) '(simple-vector 3))
          (typep "abc" '(simple-array character (3)))
          (typep #*101 '(simple-bit-vector 3))
          (typep (make-array '(2 2)) '(simple-array t (2 2)))
          (typep (make-array 3 :element-type '(unsigned-byte 8)) '(simple-array (unsigned-byte 8) (3)))
          (typep (make-array 3 :adjustable nil) '(simple-array t (3))))
  (t t t t t t t))

(deftest typep-simple-array.rank-zero-and-nil-vectors
    ;; an array of rank zero made without :ADJUSTABLE, and a nil vector,
    ;; are simple, and ADJUSTABLE-ARRAY-P agrees with TYPEP
    (let ((z (make-array '())) (za (make-array '() :adjustable t))
          (n (make-array 3 :element-type nil)))
      (list (typep z '(simple-array t ())) (typep z '(simple-array *)) (adjustable-array-p z)
            (typep za '(simple-array t ())) (adjustable-array-p za)
            (typep n '(simple-array nil (*))) (adjustable-array-p n)))
  (t t nil nil t t nil))

(deftest typep-simple-array.rank-zero-adjust
    ;; ADJUST-ARRAY of an array of rank zero creates no element, so
    ;; :INITIAL-ELEMENT changes nothing, adjustable or not
    (let* ((z (make-array '() :initial-element 'x))
           (z2 (adjust-array z '() :initial-element 'y))
           (za (make-array '() :initial-element 'x :adjustable t))
           (za2 (adjust-array za '() :initial-element 'y)))
      (list (aref z) (aref z2) (eq z z2) (aref za2) (eq za za2)
            (aref (adjust-array z '() :initial-contents 'w))))
  (x x nil x t w))

(deftest typep-simple-array.adjustable-still-arrays
    ;; the non-simple types still hold them
    (list (typep (make-array 3 :adjustable t) '(array t (3)))
          (typep (make-array 3 :adjustable t) '(vector t 3))
          (typep (make-array 3 :element-type 'character :adjustable t) '(string 3))
          (typep (make-array 3 :element-type 'bit :adjustable t) '(bit-vector 3)))
  (t t t t))
