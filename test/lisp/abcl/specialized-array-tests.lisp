(in-package #:abcl.test.lisp)

;;; An array of rank zero, or of rank two or more, whose element type has
;;; no array class of its own (BIT and CHARACTER among them) was held in a
;;; general array: it began full of NIL and stored any object, while
;;; ARRAY-ELEMENT-TYPE said BIT.  The upgraded element type depends on the
;;; element type alone, not on rank (CLHS 15.1.2.1), so these arrays keep
;;; it as the vectors do.

(defparameter *specialized-array-element-types*
  '(bit character base-char (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32) t))

(defun every-element (array)
  (loop for i below (array-total-size array) collect (row-major-aref array i)))

(deftest specialized-array.default-elements
    ;; with no initial element, every element is of the element type
    (let ((wrong '()))
      (dolist (et *specialized-array-element-types*)
        (dolist (dims '(() (2 2) (2 3 2)))
          (dolist (adjustable '(nil t))
            (let* ((a (make-array dims :element-type et :adjustable adjustable))
                   (type (array-element-type a)))
              (unless (every (lambda (x) (typep x type)) (every-element a))
                (push (list et dims adjustable (every-element a)) wrong))))))
      wrong)
  nil)

(deftest specialized-array.refuses-other-elements
    ;; storing an object that is not of the element type signals, by AREF,
    ;; ROW-MAJOR-AREF, :INITIAL-ELEMENT and :INITIAL-CONTENTS alike
    (let ((wrong '()))
      (dolist (et '(bit character (unsigned-byte 16)))
        (dolist (adjustable '(nil t))
          (let ((a (make-array '(2 2) :element-type et :adjustable adjustable)))
            (dolist (how (list (lambda () (setf (aref a 0 1) 'symbol))
                               (lambda () (setf (row-major-aref a 3) 'symbol))
                               (lambda () (make-array '(2 2) :element-type et :adjustable adjustable
                                                             :initial-element 'symbol))
                               (lambda () (make-array '(2 2) :element-type et :adjustable adjustable
                                                             :initial-contents '((symbol symbol)
                                                                                 (symbol symbol))))))
              (unless (handler-case (progn (funcall how) nil) (type-error () t))
                (push (list et adjustable) wrong))))))
      wrong)
  nil)

(deftest specialized-array.stores-elements
    (let ((b (make-array '(2 2) :element-type 'bit))
          (c (make-array '(2 2) :element-type 'character :initial-element #\a))
          (z (make-array '() :element-type 'bit)))
      (setf (aref b 1 1) 1 (aref c 0 0) #\z (aref z) 1)
      (list (every-element b) (every-element c) (aref z)
            (array-element-type b) (array-element-type c)))
  ((0 0 0 1) (#\z #\a #\a #\a) 1 bit character))

(deftest specialized-array.adjusted
    ;; ADJUST-ARRAY's new elements are of the element type too
    (let ((b (adjust-array (make-array '(2 2) :element-type 'bit :adjustable t) '(3 3)))
          (c (adjust-array (make-array '(2 2) :element-type 'character :initial-element #\a
                                              :adjustable t)
                           '(3 3))))
      (list (every (lambda (x) (typep x 'bit)) (every-element b))
            (every #'characterp (every-element c))))
  (t t))

(deftest specialized-array.t-unchanged
    ;; a general array stores anything and starts as before
    (let ((a (make-array '(2 2))))
      (setf (aref a 0 0) 'symbol (aref a 1 1) "string")
      (list (aref a 0 0) (aref a 0 1) (aref a 1 1) (aref (make-array '()) )))
  (symbol nil "string" nil))
