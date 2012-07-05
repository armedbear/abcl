;;; Some simple micro-benchmarks for CLOS.
;;;
;;; From: Kiczales and Rodriguez Jr., "Efficient Method Dispatch in PCL"

(defun fun (x) 0)

(defclass c1 ()
  ((x :initform 0
      :accessor accessor1
      :accessor accessor2
      :accessor accessor3)))

(defclass c2 (c1)
  ())

(defclass c3 (c1)
  ())

(defmethod g1 ((f c1)) 0)

(defmethod g2 ((f c1)) 0)
(defmethod g2 ((b c2)) 0)

(defvar *outer-times* 3)
(defvar *inner-times* 100000)

(defmacro test (&body body)
  `(let ((i1 (make-instance 'c1))
         (i2 (make-instance 'c2))
         (i3 (make-instance 'c3)))
     (dotimes (i *outer-times*)
       (time (dotimes (j *inner-times*)
               ,@body)))))

(defun fun-test () (test (fun i1)))
(defun accessor1-test () (test (accessor1 i1)))
(defun accessor2-test () (test (accessor2 i2)
                               (accessor2 i2)))
(defun accessor3-test () (test (accessor3 i1)
                               (accessor3 i2)
                               (accessor3 i3)))
(defun g1-test () (test (g1 i1)))
(defun g2-test () (test (g2 i2)
                        (g2 i2)))
