(in-package #:abcl.test.lisp)

#|
(deftest weak-hash-table.1
    (labels ((random-key () 
               (coerce (/ (random 10000) (random 10000))
                       'single-float)))
    (let ((ht (make-hash-table :weakness :keys))
      (dotimes (i 1000) 
        (setf (gethash (random-key) ht) (random 100000))
        (sys::hash-table-entries ht)

|#

(defun random-object ()
  "A randomly constructed object that is elgible for garbage collection."
  (coerce (/ (random 10000) (1+ (random 10000)))
          'single-float))

(deftest weak-hash-table.1 
  (let* ((ht (make-hash-table :weakness :key))
         (entries 0))
    (dotimes (i 100000) 
      (setf (gethash (random-object) ht) (random 100000))
      (let ((new-entries (sys::hash-table-count ht)))
        (when (and new-entries
                   (> entries new-entries))
          (format t "~&Previously ~A entries, now ~A." 
                  entries new-entries))
        (setf entries new-entries))))
    nil)

(deftest weak-hash-table.2 
    (let* ((ht (make-hash-table :weakness :value))
           (entries 0))
      (dotimes (i 100000) 
        (setf (gethash (random-object) ht) (random 100000))
        (let ((new-entries (sys::hash-table-count ht)))
          (when (and new-entries
                     (> entries new-entries))
            (format t "~&Previously ~A entries, now ~A." 
                    entries new-entries))
          (setf entries new-entries))))
    nil)

(deftest weak-hash-table.3
    (let* ((ht (make-hash-table :weakness :key-and-value))
           (entries 0))
      (dotimes (i 100000) 
        (setf (gethash (random-object) ht) (random 100000))
        (let ((new-entries (sys::hash-table-count ht)))
          (when (and new-entries
                     (> entries new-entries))
            (format t "~&Previously ~A entries, now ~A." 
                    entries new-entries))
          (setf entries new-entries))))
    nil)

(deftest weak-hash-table.4
    (let* ((ht (make-hash-table :weakness :key-or-value))
           (entries 0))
      (dotimes (i 100000) 
        (setf (gethash (random-object) ht) (random 100000))
        (let ((new-entries (sys::hash-table-count ht)))
          (when (and new-entries
                     (> entries new-entries))
            (format t "~&Previously ~A entries, now ~A." 
                    entries new-entries))
          (setf entries new-entries))))
    nil)


            
  


