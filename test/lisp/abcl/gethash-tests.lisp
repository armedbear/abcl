(in-package #:abcl.test.lisp)

;;; Compiled GETHASH and (SETF GETHASH) call HashTableFunctions directly
;;; rather than the primitives.  These check that the values, the
;;; default, weak tables and the type error are as before.

(defun gethash-test-compile (lambda-expression)
  (compile nil lambda-expression))

(deftest gethash.compiled.values
    (let ((table (make-hash-table :test 'eq))
          (get (gethash-test-compile
                '(lambda (key table)
                  (multiple-value-list (gethash key table))))))
      (setf (gethash :a table) 1)
      (list (funcall get :a table)
            (funcall get :b table)))
  ((1 t) (nil nil)))

(deftest gethash.compiled.default
    (let ((table (make-hash-table :test 'equal))
          (get (gethash-test-compile
                '(lambda (key table)
                  (multiple-value-list (gethash key table :default))))))
      (setf (gethash "a" table) nil)
      (list (funcall get "a" table)
            (funcall get "b" table)))
  ((nil t) (:default nil)))

(deftest gethash.compiled.multiple-value-bind
    (let ((table (make-hash-table))
          (get (gethash-test-compile
                '(lambda (key table)
                  (multiple-value-bind (value present-p) (gethash key table)
                    (list value present-p))))))
      (setf (gethash 1 table) :one)
      (list (funcall get 1 table) (funcall get 2 table)))
  ((:one t) (nil nil)))

(deftest gethash.compiled.single-value
    (let ((table (make-hash-table))
          (get (gethash-test-compile
                '(lambda (key table)
                  (list (gethash key table) (gethash key table :none))))))
      (setf (gethash 1 table) :one)
      (list (funcall get 1 table) (funcall get 2 table)))
  ((:one :one) (nil :none)))

(deftest gethash.compiled.weak-table
    (let ((table (make-hash-table :test 'eq :weakness :key))
          (key (list 'key))
          (get (gethash-test-compile
                '(lambda (key table)
                  (multiple-value-list (gethash key table)))))
          (put (gethash-test-compile
                '(lambda (key table value)
                  (setf (gethash key table) value)))))
      (list (funcall put key table :value)
            (funcall get key table)
            (funcall get (list 'other) table)))
  (:value (:value t) (nil nil)))

(deftest gethash.compiled.setf
    (let ((table (make-hash-table :test 'equal))
          (put (gethash-test-compile
                '(lambda (key table value)
                  (setf (gethash key table) value)))))
      (list (funcall put "k" table 1)
            (funcall put "k" table 2)
            (gethash "k" table)
            (hash-table-count table)))
  (1 2 2 1))

(deftest gethash.compiled.type-error
    (let ((get (gethash-test-compile
                '(lambda (key table) (gethash key table))))
          (put (gethash-test-compile
                '(lambda (key table) (setf (gethash key table) 1)))))
      (list (handler-case (funcall get :a (list 1 2)) (type-error () :type-error))
            (handler-case (funcall put :a "not a table") (type-error () :type-error))))
  (:type-error :type-error))
