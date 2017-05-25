(in-package :named-readtables)

(defmacro define-api (name lambda-list type-list &body body)
  (flet ((parse-type-list (type-list)
           (let ((pos (position '=> type-list)))
             (assert pos () "You forgot to specify return type (`=>' missing.)")
             (values (subseq type-list 0 pos)
                     `(values ,@(nthcdr (1+ pos) type-list) &optional)))))
    (multiple-value-bind (body decls docstring)
        (parse-body body :documentation t :whole `(define-api ,name))
      (multiple-value-bind (arg-typespec value-typespec)
          (parse-type-list type-list)
        (multiple-value-bind (reqs opts rest keys)
            (parse-ordinary-lambda-list lambda-list)
          (declare (ignorable reqs opts rest keys))
          `(progn
             (declaim (ftype (function ,arg-typespec ,value-typespec) ,name))
             (locally
                 ;;; Muffle the annoying "&OPTIONAL and &KEY found in
                 ;;; the same lambda list" style-warning
                 #+sbcl (declare (sb-ext:muffle-conditions style-warning))
               (defun ,name ,lambda-list
                 ,docstring

                 #+sbcl (declare (sb-ext:unmuffle-conditions style-warning))

                 ,@decls

                 ;; SBCL will interpret the ftype declaration as
                 ;; assertion and will insert type checks for us.
                 #-sbcl
                 (progn
                   ;; CHECK-TYPE required parameters
                   ,@(loop for req-arg in reqs
                           for req-type = (pop type-list)
                           do (assert req-type)
                           collect `(check-type ,req-arg ,req-type))

                   ;; CHECK-TYPE optional parameters
                   ,@(loop initially (assert (or (null opts)
                                                 (eq (pop type-list) '&optional)))
                           for (opt-arg . nil) in opts
                           for opt-type = (pop type-list)
                           do (assert opt-type)
                           collect `(check-type ,opt-arg ,opt-type))

                   ;; CHECK-TYPE rest parameter
                   ,@(when rest
                       (assert (eq (pop type-list) '&rest))
                       (let ((rest-type (pop type-list)))
                         (assert rest-type)
                         `((dolist (x ,rest)
                             (check-type x ,rest-type)))))

                   ;; CHECK-TYPE key parameters
                   ,@(loop initially (assert (or (null keys)
                                                 (eq (pop type-list) '&key)))
                           for ((keyword key-arg)  . nil) in keys
                           for (nil key-type) = (find keyword type-list :key #'car)
                           collect `(check-type ,key-arg ,key-type)))

                 ,@body))))))))
