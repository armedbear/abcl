(in-package #:abcl.test.lisp)
;;; From CL-FAD
(defvar *stream-buffer-size* 8192)
(defun cl-fad-copy-stream (from to &optional (checkp t))
  "Copies into TO \(a stream) from FROM \(also a stream) until the end
of FROM is reached, in blocks of *stream-buffer-size*.  The streams
should have the same element type.  If CHECKP is true, the streams are
checked for compatibility of their types."
  (when checkp
    (unless (subtypep (stream-element-type to) (stream-element-type from))
      (error "Incompatible streams ~A and ~A." from to)))
  (let ((buf (make-array *stream-buffer-size*
                         :element-type (stream-element-type from))))
    (loop
     (let ((pos (read-sequence buf from)))
       (when (zerop pos) (return))
       (write-sequence buf to :end pos))))
  (values))

(defun cl-fad-copy-file (from to &key overwrite)
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO.  If
OVERWRITE is true overwrites the file designtated by TO if it exists."
  (let ((element-type '(unsigned-byte 8)))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists (if overwrite
                                           :supersede :error))
        (cl-fad-copy-stream in out))))
  (values))

(defvar *foo.lisp*
  `(defun foo ()
     (labels ((output ()
                (format t "FOO here.")))
       (output))))

(defmacro with-temp-directory ((directory) &rest body)
  `(let ((*default-pathname-defaults* *abcl-test-directory*))
     (ensure-directories-exist ,directory)
     (prog1
         ,@body
       (delete-directory-and-files ,directory))))

