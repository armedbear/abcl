(defpackage abcl/tools/resource
  (:use #:cl)
  (:export
   #:retrieve))

(in-package :abcl/tools/resource)

(defun retrieve (uri &key (destination #p"resource.get"))
  (let ((stream (dexador:get uri :want-stream t)))
    (alexandria:with-output-to-file (output destination)
      (alexandria:copy-stream stream output))
    (values ;; TODO track the number of bytes retrieved
     (truename destination))))
  

