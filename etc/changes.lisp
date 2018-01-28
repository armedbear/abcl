(defpackage abcl/model/changes
  (:use :cl)
  (:export
   #:validate))

(in-package :abcl/model/changes)

(defun validate (&key
                   (system "abcl")
                   (model "etc/changes.n3"))
  (let ((file (probe-file (asdf:system-relative-pathname system model))))
    (when file
      (jeannie:read-rdf file :format :n3))))

            
  
