#-abcl (error "Sorry, but this only currently works with the Bear.")
(in-package :abcl-build)

(require :asdf)
(defgeneric install ((uri pathname) &key
                                      (type :unpack/zip)))

(defmethod install ((uri pathname) &key
                                     (type :unpack/zip))
  (install-zip uri type))
  
(defun install-zip (uri binary-unzip)
  (let ((directory (make-pathname :defaults uri :host nil :device nil :name nil :version nil))
        (archive (make-pathname :defaults uri :host nil :device nil :directory nil)))
    (ensure-directories-exist directory)
    (uiop:copy-file
     (open uri :direction :input)
     (merge-pathnames archive directory))))

     
   
