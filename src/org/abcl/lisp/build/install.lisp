#-abcl (error "Sorry, but this only currently works with the Bear.")
(in-package :abcl-build)

(require :asdf)
(defgeneric install ((uri pathname) &key
                                      (type :unpack/zip)))

(defmethod install ((uri pathname) &key
                                     (type :unpack/zip))
  (install-zip uri type))
  
(defun install-zip (uri binary-unzip)
  (let ((archive 
         (download-artifact uri))
        (root
          (uiop/configuration:xdg-data-home
           (concatenate 'string
                        "abcl/"
                        (pathname-name uri)
                        "/"))))
    (ensure-directories-exist root)
    (sys:unzip archive root)
    root))

(defun download-artifact (uri)
    (let* ((root
            (uiop/configuration:xdg-data-home "abcl/dist/"))
           (archive
            (make-pathname :defaults uri :host nil :device nil :directory nil))
           (destination
            (merge-pathnames archive root)))
      (ensure-directories-exist destination)
      (uiop:copy-file
       (open uri :direction :input)
       destination)
      destination))
       
    


