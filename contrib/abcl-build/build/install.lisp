#-abcl (error "Sorry, but this only currently works with the Bear.")
(in-package :abcl/build)

(defun xdg/abcl-install-root (uri)
  "Return the private xdg rooted installation location for URI."
  (merge-pathnames
   (make-pathname :directory `(:relative "abcl" "install" ,(pathname-name uri)))
   (uiop/configuration:xdg-data-home)))

(defun xdg/abcl-download-root (&key (for-uri nil for-uri-p))
  (declare (ignore for-uri-p))
  (let ((root (merge-pathnames
               (make-pathname :directory '(:relative "abcl" "dist"))
               (uiop/configuration:xdg-data-home)))) ;; TODO move to proper XDG cache hierarchy
    (unless for-uri
      (return-from xdg/abcl-download-root root))
    (let* ((uri (if (pathnamep for-uri)
                    for-uri
                    (pathname for-uri)))
           (name (pathname-name uri)))
      (merge-pathnames
         (make-pathname :directory `(:relative ,name))
         root))))

(defgeneric xdg/install ((uri pathname) &key type)
  (:method ((uri pathname) &key (type :unzip))
    (declare (ignore type))
    (download-and-unzip uri)))

(defun download-and-unzip (uri)
  (let ((archive 
         (download uri))
        (root
         (xdg/abcl-download-root :for-uri uri)))
    (ensure-directories-exist root)
    (sys:unzip archive root)
    (values
     root
     (directory (merge-pathnames "**/*" root)))))

(defun download (uri &key (destination
                           (merge-pathnames
                            (make-pathname :defaults uri :host nil :device nil :directory nil)
                            (xdg/abcl-download-root))))
  "Download the contents of URI to DESTINATION.

Returns the local pathname of the download artifact."
  (ensure-directories-exist destination)
  (uiop:copy-file
   (open uri :direction :input)
   destination)
  destination)

  



