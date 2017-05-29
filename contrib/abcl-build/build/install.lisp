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
         (xdg/abcl-install-root uri)))
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

(defun xdg/executable (uri relative-path)
  (let* ((directory (xdg/abcl-install-root uri))
         (root (let ((name (pathname-name uri)))
                 (subseq name 0 (- (length name) (length "-bin")))))
         (home (merge-pathnames (make-pathname :directory `(:relative ,root))
                                    directory))
         (path (merge-pathnames relative-path home)))
    (dolist (p (possible-executable-names path))
      (when (probe-file p)
        (return-from xdg/executable
          (values
           (probe-file p)
           path))))
    ;; failure
    (values
     nil
     path)))


  

  



