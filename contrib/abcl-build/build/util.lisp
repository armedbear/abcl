;;;; TODO: move to a utility package
(in-package :abcl/build)

;;; TODO remove
(defun localize-executable-name (name)
  (let* ((p (if (pathnamep name)
                name
                (pathname name)))
         (type (pathname-type p)))
    (make-pathname :defaults p
                   :type 
                   (if (uiop:os-windows-p)
                       (when (null type)
                         "exe")
                       type))))

(defun possible-executable-names (name
				  &key (suffixes '("exe" "cmd" "bat") suffixes-p))
  (let* ((p (if (pathnamep name)
                name
                (pathname name)))
         (type (pathname-type p)))
    (unless (or (uiop:os-windows-p) suffixes-p)
      (return-from possible-executable-names
        (listify name)))
    (loop
       :for suffix :in suffixes
       :with result = (list p)
       :doing (push (make-pathname :defaults p :type suffix)
		    result)
       :finally (return (nreverse result)))))
       
(defun introspect-path-for (executable)
  (let ((which-command (if (uiop:os-windows-p)
                           "where"
                           "which")))
    (when (ignore-errors
            (uiop:run-program (list which-command which-command) :output :string))
      (dolist (p (possible-executable-names executable))
	(let ((raw-result 
	       (ignore-errors (uiop:run-program
			       (list which-command
				     (namestring p))
			       :output :string))))
	  (when raw-result
	    (let ((result (first (split-string raw-result #\Newline))))
	      (return-from introspect-path-for
		(values
		 result
		 (pathname result))))))))))

(defun probe-for-executable (directory executable)
  (dolist (p (possible-executable-names executable))
    (let ((pathname
	   (probe-file
	    (merge-pathnames executable directory))))
      (when pathname
	(return-from probe-for-executable
	  pathname)))))
  
(defun split-string (string split-char)
  (loop :for i = 0 :then (1+ j)
     :as j = (position split-char string :test #'string-equal :start i)
     :collect (subseq string i j)
     :while j))

(defun stringify (thing)
  (cond
    ((pathnamep thing)
     (namestring thing))
    ((stringp thing)
     thing)
    (t
     (error "Don't know how stringify ~a." thing))))

(defun listify (thing)
  (if (consp thing)
      thing
      (list thing)))

(defun some-directory-containing (executable)
  ;; search path
  (let ((in-path (introspect-path-for executable)))
    (when in-path
      (return-from some-directory-containing
                   in-path))
    (dolist (d 
              (if (uiop:os-windows-p)
                  '(#p"c:/Program Files/") ;; TODO localize me!
                  '(#p"/usr/local/bin/" #p"/opt/local/bin/" #p"/usr/bin/")))
      (let* ((e (localize-executable-name
                 (merge-pathnames :defaults d
                                  :name executable)))
             (p (probe-file p)))
        (when p
          (return-from some-directory-containing p))))))


        
       
