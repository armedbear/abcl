(in-package :abcl-asdf)

(defvar *added-to-classpath* nil)

(defvar *inhibit-add-to-classpath* nil)

(defun add-directory-jars-to-class-path (directory recursive-p)
  (loop :for jar :in (if recursive-p 
                         (all-jars-below directory) 
                         (directory (merge-pathnames "*.jar" directory)))
     :do (java:add-to-classpath jar)))

(defun all-jars-below (directory) 
  (loop :with q = (system:list-directory directory) 
     :while q :for top = (pop q)
     :if (null (pathname-name top)) 
     :do (setq q (append q (all-jars-below top))) 
     :if (equal (pathname-type top) "jar") 
     :collect top))

(defun need-to-add-directory-jar? (directory recursive-p)
  (loop :for jar :in (if recursive-p 
                         (all-jars-below directory)
                         (directory (merge-pathnames "*.jar" directory)))
     :doing (if (not (member (namestring (truename jar)) 
                             *added-to-classpath* :test 'equal))
                (return-from need-to-add-directory-jar? t)))
  nil)

(defmethod java:add-to-classpath :around ((uri-or-uris t) &optional classloader)
  (declare (ignore classloader))
  (call-next-method)
  (if (listp uri-or-uris)
      (dolist (uri uri-or-uris)
        (pushnew uri *added-to-classpath*))
      (pushnew uri-or-uris *added-to-classpath*)))

(in-package :asdf)

(defclass jar-directory (static-file) ())

(defmethod perform ((operation compile-op) (c jar-directory))
  (unless abcl-asdf:*inhibit-add-to-classpath*
    (abcl-asdf:add-directory-jars-to-class-path (truename (component-pathname c)) t)))

(defmethod perform ((operation load-op) (c jar-directory))
  (unless abcl-asdf:*inhibit-add-to-classpath*
    (abcl-asdf:add-directory-jars-to-class-path (truename (component-pathname c)) t)))

(defmethod operation-done-p ((operation load-op) (c jar-directory))
  (or abcl-asdf:*inhibit-add-to-classpath*
      (not (abcl-asdf:need-to-add-directory-jar? (component-pathname c) t))))

(defmethod operation-done-p ((operation compile-op) (c jar-directory))
  t)

(defclass jar-file (static-file) ())

(defmethod source-file-type ((c jar-file) (s module)) "jar")

(defmethod perform ((operation compile-op) (c jar-file))
  (java:add-to-classpath (component-pathname c)))

(defmethod perform ((operation load-op) (c jar-file))
  (or abcl-asdf:*inhibit-add-to-classpath*
      (java:add-to-classpath (component-pathname c))))

;;; The original JSS specified jar pathnames as having a NAME ending
;;; in ".jar" without a TYPE.  If we encounter such a definition, we
;;; clean it up.
(defmethod normalize-jar-name ((component jar-file))
  (when (#"endsWith" (slot-value component 'name) ".jar")
    (with-slots (name absolute-pathname) component
      (let* ((new-name 
              (subseq name 0 (- (length name) 4)))
             (new-absolute-pathname 
              (make-pathname :defaults absolute-pathname :name new-name)))
        (setf name new-name
              absolute-pathname new-absolute-pathname)))))

(defmethod perform :before ((operation compile-op) (c jar-file))
  (normalize-jar-name c))

(defmethod perform :before ((operation load-op) (c jar-file))
  (normalize-jar-name c))

(defmethod operation-done-p :before ((operation load-op) (c jar-file))
  (normalize-jar-name c))

(defmethod operation-done-p ((operation load-op) (c jar-file))
  (or abcl-asdf:*inhibit-add-to-classpath*
      (member (namestring (truename (component-pathname c)))
              abcl-asdf:*added-to-classpath* :test 'equal)))

(defmethod operation-done-p ((operation compile-op) (c jar-file))
  t)

(defclass class-file-directory (static-file) ())

(defmethod perform ((operation compile-op) (c class-file-directory))
  (java:add-to-classpath (component-pathname c)))

(defmethod perform ((operation load-op) (c class-file-directory))
  (java:add-to-classpath (component-pathname c)))

;; a jar file where the pathname and name are relative to JAVA_HOME
(defclass jdk-jar (jar-file) ())

(defmethod normalize-jar-name :after ((c jdk-jar))
  (setf (slot-value c 'absolute-pathname)
	(merge-pathnames
	 (merge-pathnames 
	  (slot-value c 'name)
	  (make-pathname :directory `(:relative ,(slot-value (component-parent c) 'relative-pathname))))
	 (java::jstatic "getProperty" "java.lang.System" "java.home"))))


		   
