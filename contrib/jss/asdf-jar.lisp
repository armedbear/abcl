(in-package :asdf)

(defclass jar-directory (static-file) ())

(defmethod perform ((operation compile-op) (c jar-directory))
  (unless jss:*inhibit-add-to-classpath*
    (jss:add-directory-jars-to-class-path (truename (component-pathname c)) t)))

(defmethod perform ((operation load-op) (c jar-directory))
  (unless jss:*inhibit-add-to-classpath*
    (jss:add-directory-jars-to-class-path (truename (component-pathname c)) t)))

(defmethod operation-done-p ((operation load-op) (c jar-directory))
  (or jss:*inhibit-add-to-classpath*
    (not (jss:need-to-add-directory-jar? (component-pathname c) t))))

(defmethod operation-done-p ((operation compile-op) (c jar-directory))
  t)

(defclass jar-file (static-file) ())

(defmethod perform ((operation compile-op) (c jar-file))
  (jss:add-to-classpath (component-pathname c)))

(defmethod perform ((operation load-op) (c jar-file))
  (or jss:*inhibit-add-to-classpath*
      (jss:add-to-classpath (component-pathname c))))

(defmethod operation-done-p ((operation load-op) (c jar-file))
  (or jss:*inhibit-add-to-classpath*
      (member (namestring (truename (component-pathname c))) jss:*added-to-classpath* :test 'equal)))

(defmethod operation-done-p ((operation compile-op) (c jar-file))
  t)

(defclass class-file-directory (static-file) ())

(defmethod perform ((operation compile-op) (c class-file-directory))
  (jss:add-to-classpath (component-pathname c)))

(defmethod perform ((operation load-op) (c class-file-directory))
  (jss:add-to-classpath (component-pathname c)))

(defmethod source-file-type ((c jar-file) (s module)) "jar")




