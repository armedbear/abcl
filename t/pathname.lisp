(in-package :cl-user)

(prove:plan 1)
(let* ((uri #p"http://example.org/directory/name.version")
       (p (make-pathname :host nil :defaults uri)))
  (prove:like (namestring p) "^/directory/name.version$"))

(defparameter *removable-artifacts* nil)

(defun create-zip ()
  (uiop:with-temporary-file (:pathname tmp :type "zip" :keep t)
    (let* ((file
             (create-file))
           (fasl
             (compile-file file))
           (renamed-file
             (rename-file file (make-pathname :defaults file :name "output-date")))
           (renamed-fasl
             (rename-file fasl (make-pathname :defaults fasl :name "output-date")))
           (contents
             `(,renamed-file ,renamed-fasl))
           (zipfile
             (sys:zip tmp contents)))
      (dolist (file (list renamed-file renamed-fasl zipfile))
        (pushnew file *removable-artifacts*))
      (values
       zipfile
       contents))))
  
(defun create-file ()
  (uiop:with-temporary-file (:stream s :pathname p :keep t :type "lisp")
    (write
     `(format t "~s ~s~%" ,(get-universal-time) (symbol-name (gensym)))
     :stream s)
    p))

(prove:plan 5)
(let* ((zip-1
         (create-zip))
       (zip-2
         (progn
           (sleep 1.5)
           (create-zip)))
       (zip-tmp-1
         (make-pathname :defaults zip-1 :type "tmp"))
       (zip-tmp-2
         (make-pathname :defaults zip-2 :type "tmp"))
       (zip-1-path
         (ext:as-jar-pathname-archive zip-1))
       (zip-2-path
         (ext:as-jar-pathname-archive zip-2))
       (zip-1-entry-1
         (merge-pathnames "output-date.lisp" zip-1-path))
       (zip-2-entry-1
         (merge-pathnames "output-date.lisp" zip-2-path))
       (zip-1-entry-2
         (merge-pathnames "output-date.abcl" zip-1-path))
       (zip-2-entry-2
         (merge-pathnames "output-date.abcl" zip-2-path)))

  (uiop:copy-file zip-1 zip-tmp-1)
  (uiop:copy-file zip-2 zip-tmp-2)

  ;;; If the first two tests fail, we have not setup the test correctly
  ;;; FIXME: how to bail on the remaining tests early?
  (let ((zip-1-date (file-write-date zip-1))
        (zip-2-date (file-write-date zip-2)))
    (prove:ok
     (not (equal zip-1-date zip-2-date))
     (format nil "Archives have different times~%~a ~a~%~a ~a~%"
             zip-1-date zip-1
             zip-2-date zip-2)))
  (let ((date-1 (file-write-date zip-1-entry-1))
        (date-2 (file-write-date zip-2-entry-1)))
    (prove:ok
     (not (equal date-1 date-2))
     (format nil "Archive entries have different times~%~a <~a>~%~a <~a>~%"
             date-1 zip-1-entry-1
             date-2 zip-2-entry-1)))
  
  (let ((date-1 (file-write-date zip-1-path)))
    (sleep 1)
    (rename-file zip-2 zip-1)
    (let ((date-2 (file-write-date zip-1-path)))
      (prove:ok
       (not (equal date-1 date-2))
       (format nil "ZipCache recomputes date on JAR-PATHNAME archive~%<~a>~%~a ~a~%"
               zip-1-path date-1 date-2))))

  (uiop:copy-file zip-tmp-1 zip-1)
  (uiop:copy-file zip-tmp-2 zip-2)
  (let ((date-1 (file-write-date zip-1-entry-1)))
    (sleep 1.1)
    (rename-file zip-2 zip-1)
    (let ((date-2 (file-write-date zip-1-entry-1)))
      (prove:ok
       (not (equal date-1 date-2))
       (format nil "ZipCache recomputes JAR-PATHNAME entry dates~%<~a>~%~a ~a~%"
               zip-1-entry-1
               date-1 date-2))))

  (uiop:copy-file zip-tmp-1 zip-1)
  (uiop:copy-file zip-tmp-2 zip-2)
  (let* ((entry
           (make-pathname :defaults (ext:as-jar-pathname-archive zip-1-entry-2)
                          :name "__loader__" :type "_"))
         (date-1
           (file-write-date entry)))
    (sleep 1.1)
    (rename-file zip-2 zip-1)
    (let ((date-2 (file-write-date entry)))
      (prove:ok
       (not (equal date-1 date-2))
       (format nil "ZipCache recomputes singly nested JAR-PATHNAME entry dates~%<~a>~%~a ~a~%"
               entry
               date-1 date-2)))))

(prove:finalize)

   
