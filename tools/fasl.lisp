(in-package :cl-user)

(defun unpack (fasl-path &key (dir (make-temp-directory)))
  "Unpack compressed fasl at FASL-PATH into 'org/armedbear/lisp' under DIR renaming *.cls to *.class."
  (let ((pkg-dir (merge-pathnames "org/armedbear/lisp/" dir)))
    (ensure-directories-exist dir)
    (sys:unzip fasl-path dir)
    (ensure-directories-exist pkg-dir)
    (loop :for fasl :in (directory (merge-pathnames "*.cls" dir))
       :doing (rename-file 
               fasl
               (make-pathname :defaults fasl
                              :directory (pathname-directory pkg-dir)
                              :type "class")))
    dir))

        