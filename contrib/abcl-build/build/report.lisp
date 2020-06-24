(in-package :abcl/build)

;;; FIXME: will not work if DIRECTORY contains subdirectories
(defun directory-hashes (directory)
  "Return the size and sha256 hash of every direct entry of DIRECTORY."
  (let ((d (if (typep directory 'pathname)
               directory
               (pathname (concatenate 'string directory "/")))))
    (let ((result 
            (loop :for file
                  :in (directory (merge-pathnames "*.*" d))
                  :collecting (list
                               file 
                               (with-open-file (s file :direction :input)
                                 (when s
                                   (file-length s)))
                               (sys:sha256 file)))))
      (values
       result
       (hashes-report result)))))

(defun hashes-report (report)
  (format nil "~{~a~}~%"
          (loop :for (file size hash) :in report
                :collecting (format nil "~%<file:~a>~%~t:size ~a ;~%~t:sha256 ~a ."
                                    file size hash))))

             

