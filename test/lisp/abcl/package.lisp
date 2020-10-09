(defpackage #:abcl/test/lisp
  (:use #:cl #:abcl-rt)
  (:nicknames #:abcl-test-lisp #:abcl-test #:abcl.test.lisp)
  (:export 
   #:run 
   #:do-test 
   #:do-tests
   #:do-tests-matching
   ;; previously in file-system-tests.lisp
   #:pathnames-equal-p #:run-shell-command #:copy-file #:make-symbolic-link
   #:touch #:make-temporary-directory #:delete-directory-and-files
   ;;; Deprecated
   #:do-matching #:run-matching))

(in-package #:abcl.test.lisp)

(defparameter *abcl-test-directory* 
  (if (find :asdf2 *features*)
      (asdf:system-relative-pathname :abcl "test/lisp/abcl/")
      (make-pathname :host (pathname-host *load-truename*)
                     :device (pathname-device *load-truename*)
                     :directory (pathname-directory *load-truename*))))

(defun run ()
  "Run the Lisp test suite for ABCL."
  (let ((*default-pathname-defaults* *abcl-test-directory*))
    (do-tests)))

;;; XXX move this into test-utilities.lisp?
(defvar *last-run-matching* "url-pathname")

(defun do-tests-matching (&optional (match *last-run-matching*))
  "Run all tests in suite whose symbol contains MATCH in a case-insensitive manner."
  (setf *last-run-matching* match)
  (let* ((matching (string-upcase match))
         (count 0))
    (mapcar (lambda (entry) 
              (if (search matching (symbol-name (abcl-rt::name entry)))
                  (setf (abcl-rt::pend entry) t
                        count (1+ count))
                  (setf (abcl-rt::pend entry) nil)))
            (rest abcl-rt::*entries*))
    (format t "Performing ~A tests matching '~A'.~%" count matching)
    (abcl-rt::do-entries t)))

;;; Deprecated 
(setf (symbol-function 'run-matching) #'do-tests-matching)
(setf (symbol-function 'do-matching) #'do-tests-matching)

    


	
