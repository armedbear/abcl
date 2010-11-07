(defpackage #:abcl.test.lisp 
  (:use #:cl #:abcl-rt)
  (:nicknames "ABCL-TEST-LISP" "ABCL-TEST")
  (:export 
   #:run #:run-matching
   #:do-test #:do-tests))
(in-package #:abcl.test.lisp)

(defparameter *abcl-test-directory* 
  (if (find :asdf2 *features*)
      (asdf:system-relative-pathname :abcl-test-lisp "test/lisp/abcl/")
      (make-pathname :host (pathname-host *load-truename*)
                     :device (pathname-device *load-truename*)
                     :directory (pathname-directory *load-truename*))))

(defun run ()
  "Run the Lisp test suite for ABCL."
  (let ((*default-pathname-defaults* *abcl-test-directory*))
    (do-tests)))

(defvar *last-run-matching* "url-pathname")

;;; XXX move this into test-utilities.lisp?
(defun run-matching (&optional (match *last-run-matching*))
  "Run all tests in suite whose symbol contains MATCH in a case-insensitive manner."
  (setf *last-run-matching* match)
  (let* ((matching (string-upcase match))
         (tests
          (remove-if-not
           (lambda (name) (search matching name))
           (mapcar (lambda (entry) 
                     (symbol-name (abcl-rt::name entry))) 
                   (rest abcl-rt::*entries*)))))
    (dolist (test tests)
      (do-test (intern test :abcl.test.lisp)))))
    


	