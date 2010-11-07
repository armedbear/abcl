(in-package :abcl.test.lisp)

;;; Various tests for PATHNAMES :WILD and :WILD-INFERIORS

(defvar *test-files*
  '("foo.ext" "a/b/c/foo.ext" "a/d/e/foo.ext" "b/foo.ext" "a/foo.ext"))

(defvar *temp-directory-root* 
  (merge-pathnames "tmp/" *this-directory*))

(defun create-wild-test-hierarchy ()
  (dolist (file *test-files*)
    (let ((file (merge-pathnames file *temp-directory-root*)))
      (ensure-directories-exist (directory-namestring file))
      (touch file))))

(defun remove-wild-test-hierarchy ()
  (delete-directory-and-files *temp-directory-root*))

(defmacro with-test-directories (&rest body)
  `(prog2 (create-wild-test-hierarchy)
          ,@body
     (remove-wild-test-hierarchy)))

(defun set-equal (a b)
  (and
   (= (length a) (length b))
   (subsetp a b :test #'equal)
   (subsetp b a :test #'equal)))
    
(deftest wild-pathnames.1
    (let ((results
           (with-test-directories
               (directory (merge-pathnames "**/*.ext"
                                           *temp-directory-root*))))
          (expected
           (loop :for file :in *test-files*
              :collecting (merge-pathnames file
                                           *temp-directory-root*))))
      (set-equal results expected))
  t)

;;; XXX try to track this down by going to the git version?
;;;
;;; Passing, but some form of :VERSION :NEWEST was failing for
;;; ASDF-2.116 according to Faré in proviate email of 18.08.2010
(deftest wild-pathnames.2
    (equal 
     (first (with-test-directories
                (directory (make-pathname :directory (pathname-directory *temp-directory-root*)
                                          :name :wild :type "ext"
                                          :version :newest))))
     (merge-pathnames *temp-directory-root* "foo.ext"))
  t)


