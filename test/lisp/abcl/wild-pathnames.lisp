(in-package :abcl.test.lisp)

;;; Various tests for PATHNAMES :WILD and :WILD-INFERIORS

(defvar *test-files*
  '("foo.ext" "a/b/c/foo.ext" "a/d/e/foo.ext" "b/foo.ext" "a/foo.ext"))

(defvar *temp-directory-root* 
  (ext:make-temp-directory))

(defun create-wild-test-hierarchy ()
  (ensure-directories-exist *temp-directory-root*)
  (dolist (file *test-files*)
    (let ((file (merge-pathnames file *temp-directory-root*)))
      (ensure-directories-exist (directory-namestring file))
      (unless (probe-file file)
        (touch file)))))

(defun remove-wild-test-hierarchy ()
  (ignore-errors
    (delete-directory-and-files *temp-directory-root*)))

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
    (with-test-directories
        (let ((results
               (directory (merge-pathnames "**/*.ext"
                                           *temp-directory-root*)))
              (expected
               (loop :for file :in *test-files*
                  :collecting (merge-pathnames file
                                               *temp-directory-root*))))
      (values 
       (eq (length results) (length expected))
      ;; link --> file is not resolved by change in DIRECTORY to :RESOLVE-SYMLINKS nil
       results
       expected
       (set-equal (mapcar #'truename results) 
                  (mapcar #'truename expected)))))
  t)

(deftest wild-pathnames.2
    (equal 
     (first (with-test-directories
                (directory (make-pathname :directory (pathname-directory *temp-directory-root*)
                                          :name :wild :type "ext"
                                          :version :newest))))
     (merge-pathnames *temp-directory-root* "foo.ext"))
  t)

    

