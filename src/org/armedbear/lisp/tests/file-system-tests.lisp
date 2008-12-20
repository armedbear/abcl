;;; file-system-tests.lisp
;;;
;;; Copyright (C) 2005 Peter Graves
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#+sbcl
(require '#:sb-posix)

(load "test-utilities.lisp")

(in-package #:test)

(export '(pathnames-equal-p run-shell-command copy-file make-symbolic-link
          touch make-temporary-directory delete-directory-and-files))

(defparameter *this-file*
  (merge-pathnames (make-pathname :type "lisp")
                   *load-truename*))

(defparameter *this-directory*
  (make-pathname :host (pathname-host *load-truename*)
                 :device (pathname-device *load-truename*)
                 :directory (pathname-directory *load-truename*)))

(defmacro signals-error (form error-name)
  `(locally (declare (optimize safety))
     (handler-case ,form
       (error (c) (typep c ,error-name))
       (:no-error (&rest ignored) (declare (ignore ignored)) nil))))

(defun pathnames-equal-p (pathname1 pathname2)
  #-(or allegro clisp cmu lispworks)
  (equal pathname1 pathname2)
  #+(or allegro clisp cmu)
  (and (pathnamep pathname1)
       (pathnamep pathname2)
       (equal (pathname-host pathname1) (pathname-host pathname2))
       (equal (pathname-device pathname1) (pathname-device pathname2))
       (equal (pathname-directory pathname1) (pathname-directory pathname2))
       (equal (pathname-name pathname1) (pathname-name pathname2))
       (equal (pathname-type pathname1) (pathname-type pathname2))
       (or (equal (pathname-version pathname1) (pathname-version pathname2))
           (and (member (pathname-version pathname1) '(:newest nil))
                (member (pathname-version pathname2) '(:newest nil))
                t)))
  #+lispworks
  (string= (namestring pathname1) (namestring pathname2)))

#+abcl
(defun run-shell-command (command &key directory (output *standard-output*))
  (ext:run-shell-command command :directory directory :output output))

#+allegro
(defun run-shell-command (command &key directory (output *standard-output*))
  (excl:run-shell-command command :directory directory :input nil :output output))

#+clisp
(defun run-shell-command (command &key directory (output *standard-output*))
  (declare (ignore output)) ;; FIXME
  (let (status old-directory)
    (when directory
      (setf old-directory (ext:cd))
      (ext:cd directory))
    (unwind-protect
        (setf status (ext:run-shell-command command))
      (when old-directory
        (ext:cd old-directory)))
    (cond ((numberp status)
           status)
          ((eq status t)
           0)
          (t
           -1))))

#+cmu
(defun run-shell-command (command &key directory (output *standard-output*))
  (when directory
    (setf command (concatenate 'string
                               "\\cd \""
                               (namestring (pathname directory))
                               "\" && "
                               command)))
  (ext:process-exit-code
   (ext:run-program
    "/bin/sh"
    (list  "-c" command)
    :input nil :output output)))

#+sbcl
(defun run-shell-command (command &key directory (output *standard-output*))
  (when directory
    (setf command (concatenate 'string
                               "\\cd \""
                               (namestring (pathname directory))
                               "\" && "
                               command)))
  (sb-ext:process-exit-code
   (sb-ext:run-program
    "/bin/sh"
    (list  "-c" command)
    :input nil :output output)))

#+lispworks
(defun run-shell-command (command &key directory (output *standard-output*))
  (when directory
    #+unix
    (setf command (concatenate 'string
                               "\\cd \""
                               (namestring (pathname directory))
                               "\" && "
                               command)))
  (system:call-system-showing-output
   command
   :shell-type "/bin/sh"
   :output-stream output))

(defun copy-file (from to)
  (let* ((from-namestring (namestring (pathname from)))
         (to-namestring (namestring (pathname to)))
         (command (concatenate 'string "cp " from-namestring " " to-namestring)))
    (zerop (run-shell-command command))))

(defun make-symbolic-link (from to)
  (let* ((from-namestring (namestring (pathname from)))
         (to-namestring (namestring (pathname to)))
         (command (concatenate 'string "ln -s " from-namestring " " to-namestring)))
    (zerop (run-shell-command command))))

(defun probe-directory (pathname)
  #+abcl (ext:probe-directory pathname)
  #+allegro (excl:probe-directory pathname)
  #+clisp (ignore-errors (ext:probe-directory pathname))
  #+cmu (probe-file pathname) ; FIXME
  #+sbcl (probe-file pathname) ; FIXME
  #+lispworks (probe-file pathname)
  )

(defun file-directory-p (pathname)
  #+abcl (ext:file-directory-p pathname)
  #+allegro (excl:file-directory-p pathname)
  #-(or abcl allegro)
  (let* ((namestring (namestring pathname))
         (len (length namestring))
         (last-char (and (> len 0) (char namestring (1- len)))))
    (eql last-char #+windows #\\ #-windows #\/)))

(defun make-directory (pathname)
  #+allegro
  (excl:make-directory pathname)
  #-allegro
  (and (ensure-directories-exist pathname) t))

(defun delete-directory (pathname)
  #+abcl (delete-file pathname)
  #+allegro (excl:delete-directory pathname)
  #+clisp (ext:delete-dir (namestring pathname))
  #+cmu (unix:unix-rmdir (namestring pathname))
  #+sbcl (zerop (sb-posix:rmdir (namestring pathname)))
  #+lispworks (lw:delete-directory pathname)
  )

;; This approach is race-prone, but it should be adequate for our limited
;; purposes here.
(defun make-temporary-filename (directory)
  (unless (probe-directory directory)
    (error "The directory ~S does not exist." directory))
  (dotimes (i 10)
    (let ((pathname (merge-pathnames (make-pathname :name (symbol-name (gensym))
                                                    :type nil)
                                     directory)))
      (unless (probe-file pathname)
        (return-from make-temporary-filename pathname))))
  (error "Unable to create a temporary filename in ~S" directory))

(defun touch (filespec)
  (with-open-file (stream filespec :direction :output :if-exists :error)))

(defun make-temporary-directory (parent-directory)
  (let* ((tmp (make-temporary-filename parent-directory))
         (directory-namestring (concatenate 'string (namestring tmp) "/"))
         (directory-pathname (pathname directory-namestring)))
    (make-directory directory-pathname)
    directory-pathname))

(defun delete-directory-and-files (pathspec &key (quiet t) (dry-run nil))
  (let* ((namestring (namestring pathspec))
         (len (length namestring))
         (last-char (and (> len 0) (char namestring (1- len)))))
    (unless (eql last-char #+windows #\\ #-windows #\/)
      (setf namestring (concatenate 'string namestring #+windows "\\" #-windows "/")))
    (let ((pathname (pathname namestring)))
      (unless (probe-directory pathname)
        (error "Directory does not exist: ~S" pathname))
      (unless quiet
        (format t "~&processing directory ~S~%" pathname))
      (let ((list (directory (make-pathname :name :wild
                                            :type #-clisp :wild #+clisp nil
                                            :defaults pathname))))
        (dolist (x list)
          (cond ((file-directory-p x)
                 (delete-directory-and-files x :quiet quiet))
                (t
                 (unless quiet
                   (format t "~&deleting file ~S~%" x))
                 (unless dry-run
                   (delete-file x)))))
        (unless quiet
          (format t "~&deleting directory ~S~%" pathname))
        (unless dry-run
          (delete-directory pathname))))))

#-(or allegro clisp lispworks windows)
(deftest run-shell-command.1
  (let* ((raw-string
          (with-output-to-string (s) (run-shell-command "pwd"
                                                        :directory *this-directory*
                                                        :output s)))
         (string
          (string-right-trim '(#\newline #\return) raw-string))
         (length (length string)))
    (when (> length 0)
      (unless (eql (char string (1- length)) #\/)
        (setf string (concatenate 'string string (string #\/)))))
    (string= string (directory-namestring *this-directory*)))
  t)

#-(or allegro clisp lispworks windows)
(deftest run-shell-command.2
  (let* ((directory
          (probe-file (merge-pathnames "../" *this-directory*)))
         (raw-string
          (with-output-to-string (s) (run-shell-command "pwd"
                                                        :directory directory
                                                        :output s)))
         (string
          (string-right-trim '(#\newline #\return) raw-string))
         (length (length string)))
    (when (> length 0)
      (unless (eql (char string (1- length)) #\/)
        (setf string (concatenate 'string string (string #\/)))))
    (string= string (directory-namestring directory)))
  t)

(deftest probe-file.1
  (pathnames-equal-p (probe-file *this-file*) *this-file*)
  t)

(deftest probe-file.2
  (let ((pathname #p"."))
    #-clisp
    (pathnames-equal-p (probe-file pathname) (truename pathname))
    #+clisp
    ;; "." names a directory, not a file.
    (signals-error (probe-file pathname) 'file-error))
  t)
#+(and clisp windows)
(pushnew 'probe-file.2 *expected-failures*)

(deftest probe-file.3
  (let ((pathname #p"./"))
    #-clisp
    (pathnames-equal-p (probe-file pathname) *this-directory*)
    #+clisp
    ;; "no file name given"
    (signals-error (probe-file pathname) 'file-error))
  t)

(deftest probe-file.4
  (let ((pathname #p".."))
    #-clisp
    (pathnames-equal-p (probe-file pathname) (truename pathname))
    #+clisp
    ;; ".." names a directory, not a file.
    (signals-error (probe-file pathname) 'file-error))
  t)
#+(and clisp windows)
(pushnew 'probe-file.4 *expected-failures*)

(deftest probe-file.5
  (or
   ;; It might not exist. That's OK.
   (null (probe-directory #p"/home/"))
   (pathnames-equal-p (probe-file #p"/home") (probe-file #p"/home/")))
  t)
#+(or allegro cmu clisp)
(pushnew 'probe-file.5 *expected-failures*)

(deftest truename.1
  (pathnames-equal-p (truename *this-file*) *this-file*)
  t)

(deftest truename.2
  (pathnames-equal-p (truename #p"./") *this-directory*)
  t)

(deftest directory.1
  (let ((list (directory *this-file*)))
    (and
     (= (length list) 1)
     (pathnames-equal-p (car list) *this-file*)))
  t)

;; Verify that DIRECTORY returns nil if the directory is empty.
(deftest directory.2
  (let ((directory-pathname (make-temporary-directory *this-directory*)))
    (unwind-protect
        (directory (make-pathname :name :wild :defaults directory-pathname))
      (delete-directory-and-files directory-pathname)))
  nil)

;; A directory with a one file named "foo".
(deftest directory.3
  (let ((directory-pathname (make-temporary-directory *this-directory*)))
    (unwind-protect
        (let ((file-pathname (make-pathname :name "foo" :defaults directory-pathname)))
          (touch file-pathname)
          (let ((directory (directory (make-pathname :name :wild
                                                     :defaults directory-pathname))))
            (and (listp directory)
                 (= (length directory) 1)
                 (pathnames-equal-p (car directory) file-pathname))))
      (delete-directory-and-files directory-pathname)))
  t)

;; Same as DIRECTORY.3, but use :type :wild for the wildcard.
(deftest directory.4
  (let ((directory-pathname (make-temporary-directory *this-directory*)))
    (unwind-protect
        (let ((file-pathname (make-pathname :name "foo" :defaults directory-pathname)))
          (touch file-pathname)
          (let ((directory (directory (make-pathname :name :wild
                                                     :type :wild
                                                     :defaults directory-pathname))))
            (and (listp directory)
                 (= (length directory) 1)
                 (pathnames-equal-p (car directory) file-pathname))))
      (delete-directory-and-files directory-pathname)))
  t)
#+clisp
;; A pathname with type nil does not match a wildcard with type :WILD.
(pushnew 'directory.4 *expected-failures*)

#-windows
(deftest symlink.1
  (let* ((tmp1 (make-temporary-filename *this-directory*))
         (tmp2 (make-temporary-filename *this-directory*)))
    (unwind-protect
        (values
         (unwind-protect
             (and
              ;; Copy this file to tmp1.
              (copy-file *this-file* tmp1)
              (pathnames-equal-p (probe-file tmp1) tmp1)
              ;; Create tmp2 as a symlink to tmp1.
              (make-symbolic-link tmp1 tmp2)
              ;; Verify that the symlink exists and points to the copy.
              (pathnames-equal-p (probe-file tmp2) tmp1)
              (pathnames-equal-p (truename tmp2) tmp1))
           ;; Delete the symlink.
           (when (probe-file tmp2)
             (delete-file tmp2)))
         ;; Copy should still exist after symlink is deleted.
         (pathnames-equal-p (probe-file tmp1) tmp1))
      (when (probe-file tmp1)
        (delete-file tmp1))))
  t t)
#+allegro
;; Allegro's PROBE-FILE doesn't follow the symlink.
(pushnew 'symlink.1 *expected-failures*)

#-windows
(deftest symlink.2
  (let* ((copy (make-temporary-filename *this-directory*))
         (link (make-temporary-filename *this-directory*))
         directory)
    (unwind-protect
        (and
         ;; Copy this file to copy.
         (copy-file *this-file* copy)
         ;; Verify that copy exists.
         (pathnames-equal-p (probe-file copy) copy)
         ;; Create link as a symlink to copy.
         (make-symbolic-link copy link)
         ;; Verify that the symlink appears in the directory listing.
         (setf directory (directory link))
         (= (length directory) 1)
         ;; The directory listing should contain the truename of the symlink.
         (pathnames-equal-p (car directory) (truename link)))
      (progn
        ;; Clean up.
        (when (probe-file link)
          (delete-file link))
        (when (probe-file copy)
          (delete-file copy)))))
  t)
#+allegro
(pushnew 'symlink.2 *expected-failures*)

;; user-homedir-pathname &optional host => pathname

;; "USER-HOMEDIR-PATHNAME returns a pathname without any name, type, or version
;; component (those components are all nil) for the user's home directory on
;; HOST. If it is impossible to determine the user's home directory on HOST,
;; then nil is returned. USER-HOMEDIR-PATHNAME never returns nil if HOST is not
;; supplied."
(deftest user-homedir-pathname.1
  (let ((pathname (user-homedir-pathname)))
    (values (pathnamep pathname)
            (pathname-name pathname)
            (pathname-type pathname)
            (pathname-version pathname)))
  t nil nil nil)
#+allegro
;; Allegro's version component is :UNSPECIFIC.
(pushnew 'user-homedir-pathname.1 *expected-failures*)

(deftest directory-namestring.1
  (let ((pathname (user-homedir-pathname)))
    (equal (namestring pathname) (directory-namestring pathname)))
  #-windows
  t
  #+windows
  ;; The drive prefix ("C:\\") is not part of the directory namestring.
  nil)
#+clisp
(pushnew 'directory-namestring.1 *expected-failures*)

(deftest directory-namestring.2
  (let ((pathname (user-homedir-pathname)))
    (equal (directory-namestring pathname)
           (namestring (make-pathname :directory (pathname-directory pathname)))))
  t)
#+clisp
(pushnew 'directory-namestring.2 *expected-failures*)

(deftest ensure-directories-exist.1
  (let* ((tmp (make-temporary-filename *this-directory*))
         (directory-namestring (concatenate 'string (namestring tmp) "/"))
         (file-namestring (concatenate 'string directory-namestring "foo.bar")))
    (multiple-value-bind (path created)
        (ensure-directories-exist file-namestring)
      (values
       ;; 1. "The primary value is the given pathspec..."
       #+(or allegro clisp)
       (eq path file-namestring)
       #-(or allegro clisp)
       (pathnames-equal-p (pathname path) (pathname file-namestring))
       ;; 2. Verify that the directory was created.
       created
       ;; 3. Verify that the directory exists.
       #+clisp
       ;; CLISP's PROBE-DIRECTORY just returns T.
       (ext:probe-directory directory-namestring)
       #-clisp
       (pathnames-equal-p (probe-file directory-namestring)
                          (pathname directory-namestring))
       ;; 4. Delete the directory.
       (when (probe-directory directory-namestring)
         (delete-directory directory-namestring))
       ;; 5. Verify that the directory is no longer there.
       (probe-directory directory-namestring))
       ))
  t t t t nil)

;; What happens if you call ENSURE-DIRECTORIES-EXIST with a pathname that has
;; no name, type, or version component?

;; Case 1: the directory in question already exists.
(deftest ensure-directories-exist.2
  (let ((pathname
         (make-pathname :host (pathname-host *this-directory*)
                        :device (pathname-device *this-directory*)
                        :directory (pathname-directory *this-directory*)
                        :name nil :type nil :version nil)))
    (multiple-value-bind (path created)
        (ensure-directories-exist pathname)
      (values
       #+(or allegro clisp)
       (eq path pathname)
       #-(or allegro clisp)
       (pathnames-equal-p (pathname path) (pathname pathname))
       created)))
  t nil)

;; Case 2: the directory in question does not exist.
(deftest ensure-directories-exist.3
  (let* ((tmp (make-temporary-filename *this-directory*))
         (directory-namestring (concatenate 'string (namestring tmp) "/"))
         (pathname (pathname directory-namestring)))
    (multiple-value-bind (path created)
        (ensure-directories-exist pathname)
      (values
       #+(or allegro clisp)
       (eq path pathname)
       #-(or allegro clisp)
       (pathnames-equal-p (pathname path) (pathname pathname))
       created
       (not (null (probe-directory directory-namestring)))
       (when (probe-directory directory-namestring)
         (delete-directory directory-namestring))
       )))
  t t t t)

(do-tests)
