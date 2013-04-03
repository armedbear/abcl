;;; fasl-concat.lisp
;;;
;;; Copyright (C) 2013 Erik Huelsmann
;;; $Id$
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
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.


(in-package #:system)


(export '(concatenate-fasls))


(defun pathname-directory-p (pathname)
  (and (null (pathname-type pathname))
       (null (pathname-name pathname))
       (null (pathname-version pathname))))

(defun load-concatenated-fasl (sub-fasl)
  (let ((fasl-path (merge-pathnames (make-pathname :directory (list :relative
                                                                    sub-fasl)
                                                   :name "__loader__"
                                                   :type "_")
                                    *load-truename-fasl*)))
    (load fasl-path)))

(defun concatenate-fasls (inputs output)
  (let ((directory (ext:make-temp-directory))
        paths)
    (unwind-protect
         (let* ((unpacked (mapcan #'(lambda (input)
                                      (sys:unzip input
                                                 (ensure-directories-exist
                                                  (sub-directory directory
                                                                 (pathname-name  input)))))
                                   inputs))
                (chain-loader (make-pathname :name "__loader__"
                                             :type "_"
                                             :defaults directory)))
           (with-open-file (f chain-loader
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :overwrite)
             (write-string
              ";; loader code to delegate loading of the embedded fasls below" f)
             (terpri f)
             (sys::dump-form `(sys:init-fasl :version ,sys:*fasl-version*) f)
             (terpri f)
             (dolist (input inputs)
               (sys::dump-form `(load-concatenated-fasl ,(pathname-name input)) f)
               (terpri f)))
           (setf paths
                 (directory (merge-pathnames
                             (make-pathname :directory '(:relative
                                                         :wild-inferiors)
                                            :name "*"
                                            :type "*")
                             directory)))
           (sys:zip output (remove-if #'pathname-directory-p paths) directory)
           (values directory unpacked chain-loader))
      (dolist (path paths)
        (ignore-errors (delete-file path)))
      (ignore-errors (delete-file directory)))))

(defun sub-directory (directory name)
  (merge-pathnames (make-pathname :directory (list :relative name))
                   directory))