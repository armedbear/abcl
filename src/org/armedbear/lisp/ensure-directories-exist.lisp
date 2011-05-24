;;; ensure-directories-exist.lisp
;;;
;;; Copyright (C) 2004-2007 Peter Graves
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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

;;; Adapted from SBCL.

(in-package "SYSTEM")

(defun ensure-directories-exist (pathspec &key verbose)
  (let ((pathname (pathname pathspec))
	(created-p nil))
;;; CLHS: Function ENSURE-DIRECTORIES-EXIST "An error of type
;;; file-error is signaled if the host, device, or directory part of
;;; pathspec is wild."
    (when (or (wild-pathname-p pathname :host)
              (wild-pathname-p pathname :device)
              (wild-pathname-p pathname :directory))
      (error 'file-error
	     :format-control "Bad place for a wild HOST, DEVICE, or DIRECTORY component."
	     :pathname pathname))
    (let ((dir (pathname-directory pathname)))
      (loop for i from 1 upto (length dir)
        do (let ((newpath (make-pathname
                           :host (pathname-host pathname)
                           :device (pathname-device pathname)
                           :directory (subseq dir 0 i))))
             (unless (probe-file newpath)
               (let ((namestring (namestring newpath)))
                 (when verbose
                   (fresh-line)
                   (format *standard-output*
                           "Creating directory: ~A~%"
                           namestring))
                 (mkdir namestring)
                 (unless (probe-file namestring)
                   (error 'file-error
                          :pathname pathspec
                          :format-control "Can't create directory ~A."
                          :format-arguments (list namestring)))
                 (setq created-p t)))))
      (values pathname created-p))))
