;;; require.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
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

;;; PROVIDE, REQUIRE (from SBCL)
(defun provide (module-name)
  (pushnew (string module-name) *modules* :test #'string=)
  t)

(defun module-provide-system (module) 
  (let ((*readtable* (copy-readtable nil)))
       (handler-case 
           (load-system-file (string-downcase (string module)))
         (t (e) 
           (unless (and (typep e 'error)
                        (search "Failed to find loadable system file"
                                (format nil "~A" e)))
             (format *error-output* "Failed to require  ~A because '~A'~%" 
                     module e))
           nil))))

         ;;   (progn 
         ;;     (format t "BEFORE~%")
         ;;     (load-system-file (string-downcase (string module)))
         ;;     (format t "AFTER~%"))
         ;; ((error (c) 
         ;;   (progn 
         ;;     (format t "MATCHED~%")
         ;;     ;; XXX It would be much better to detect an error
         ;;     ;; type rather than searching for a string, but

         ;;     ;; that's tricky as LOAD-SYSTEM-FILE is such an
         ;;     ;; early primitive.
         ;;     (when (search "Failed to find loadable system file"
         ;;                   (format nil "~A" c))
         ;;       (return-from module-provide-system (values nil c)))))))))
    
(defvar *module-provider-functions* nil)

(defun require (module-name &optional pathnames)
  (unless (member (string module-name) *modules* :test #'string=)
    (let ((saved-modules (copy-list *modules*)))
      (cond (pathnames
             (unless (listp pathnames) (setf pathnames (list pathnames)))
             (dolist (x pathnames)
               (load x)))
            (t
             (unless (some (lambda (p) (funcall p module-name))
                           (append (list #'module-provide-system)
                                 sys::*module-provider-functions*))
               (warn "Failed to require ~A." module-name))))
      (set-difference *modules* saved-modules))))

