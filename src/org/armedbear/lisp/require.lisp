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
        (progn 
          (load-system-file (string-downcase (string module)))
          (provide module))
      (t (e) 
        (unless (and (typep e 'error)
                     (search "Failed to find loadable system file"
                             (format nil "~A" e)))
          (format *error-output* "Failed to require  ~A because '~A'~%" 
                  module e))
        nil))))

    
(defvar *module-provider-functions* nil)

(defun require (module-name &optional pathnames)
  (unless (member (string module-name) *modules* :test #'string=)
    (let ((saved-modules (copy-list *modules*)))
      (cond                
;;; Since these are files packaged with the system we ensure that
;;; PROVIDE has been called unless the module has other dependencies
;;; that must be satisfied to be loaded, which is currently only the
;;; case with 'abcl-contrib'.
        (pathnames
         (unless (listp pathnames) (setf pathnames (list pathnames)))
         (dolist (x pathnames)
           (load x))
         (unless (string-equal module-name "abcl-contrib")
           (provide module-name)))
;;; Responsibility for actually calling PROVIDE up to module provider
;;; function
        (t
         (unless (some (lambda (p) (funcall p module-name))
                           (append (list #'module-provide-system)
                                   sys::*module-provider-functions*))
               (error "Don't know how to ~S ~A." 'require module-name))))
      (set-difference *modules* saved-modules))))

