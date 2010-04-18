;;; compiler-error.lisp
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

(export '(*compiler-error-context*
          compiler-style-warn
          compiler-warn
          compiler-error
          internal-compiler-error
          compiler-unsupported))

(defvar *compiler-error-context* nil)

(defun compiler-style-warn (format-control &rest format-arguments)
  (warn 'style-warning
        :format-control format-control
        :format-arguments format-arguments))

(defun compiler-warn (format-control &rest format-arguments)
  (warn 'warning
        :format-control format-control
        :format-arguments format-arguments))

(defun compiler-error (format-control &rest format-arguments)
  (error 'compiler-error
         :format-control format-control
         :format-arguments format-arguments))

(defun internal-compiler-error (format-control &rest format-arguments)
  (cerror "Eventually use interpreted form instead" 
          'internal-compiler-error
          :format-control format-control
          :format-arguments format-arguments))

(defun compiler-unsupported (format-control &rest format-arguments)
  (error 'compiler-unsupported-feature-error
         :format-control format-control
         :format-arguments format-arguments))
