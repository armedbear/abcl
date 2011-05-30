;;; dotimes.lisp
;;;
;;; Copyright (C) 2004-2005 Peter Graves
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

(defmacro dotimes ((var count &optional (result nil)) &body body)
  (multiple-value-bind (forms decls) (parse-body body nil)
    (let ((index (gensym "INDEX-"))
          (top   (gensym "TOP-")))
      (if (numberp count)
          `(block nil
             (let ((,var 0)
                   (,index 0))
               (declare (type (integer 0 ,count) ,index))
               (declare (ignorable ,var))
               ,@decls
               (when (> ,count 0)
                 (tagbody
                  ,top
                  ,@forms
                  (setq ,index (1+ ,index))
                  (setq ,var ,index)
                  (when (< ,index ,count)
                    (go ,top))))
               (progn ,result)))
          (let ((limit (gensym "LIMIT-")))
            ;; Annotations for the compiler.
            (setf (get limit 'dotimes-limit-variable-p) t)
            (setf (get index 'dotimes-index-variable-name) index)
            (setf (get index 'dotimes-index-variable-p) t)
            (setf (get limit 'dotimes-limit-variable-name) limit)
            `(block nil
               (let ((,var 0)
                     (,limit ,count)
                     (,index 0))
                 (declare (ignorable ,var))
                 ,@decls
                 (when (> ,limit 0)
                   (tagbody
                    ,top
                    ,@forms
                    (setq ,index (1+ ,index))
                    (setq ,var ,index)
                    (when (< ,index ,limit)
                      (go ,top))))
                 (progn ,result))))))))
