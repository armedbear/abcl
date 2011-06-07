;;; make-hash-table.lisp
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

(defun make-hash-table (&key (test 'eql) (size 11) (rehash-size 1.5)
                             (rehash-threshold 0.75)
                             (weakness nil))
  (setf test (coerce-to-function test))
  (unless (and (integerp size) (>= size 0))
    (error 'type-error :datum size :expected-type '(integer 0)))
  (let ((size (max 11 (min size array-dimension-limit)))
        (weakness-types '(or (eql :key) (eql :value)
                             (eql :key-and-value)
                             (eql :key-or-value))))
    (if weakness
        (if (not (typep weakness weakness-types))
            (error 'type-error :datum weakness 
                   :expected-type weakness-types)
            (%make-weak-hash-table test size rehash-size 
                                   rehash-threshold weakness))
	(%make-hash-table test size 
                          rehash-size rehash-threshold))))

    
  
