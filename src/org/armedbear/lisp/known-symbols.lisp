;;; known-symbols.lisp
;;;
;;; Copyright (C) 2005 Peter Graves
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

(require "JAVA")

(export '(lookup-known-symbol))

(let ((symbols (make-hash-table :test 'eq :size 2048)))
  (defun initialize-known-symbols (source ht)
    (let* ((source-class (java:jclass source))
           (class-designator (jvm::make-jvm-class-name source))
           (symbol-class (java:jclass "org.armedbear.lisp.Symbol"))
           (fields (java:jclass-fields source-class :declared t :public t)))
      (dotimes (i (length fields))
        (let* ((field (aref fields i))
               (type (java:jfield-type field)))
          (when (equal type symbol-class)
            (let* ((name (java:jfield-name field))
                   (symbol (java:jfield source-class name)))
              (puthash symbol ht (list name class-designator)))))))
    (hash-table-count ht))

  (initialize-known-symbols "org.armedbear.lisp.Symbol" symbols)
  (initialize-known-symbols "org.armedbear.lisp.Keyword" symbols)
  (initialize-known-symbols "org.armedbear.lisp.Lisp" symbols)
  (initialize-known-symbols "org.armedbear.lisp.Nil" symbols)

  (defun lookup-known-symbol (symbol)
    "Returns the name of the field and its class designator
which stores the Java object `symbol'."
    (values-list (gethash1 symbol symbols))))


(provide '#:known-symbols)
