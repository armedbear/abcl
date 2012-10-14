;;; require.lisp
;;; 
;;; Copyright (C) 2012 Mark Evenson
;;; $Id$

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

(require :java)
(in-package :system)

(defun ascii-digest (digest)
  (format nil "~{~X~}"
          (mapcar (lambda (b) (if (< b 0) (+ 256 b) b))
                  (java::list-from-jarray digest))))
(export 'sha256 :system)
(defun sha256 (&rest paths-or-strings)
  (cond 
    ((= 1 (length paths-or-strings))
     (typecase paths-or-strings
       (pathname
        (ascii-digest (digest (first paths-or-strings) 'nio)))
       (string (error "Somebody implement me please")))) ; FIXME
        
    ((consp paths-or-strings)
     (concatenate 'string
                  (append
                   (mapcar #'ascii-digest
                           (mapcar (lambda (p)
                                     (funcall #'digest p 'nio))
                                   paths-or-strings)))))
    ((null paths-or-strings)
     nil)))
                                      
           
(defgeneric digest (url algorithim  &optional (digest 'sha-256))
  (:documentation "Digest byte based resource at URL with ALGORITHIM."))
(defun digest-path (path) (ascii-digest (digest path 'nio 'sha-256)))

(defvar *digest-types* 
  '((sha-1 . "SHA-1")
    (sha-256 . "SHA-256")
    (sha-512 . "SHA-512"))
  "Normalization of cryptographic digest naming.")

;;; Implementation
(defconstant +byte-buffer-rewind+ 
  (java:jmethod "java.nio.ByteBuffer" "rewind"))
(defconstant +byte-buffer-get+ 
  (java:jmethod "java.nio.ByteBuffer" "get" "[B" "int" "int"))
(defconstant +digest-update+ 
  (java:jmethod "java.security.MessageDigest" "update" "[B" "int" "int"))

(defmethod digest ((url t) (algorithim (eql 'nio)) &optional (digest 'sha-256))
  "Calculate digest with default of :SHA-256 pathname specified by URL.
Returns an array of JVM primitive signed 8-bit bytes.

*DIGEST-TYPES* controls the allowable digest types."

 (let* ((digest-type (cdr (assoc digest *digest-types*)))
        (digest (java:jstatic "getInstance" "java.security.MessageDigest" digest-type))
        (namestring (if (pathnamep url) (namestring url) url))
        (file-input-stream (java:jnew "java.io.FileInputStream" namestring))
        (channel (java:jcall "getChannel" file-input-stream))
        (length 8192)
        (buffer (java:jstatic "allocateDirect" "java.nio.ByteBuffer" length))
        (array (java:jnew-array "byte" length)))
   (do ((read (java:jcall "read" channel buffer)
              (java:jcall "read" channel buffer)))
       ((not (> read 0)))
     (java:jcall +byte-buffer-rewind+ buffer)
     (java:jcall +byte-buffer-get+ buffer array 0 read)
     (java:jcall +byte-buffer-rewind+ buffer)
     (java:jcall +digest-update+ digest array 0 read))
   (java:jcall "digest" digest)))

;;(defmethod digest ((s string) (algorithim (eql 'nio)) &optional (digest 'sha-256))
;;  (warn "Unimplemented."))
;;  (let ((input-stream (
