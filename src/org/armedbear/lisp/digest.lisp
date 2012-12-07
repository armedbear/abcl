;;; digest.lisp
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

(defun asciify (digest)
  (format nil "~{~X~}"
          (mapcar (lambda (b) (if (< b 0) (+ 256 b) b))
                  (java::list-from-jarray digest))))


;;;; Really needs to concatenate all input into a single source of
;;;; bytes, running digest over that concatentation
(defun sha256 (&rest paths-or-strings) ;;; XXX more than one arg is very broken.
  "Returned ASCIIfied representation of SHA256 digest of byte-based resource at PATHS-OR-STRINGs." 
  (warn "Unaudited computatation of cryptographic digest initiated.") ;; TODO Need tests with some tool for verification
  (let ((first (first paths-or-strings))
        (rest (rest paths-or-strings)))
    (concatenate 'string 
                  (when first
                    (asciify
                     (typecase first
                       (pathname (digest first))
                       (string (digest first))
                       (null)
                       (list
                        (concatenate 'string 
                                     (sha256 (first first))
                                     (sha256 (rest first)))))))
                  (when rest
                    (sha256 rest)))))
                        
#+nil ;; Bugs out the compiler 
(defun sha256 (paths-or-strings)   
  (labels ((walk (p-or-s)
             ((atom p-or-s)
              (typecase p-or-s 
                (pathname
                 (digest-path p-or-s))
                (string 
                 (error "Somebody implement me please"))))
             ((cons p-or-s)
              (walk (first p-or-s)
                    (rest p-or-s)))))
         (concatenate 'string
                      (walk paths-or-strings))))

           
(defgeneric digest (resource &key (digest 'sha-256))
  (:documentation "Digest byte based resource at RESOURCE."))
(defun digest-path (path) (asciify-digest (digest path 'nio 'sha-256)))

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

(defmethod digest ((url pathname) &key (digest 'sha-256))
  (digest-nio url :digest digest))

(defun digest-nio (source &key (digest 'sha-256))
  "Calculate digest with default of :SHA-256 pathname specified by URL.
Returns an array of JVM primitive signed 8-bit bytes.

Uses \"New I/O\" in JVM \"worse named API of all time\".

*DIGEST-TYPES* controls the allowable digest types."
  (let* 
      ((channel (typecase source
                    (pathname 
                     (java:jcall "getChannel" (java:jnew "java.io.FileInputStream" 
                                                       (namestring source))))
                     (string 
                      (java:jstatic "newChannel" "java.nio.channels.Channels" 
                                    (java:jnew "java.io.ByteArrayInputStream" 
                                               (java:jcall "getBytes" source))))
                   (error "Typecase failed of object of type ~S." source)))
       (digest-type (cdr (assoc digest *digest-types*)))
       (digest (java:jstatic "getInstance" "java.security.MessageDigest" digest-type))
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

(defmethod digest ((source string) &key (digest 'sha-256))
    (digest-nio source :digest digest))

(export 'sha256 :system)
