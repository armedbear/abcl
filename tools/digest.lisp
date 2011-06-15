;;;; Cryptographic  message digest calculation with ABCL with different implementations.
;;;;
;;;; Mark <evenson.not.org@gmail.com>
;;;;

(in-package :cl-user)

;;; API
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
  (jmethod "java.nio.ByteBuffer" "rewind"))
(defconstant +byte-buffer-get+ 
  (jmethod "java.nio.ByteBuffer" "get" "[B" "int" "int"))
(defconstant +digest-update+ 
  (jmethod "java.security.MessageDigest" "update" "[B" "int" "int"))

(defmethod digest ((url t) (algorithim (eql 'nio)) &optional (digest 'sha-256))
  "Calculate digest with default of :SHA-256 pathname specified by URL.
Returns an array of JVM primitive signed 8-bit bytes.

*DIGEST-TYPES* controls the allowable digest types."

 (let* ((digest-type (cdr (assoc digest *digest-types*)))
        (digest (jstatic "getInstance" "java.security.MessageDigest" digest-type))
        (namestring (if (pathnamep url) (namestring url) url))
        (file-input-stream (jnew "java.io.FileInputStream" namestring))
        (channel (jcall "getChannel" file-input-stream))
        (length 8192)
        (buffer (jstatic "allocateDirect" "java.nio.ByteBuffer" length))
        (array (jnew-array "byte" length)))
   (do ((read (jcall "read" channel buffer)
              (jcall "read" channel buffer)))
       ((not (> read 0)))
     (jcall +byte-buffer-rewind+ buffer)
     (jcall +byte-buffer-get+ buffer array 0 read)
     (jcall +byte-buffer-rewind+ buffer)
     (jcall +digest-update+ digest array 0 read))
   (jcall "digest" digest)))

(defmethod digest ((url pathname) (algorithim (eql 'lisp)) &optional (digest 'sha-256))
  "Compute digest of URL in Lisp where possible.

Currently much slower that using 'nio.

Needs ABCL svn > r13328."

 (let* ((digest-type (cdr (assoc digest *digest-types*)))
        (digest (jstatic "getInstance" "java.security.MessageDigest" digest-type))
        (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
   (with-open-file (input url :element-type '(unsigned-byte 8))
     (loop 
        :for  
            bytes = (read-sequence buffer input)
        :while 
           (plusp bytes)
        :do 
           (jcall-raw "update" digest 
                      (jnew-array-from-array "byte" buffer) 0 bytes))
     (jcall "digest" digest))))

(defun ascii-digest (digest)
  (format nil "~{~X~}"
          (mapcar (lambda (b) (if (< b 0) (+ 256 b) b))
                  (java::list-from-jarray digest))))

(defun benchmark (directory)
  "For a given DIRECTORY containing a wildcard of files, run the benchmark tests."
  (let (results)
    (flet ((benchmark (task)
             (let (start end result)
               (psetf start (get-internal-run-time)
                      result (push (funcall task) result)
                      end (get-internal-run-time))
               (nconc result (list start (- end start))))))
      (dolist (entry (directory directory))
        (let ((result 
               (list 
                (list 'nio (benchmark (lambda () (digest entry 'nio))))
                (list 'lisp (benchmark (lambda () (digest entry 'lisp)))))))
          (format t "~&~{~A~&~A~}" result)
          (push result results))))))

;;; Deprecated
(setf (symbol-function 'digest-file-1) #'digest)

;;; Test

#|
(benchmark "/usr/local/bin/*") ;; unix
(benchmark "c:/*")             ;; win32
|#
