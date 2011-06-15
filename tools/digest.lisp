(defvar *digest-types* 
  '((:sha-1 . "SHA-1")
    (:sha-256 . "SHA-256")
    (:sha-512 . "SHA-512")))

(defconstant +byte-buffer-rewind+ 
  (jmethod "java.nio.ByteBuffer" "rewind"))
(defconstant +byte-buffer-get+ 
  (jmethod "java.nio.ByteBuffer" "get" "[B" "int" "int"))
(defconstant +digest-update+ 
  (jmethod "java.security.MessageDigest" "update" "[B" "int" "int"))

;;;  needs ABCL svn > r13328 and is probably not faster than the NIO version

(defun digest-file-1 (path &key (digest :sha-256))
 (let* ((digest-type (cdr (assoc digest *digest-types*)))
        (digest (jstatic "getInstance" "java.security.MessageDigest" digest-type))
        (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
   (with-open-file (input path :element-type '(unsigned-byte 8))
     (loop :for bytes = (read-sequence buffer input)
        :while (plusp bytes)
        :do 
        (jcall-raw "update" digest 
                   (jnew-array-from-array "byte" buffer) 0 bytes))
     (jcall "digest" digest))))

(defun digest-file (path &key (digest :sha-256))
 (let* ((digest-type (cdr (assoc digest *digest-types*)))
        (digest (jstatic "getInstance" "java.security.MessageDigest" digest-type))
        (namestring (if (pathnamep path) (namestring path) path))
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

(defun ascii-digest (digest)
  (format nil "~{~X~}"
          (mapcar (lambda (b) (if (< b 0) (+ 256 b) b))
                  (java::list-from-jarray digest))))

(defun benchmark (directory)
    (let (results start-1 end-1 start-2 end-2)
      (dolist (entry (directory directory))
        (setf start-1 (get-internal-run-time))
        (digest-file-1 entry)
        (setf end-1 (get-internal-run-time))
        (setf start-2 (get-internal-run-time))
        (digest-file entry)
        (setf end-2 (get-internal-run-time))
        (let ((result (list entry (- end-1 start-1) (- end-2 start-2))))
          (format t "~&~A" result)
          (push result results)))
      results))
    
        


    

