(in-package :extensions)

;;; TODO: submit upstream patch to <cffi/src/cffi-abcl.lisp> for removal
(defun write-class (class-bytes pathname)
  "Write the Java byte[] array CLASS-BYTES to PATHNAME."
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(unsigned-byte 8))
    (dotimes (i (java:jarray-length class-bytes))
      (write-byte (java:jarray-ref class-bytes i) stream))))

(defun read-class (pathname)
  "Read the file at PATHNAME as a Java byte[] array"
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((length
             (file-length stream))
           (array
             (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence array stream :end length)
      (java:jnew-array-from-array "byte" array))))
  
  
(export '(write-class
          read-class)
        :extensions)

