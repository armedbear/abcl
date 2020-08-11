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

;;;  Determining the underlying unix file descriptor depends on
;;;  navigating private member structures dependent on the hosting
;;;  JVMs wrapping of native socket. The JAVA package currently does
;;;  not have a means for such aggressive intropsection, so we add it
;;;  as a utility here
;;;
;;;  TODO test under :msdog
;;;  TODO Should be in socket.lisp
(defun stream-unix-fd (stream)
  "Return the integer of the underlying unix file descriptor for STREAM

Added by ABCL-INTROSPECT."
  (check-type stream 'system::socket-stream)
  (flet ((get-java-fields (object fields) ;; Thanks to Cyrus Harmon
           (reduce (lambda (x y)
                     (jss:get-java-field x y t))
                          fields
                          :initial-value object))
                (jvm-version ()
                  (read
                   (make-string-input-stream
                    (java:jstatic "getProperty" "java.lang.System"
                                  "java.specification.version")))))
           (ignore-errors
            (get-java-fields (java:jcall "getWrappedInputStream"  ;; TODO: define this as a constant
                                         (two-way-stream-input-stream stream))
                             (if (< (jvm-version) 14)
                                 '("in" "ch" "fdVal")
                                 '("in" "this$0" "sc" "fd" "fd"))))))

(export '(stream-unix-fd) :extensions)
