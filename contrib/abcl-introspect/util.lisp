(in-package :extensions)

;;; TODO: submit upstream patch to <cffi/src/cffi-abcl.lisp> for removal
(defun write-class (class-bytes pathname)
  "Write the Java byte[] array CLASS-BYTES to PATHNAME."
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(signed-byte 8))
    (dotimes (i (java:jarray-length class-bytes))
      (write-byte (java:jarray-ref class-bytes i) stream))))

(export '(write-class) :extensions)

