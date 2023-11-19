(require :gray-streams)
(require :clos)

(in-package :gray-streams/java)
;;;; See <file:GrayStream.java> for the Java proxy that calls into
;;;; these methods.

;;;; N.b. The function symbols seemingly have to be unique across all
;;;; packages for this to work, hence the "java/…" prefixes. 
(defun java/element-type (object)
  (let* ((method
           (or 
            (find-method #'gray-streams:stream-element-type
                         '()
                         (list
                          (class-of object))
                         nil)
            (find-method #'gray-streams::gray-stream-element-type
                         '()
                         (list
                          (class-of object)))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))
  

(defun java/force-output (object)
  (let* ((method
           (find-method #'gray-streams:stream-force-output
                        '()
                        (list
                         (class-of object))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/write-string (object string)
  (let* ((method
           (find-method #'gray-streams:stream-write-string
                        '()
                        (list
                         (class-of object)
                         (find-class t))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object ,string) nil)))
  
(defun java/write-char (object char)
  (let* ((method
           (find-method #'gray-streams:stream-write-char
                        '()
                        (list
                         (class-of object)
                         (find-class t))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object ,char) nil)))

(defun java/write-chars (object string start end) ; The defaults for start and end are 0 and nil, respectively.
  (flet ((find-method-or-nil (specialized-on-class)
           (find-method #'gray-streams:stream-write-sequence
                        '()
                        (list
                         specialized-on-class
                         (find-class t))
                        nil)))
    (let* ((method
             (or
              (find-method-or-nil (class-of object))
              (find-method-or-nil (find-class 'gray-streams:fundamental-character-output-stream))))
           (method-function
             (mop:method-function method)))
      (funcall method-function `(,object ,string ,start ,end) nil))))

(defun java/fresh-line (object)
  (let* ((method
           (find-method #'gray-streams:stream-fresh-line
                        '()
                        (list
                         (class-of object))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/read-char (object)
  (let* ((method
           (find-method #'gray-streams:stream-read-char
                        '()
                        (list
                         (class-of object))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/unread-char (object n)
  (let* ((method
           (find-method #'gray-streams:stream-unread-char
                        '()
                        (list
                         (class-of object)
                         (find-class t))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object ,n) nil)))

(defun java/listen (object)
  (let* ((method
           (find-method #'gray-streams:stream-listen
                        '()
                        (list
                         (class-of object))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/read-byte (object)
  (let* ((method
           (find-method #'gray-streams:stream-read-byte
                        '()
                        (list
                         (class-of object))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/write-byte (object n)
  (let* ((method
           (find-method #'gray-streams:stream-read-byte
                        '()
                        (list
                         (class-of object)
                         (find-class t))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object ,n) nil)))

(defun java/finish-output (object)
  (let* ((method
           (find-method #'gray-streams:stream-finish-output
                        '()
                        (list
                         (class-of object))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/file-position (object)
  (let* ((method
           (find-method #'gray-streams:stream-file-position
                        '()
                        (list
                         (class-of object))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/file-length (object)
  (let* ((method
           (find-method #'gray-streams:stream-file-length
                        '()
                        (list
                         (class-of object))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/line-column (object)
  (let* ((method
           (find-method #'gray-streams:stream-line-column
                        '()
                        (list
                         (class-of object))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))


(provide :gray-streams-java)

