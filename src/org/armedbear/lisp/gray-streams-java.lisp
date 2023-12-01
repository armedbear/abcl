(require :gray-streams)
(require :clos)

(in-package :gray-streams/java)
;;;; See <file:GrayStream.java> for the Java proxy that calls into
;;;; these methods.

;;;; N.b. The function symbols seemingly have to be unique across all
;;;; packages for this to work, hence the "java/…" prefixes.

(defun find-method-or-nil (method specialization)
  "Either return the method object for generic METHOD with a one argument
  SPECIALIZATION or nil if it doesn't exist"
  (find-method method '() (list specialization) nil))

(defun java/input-stream-p (object)
  (let* ((method
           (or
            (find-method-or-nil #'gray-streams::gray-input-stream-p
                                (class-of object))
            (find-method-or-nil #'gray-streams::gray-input-stream-p
                                (find-class 'gray-streams:fundamental-input-stream))))
         (method-function
           (mop:method-function method)))
  (funcall method-function `(,object) nil)))

(defun java/output-stream-p (object)
  (let* ((method
           (or 
            (find-method-or-nil #'gray-streams::gray-output-stream-p
                                (class-of object))
            (find-method-or-nil #'gray-streams::gray-output-stream-p
                                (find-class 'gray-streams:fundamental-output-stream))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/interactive-stream-p (object)
  (let* ((method
           (find-method-or-nil #'gray-streams::gray-interactive-stream-p
                                (class-of object)))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/open-stream-p (object)
  (let* ((method
           (or 
            (find-method-or-nil #'gray-streams::gray-open-stream-p
                                (class-of object))
            (find-method-or-nil #'gray-streams::gray-open-stream-p
                                (find-class 'gray-streams:fundamental-stream))))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object) nil)))

(defun java/element-type (object)
  (let* ((method
           (or
            (find-method-or-nil #'gray-streams:stream-element-type
                                (class-of object))
            (find-method-or-nil #'gray-streams::gray-stream-element-type
                                (find-class 'gray-streams:fundamental-character-stream))
            (find-method-or-nil #'gray-streams::gray-stream-element-type
                                (find-class 'gray-streams:fundamental-binary-stream))))
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
  (let* ((method
           (or
            (find-method #'gray-streams:stream-write-sequence
                         '()
                         (list
                          (class-of object)
                          (find-class t))
                         nil)
            (find-method #'gray-streams:stream-write-sequence
                         '()
                         (list
                          (find-class 'gray-streams:fundamental-character-output-stream)
                          (find-class t))
                         nil)))
         (method-function
           (mop:method-function method)))
    (funcall method-function `(,object ,string ,start ,end) nil)))

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

