(require :gray-streams)
(require :clos)

(in-package :gray-streams/java)
;;;; See <file:GrayStream.java> for the Java proxy that calls into
;;;; these methods.

;;;; N.b. The function symbols seemingly have to be unique across all
;;;; packages for this to work, hence the "java/…" prefixes.
;;;;
;;;; A possible optimization would be able to make these calls
;;;; directly from GrayStream.java, but since a majority of the
;;;; generic method machinery gets redfined/augmented Lisp side via
;;;; clos.lisp this probably won't be particularly easy.

(defun java/input-stream-p (object)
  (gray-streams::gray-input-stream-p object))

(defun java/output-stream-p (object)
  (gray-streams::gray-output-stream-p object))

(defun java/interactive-stream-p (object)
  (gray-streams::gray-interactive-stream-p object))

(defun java/open-stream-p (object)
  (gray-streams::gray-open-stream-p object))

(defun java/element-type (object)
  (gray-streams::gray-stream-element-type object))

(defun java/force-output (object)
  (gray-streams:stream-force-output object))

(defun java/write-string (object string)
  (gray-streams:stream-write-string object string))
  
(defun java/write-char (object char)
  (gray-streams:stream-write-char object char))

(defun java/write-chars (object string start end) ; The defaults for start and end are 0 and nil, respectively.
  (gray-streams:stream-write-sequence object string start end))

(defun java/fresh-line (object)
  (gray-streams:stream-fresh-line object))

(defun java/read-char (object)
  (gray-streams:stream-read-char object))

(defun java/unread-char (object n)
  (gray-streams:stream-unread-char object n))

(defun java/listen (object)
  (gray-streams:stream-listen object))

(defun java/read-byte (object)
  (gray-streams:stream-read-byte object))

(defun java/write-byte (object n)
  (gray-streams:stream-read-byte object n))

(defun java/finish-output (object)
  (gray-streams:stream-finish-output object))

(defun java/file-position (object)
  (gray-streams:stream-file-position object))

(defun java/file-length (object)
  (gray-streams:stream-file-length object))

(defun java/line-column (object)
  (gray-streams:stream-line-column object))

(provide :gray-streams-java)

