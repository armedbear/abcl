;;; gray-streams.lisp
;;;
;;; Copyright (C) 2004-2007 Peter Graves, Andras Simon
;;; $Id$
;;;
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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

;;; Adapted from:
;;;; Gray Streams Implementation for Corman Lisp - Version 1.3
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
;;;;
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source
;;;;    distribution.
;;;;
;;;; Notes
;;;; =====
;;;; A simple implementation of Gray streams for Corman Lisp 1.42.
;;;; Gray streams are 'clos' based streams as described at:
;;;;
;;;; ftp://parcftp.xerox.com/pub/cl/cleanup/mail/stream-definition-by-user.mail
;;;;
;;;; Some differences exist between this implementation and the
;;;; specification above. See notes below for details.
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.co.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author,
;;;; Christopher Double, at: chris@double.co.nz
;;;;
;;;; 03/03/2001 - 1.0
;;;;              Initial release.
;;;;
;;;; 20/08/2001 - 1.1
;;;;              Small modifications by Frederic Bastenaire (fba@free.fr)
;;;;              (lines flagged by  ;; # fb 1.01)
;;;;              - Make it work with the READ function by
;;;;                defining %read-char, %read-char-with-error
;;;;               and input-character-stream-p
;;;;              - Add nickname GS to package "GRAY-STREAMS" for ease of use
;;;;              - added missing '*' to *old-write-byte* in gray-write-byte
;;;;
;;;; 03/01/2002 - 1.2
;;;;              Fixed bug with GRAY-WRITE-LINE and GRAY-WRITE-STRING
;;;;              that appeared in Corman Lisp 2.0 due to changes to
;;;;              WRITE-LINE and WRITE-STRING.
;;;;
;;;; 04/01/2002 - 1.3
;;;;              Added support for STREAM-READ-SEQUENCE and STREAM-WRITE-SEQUENCE.
;;;;              Fixed STREAM-WRITE-STRING bug.
;;;;
;;;; Notes
;;;; =====
;;;;
;;;;
;;;; Much of the implementation of the Gray streams below is from the
;;;; document referenced earlier.
;;;;
(defpackage "GRAY-STREAMS"
  (:use
   "COMMON-LISP")
  (:nicknames "GS") ;; # fb 1.01
  (:export
   "FUNDAMENTAL-STREAM"
   "STREAM-OPEN-STREAM-P"
   "STREAM-STREAMP"
   "STREAM-INPUT-STREAM-P"
   "STREAM-OUTPUT-STREAM-P"
   "STREAM-STREAM-ELEMENT-TYPE"
   "STREAM-CLOSE"
   "FUNDAMENTAL-OUTPUT-STREAM"
   "FUNDAMENTAL-INPUT-STREAM"
   "FUNDAMENTAL-CHARACTER-STREAM"
   "FUNDAMENTAL-BINARY-STREAM"
   "STREAM-READ-BYTE"
   "STREAM-WRITE-BYTE"
   "FUNDAMENTAL-CHARACTER-INPUT-STREAM"
   "STREAM-READ-CHAR"
   "STREAM-UNREAD-CHAR"
   "STREAM-READ-CHAR-NO-HANG"
   "STREAM-PEEK-CHAR"
   "STREAM-LISTEN"
   "STREAM-READ-LINE"
   "STREAM-CLEAR-INPUT"
   "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
   "STREAM-WRITE-CHAR"
   "STREAM-LINE-COLUMN"
   "STREAM-START-LINE-P"
   "STREAM-WRITE-STRING"
   "STREAM-TERPRI"
   "STREAM-FRESH-LINE"
   "STREAM-FINISH-OUTPUT"
   "STREAM-FORCE-OUTPUT"
   "STREAM-CLEAR-OUTPUT"
   "STREAM-ADVANCE-TO-COLUMN"
   "STREAM-READ-SEQUENCE"
   "STREAM-WRITE-SEQUENCE"
   "FUNDAMENTAL-BINARY-INPUT-STREAM"
   "FUNDAMENTAL-BINARY-OUTPUT-STREAM"))

(in-package :gray-streams)

(defvar *old-read-char* #'read-char)
(defvar *old-peek-char* #'peek-char)
(defvar *old-unread-char* #'unread-char)
(defvar *old-listen* nil)
(defvar *old-read-line* #'read-line)
(defvar *old-read-char-no-hang* #'read-char-no-hang)
(defvar *old-write-char* #'write-char)
(defvar *old-fresh-line* #'fresh-line)
(defvar *old-terpri* #'terpri)
(defvar *old-write-string* #'write-string)
(defvar *old-write-line* #'write-line)
(defvar *old-force-output* #'sys::%force-output)
(defvar *old-finish-output* #'sys::%finish-output)
(defvar *old-clear-output* #'sys::%clear-output)
(defvar *old-clear-input* #'clear-input)
(defvar *old-read-byte* #'read-byte)
(defvar *old-write-byte* #'write-byte)
(defvar *old-stream-element-type* #'cl::stream-element-type)
(defvar *old-close* #'cl::close)
(defvar *old-input-character-stream-p*
  #'(lambda (s) (and (input-stream-p s) (eql (stream-element-type s) 'character))))
(defvar *old-input-stream-p* #'cl::input-stream-p)
(defvar *old-output-stream-p* #'cl::output-stream-p)
(defvar *old-open-stream-p* #'cl::open-stream-p)
(defvar *old-streamp* #'cl::streamp)
(defvar *old-read-sequence* #'cl::read-sequence)
(defvar *old-write-sequence* #'cl::write-sequence)
(defvar *old-make-two-way-stream* #'cl:make-two-way-stream)
(defvar *old-two-way-stream-input-stream* #'cl:two-way-stream-input-stream)
(defvar *old-two-way-stream-output-stream* #'cl:two-way-stream-output-stream)


(defun old-streamp (stream)
  (or (xp::xp-structure-p stream)
      (funcall *old-streamp* stream)))

(defclass fundamental-stream (standard-object stream))

(defgeneric gray-close (stream &key abort))
(defgeneric gray-open-stream-p (stream))
(defgeneric gray-streamp (stream))
(defgeneric gray-input-stream-p (stream))
(defgeneric gray-input-character-stream-p (stream)) ;; # fb 1.01
(defgeneric gray-output-stream-p (stream))
(defgeneric gray-stream-element-type (stream))


(defmethod stream-streamp ((s fundamental-stream))
  s)

(defclass fundamental-input-stream (fundamental-stream))

(defmethod stream-input-character-stream-p (s)  ;; # fb 1.01
  (and (stream-input-stream-p s)
       (eq (stream-stream-element-type s) 'character)))

(defmethod stream-input-stream-p ((s fundamental-input-stream))
  (declare (ignore s))
  t)

(defclass fundamental-output-stream (fundamental-stream))

(defmethod stream-output-stream-p ((s fundamental-output-stream))
  (declare (ignore s))
  t)

(defclass fundamental-character-stream (fundamental-stream))

(defmethod stream-stream-element-type ((s fundamental-character-stream))
  (declare (ignore s))
  'character)

(defclass fundamental-binary-stream (fundamental-stream))

(defgeneric stream-read-byte (stream))
(defgeneric stream-write-byte (stream integer))

(defclass fundamental-character-input-stream
  (fundamental-input-stream fundamental-character-stream))

(defgeneric stream-read-char (stream))
(defgeneric stream-unread-char (stream character))
(defgeneric stream-read-char-no-hang (stream))
(defgeneric stream-peek-char (stream))
(defgeneric stream-listen (stream))
(defgeneric stream-read-line (stream))
(defgeneric stream-clear-input (stream))

(defmethod stream-peek-char ((stream fundamental-character-input-stream))
  (let ((character (stream-read-char stream)))
    (unless (eq character :eof)
      (stream-unread-char stream character))
    character))

(defmethod stream-listen ((stream  fundamental-character-input-stream))
  (let ((char (stream-read-char-no-hang stream)))
    (and (not (null char))
         (not (eq char :eof))
         (progn
           (stream-unread-char stream char)
           t))))

(defmethod stream-read-line ((stream  fundamental-character-input-stream))
  (let ((line (make-array 64
                          :element-type 'character
                          :fill-pointer 0
                          :adjustable t)))
    (loop
      (let ((character (stream-read-char stream)))
        (if (eq character :eof)
            (return (values line t))
            (if (eql character #\Newline)
                (return (values line nil))
                (vector-push-extend character line)))))))

(defmethod stream-clear-input (stream)
  (declare (ignore stream))
  nil)

(defclass fundamental-character-output-stream
  (fundamental-output-stream fundamental-character-stream))

(defgeneric stream-write-char (stream character))
(defgeneric stream-line-column (stream))
(defgeneric stream-start-line-p (stream))
(defgeneric stream-write-string (stream string &optional start end))
(defgeneric stream-terpri (stream))
(defmethod stream-terpri (stream)
  (stream-write-char stream #\Newline))

(defgeneric stream-fresh-line (stream))
(defgeneric stream-finish-output (stream))
(defgeneric stream-force-output (stream))
(defgeneric stream-clear-output (stream))
(defgeneric stream-advance-to-column (stream column))
(defgeneric stream-read-sequence (stream sequence &optional start end))
(defgeneric stream-write-sequence (stream sequence &optional start end))

(defmethod stream-force-output (stream)
  (declare (ignore stream))
  nil)

(defmethod stream-clear-output (stream)
  (declare (ignore stream))
  nil)

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
  (equal (stream-line-column stream) 0))

(defmethod stream-write-string ((stream fundamental-character-output-stream)
                                string
                                &optional (start 0) end)
  (let ((end (or end (length string))))
    (do ((i start (1+ i)))
        ((>= i end) string)
      (stream-write-char stream (char string i)))))

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
  (if (stream-start-line-p stream)
      nil
      (progn
        (stream-terpri stream)
        t)))

(defmethod stream-advance-to-column ((stream fundamental-character-output-stream)
                                     column)
  (let ((current (stream-line-column stream)))
    (unless (null current)
      (dotimes (i (- current column) t)
        (stream-write-char stream #\Space)))))

(defmethod stream-read-sequence ((stream  fundamental-character-input-stream)
                                 sequence &optional (start 0) end)
  (let ((element-type (stream-element-type stream))
        (end (or end (length sequence)))
        (eof (cons nil nil)))
    (cond
     ((eq element-type 'character)
      (dotimes (count (- end start) (- end start))
        (let ((c (stream-read-char stream nil eof)))
          (if (eq c eof)
              (return (+ count start)))
          (setf (elt sequence (+ count start)) c))))
     ((or (eq element-type 'byte)
          (eq element-type 'unsigned-byte)
          (eq element-type 'signed-byte))
      (dotimes (count (- end start) (- end start))
        (let ((b (stream-read-byte stream nil eof)))
          (if (eq b eof)
              (return (+ count start)))
          (setf (elt sequence (+ count start)) b))))
     (t (error "Cannot READ-SEQUENCE on stream of :ELEMENT-TYPE ~A"
               element-type)))))

(defmethod stream-write-sequence ((stream fundamental-character-output-stream)
                                  sequence &optional (start 0) end)
  (let ((element-type (stream-element-type stream))
        (end (or end (length sequence))))
    (if (eq element-type 'character)
        (do ((n start (+ n 1)))
            ((= n end))
          (stream-write-char
           stream
           (if (typep (elt sequence n) 'number)
               (#+nil ccl:int-char code-char (elt sequence n))
               (elt sequence n))))
        (do ((n start (+ n 1)))
            ((= n end))
          (stream-write-byte (elt sequence n) stream))))    ;; recoded to avoid LOOP, because it isn't loaded yet
  (stream-force-output stream))

(defclass fundamental-binary-input-stream
  (fundamental-input-stream fundamental-binary-stream))

(defclass fundamental-binary-output-stream
  (fundamental-output-stream fundamental-binary-stream))

(defun decode-read-arg (arg)
  (cond ((null arg) *standard-input*)
        ((eq arg t) *terminal-io*)
        (t arg)))

(defun decode-print-arg (arg)
  (cond ((null arg) *standard-output*)
        ((eq arg t) *terminal-io*)
        (t arg)))

(defun report-eof (stream eof-errorp eof-value)
  (if eof-errorp
      (error 'end-of-file :stream stream)
      eof-value))

(defun check-for-eof (value stream eof-errorp eof-value)
  (if (eq value :eof)
      (report-eof stream eof-errorp eof-value)
      value))

(defun gray-read-char (&optional input-stream (eof-errorp t) eof-value recursive-p)
  (let ((stream (decode-read-arg input-stream)))
    (if (old-streamp stream)
        (funcall *old-read-char* stream eof-errorp eof-value recursive-p)
        (check-for-eof (stream-read-char stream) stream eof-errorp eof-value))))

(defun gray-peek-char (&optional peek-type input-stream (eof-errorp t)
                                 eof-value recursive-p)
  (let ((stream (decode-read-arg input-stream)))
    (if (old-streamp stream)
        (funcall *old-peek-char* peek-type stream eof-errorp eof-value recursive-p)
        (if (null peek-type)
            (check-for-eof (stream-peek-char stream) stream eof-errorp eof-value)
            (loop
              (let ((value (stream-peek-char stream)))
                (if (eq value :eof)
                    (return (report-eof stream eof-errorp eof-value))
                    (if (if (eq peek-type t)
                            (not (member value
                                         '(#\space #\tab #\newline #\return)))
                            (char= peek-type value))
                        (return value)
                        (stream-read-char stream)))))))))

(defun gray-unread-char (character &optional input-stream)
  (let ((stream (decode-read-arg input-stream)))
    (if (old-streamp stream)
        (funcall *old-unread-char* character stream)
        (stream-unread-char stream character))))

(defun gray-listen (&optional input-stream)
  (let ((stream (decode-read-arg input-stream)))
    (if (old-streamp stream)
        (funcall *old-listen* stream)
        (stream-listen stream))))

(defun gray-read-line (&optional input-stream (eof-error-p t)
                                 eof-value recursive-p)
  (let ((stream (decode-read-arg input-stream)))
    (if (old-streamp stream)
        (funcall *old-read-line* stream eof-error-p eof-value recursive-p)
        (multiple-value-bind (string eofp)
          (stream-read-line stream)
          (if eofp
              (if (= (length string) 0)
                  (report-eof stream eof-error-p eof-value)
                  (values string t))
              (values string nil))))))

(defun gray-clear-input (&optional input-stream)
  (let ((stream (decode-read-arg input-stream)))
    (if (old-streamp stream)
        (funcall *old-clear-input* stream)
        (stream-clear-input stream))))

(defun gray-read-char-no-hang (&optional input-stream (eof-errorp t)
                                         eof-value recursive-p)
  (let ((stream (decode-read-arg input-stream)))
    (if (old-streamp stream)
        (funcall *old-read-char-no-hang* stream eof-errorp eof-value recursive-p)
        (check-for-eof (stream-read-char-no-hang stream)
                       stream eof-errorp eof-value))))

(defun gray-write-char (character &optional output-stream)
  (let ((stream (decode-print-arg output-stream)))
    (if (old-streamp stream)
        (funcall *old-write-char* character stream)
        (stream-write-char stream character))))

(defun gray-fresh-line (&optional output-stream)
  (let ((stream (decode-print-arg output-stream)))
    (if (old-streamp stream)
        (funcall *old-fresh-line* stream)
        (stream-fresh-line stream))))

(defun gray-terpri (&optional output-stream)
  (let ((stream (decode-print-arg output-stream)))
    (if (old-streamp stream)
        (funcall *old-terpri* stream)
        (stream-terpri stream))))

(defun gray-write-string (string &optional output-stream &key (start 0) end)
  (let ((stream (decode-print-arg output-stream)))
    (if (old-streamp stream)
        (funcall *old-write-string* string stream :start start :end end)
        (stream-write-string stream string start end))))

(defun gray-write-line (string &optional output-stream &key (start 0) end)
  (let ((stream (decode-print-arg output-stream)))
    (if (old-streamp stream)
        (funcall *old-write-line* string stream :start start :end end)
        (progn
          (stream-write-string stream string start end)
          (stream-terpri stream)
          string))))

(defun gray-force-output (&optional output-stream)
  (let ((stream (decode-print-arg output-stream)))
    (if (old-streamp stream)
        (funcall *old-force-output* stream)
        (stream-force-output stream))))

(defun gray-finish-output (&optional output-stream)
  (let ((stream (decode-print-arg output-stream)))
    (if (old-streamp stream)
        (funcall *old-finish-output* stream)
        (stream-finish-output stream))))

(defun gray-clear-output (&optional output-stream)
  (let ((stream (decode-print-arg output-stream)))
    (if (old-streamp stream)
        (funcall *old-clear-output* stream)
        (stream-clear-output stream))))

(defun gray-read-byte (binary-input-stream &optional (eof-errorp t) eof-value)
  (if (old-streamp binary-input-stream)
      (funcall *old-read-byte* binary-input-stream eof-errorp eof-value)
      (check-for-eof (stream-read-byte binary-input-stream)
                     binary-input-stream eof-errorp eof-value)))

(defun gray-write-byte (integer binary-output-stream)
  (if (old-streamp binary-output-stream)
      (funcall *old-write-byte* integer binary-output-stream)
      (stream-write-byte binary-output-stream integer)))

(defmethod stream-line-column ((stream stream))
  nil)

(defun gray-stream-column (&optional input-stream)
  (let ((stream (decode-read-arg input-stream)))
    (if (old-streamp stream)
        nil ;(funcall *old-stream-column* stream)
        (stream-line-column stream))))

(defmethod gray-stream-element-type (stream)
  (funcall *old-stream-element-type* stream))

(defmethod gray-close (stream &key abort)
  (funcall *old-close* stream :abort abort))

(defmethod gray-input-stream-p (stream)
  (funcall *old-input-stream-p* stream))

(defmethod gray-input-character-stream-p (stream)
  (funcall *old-input-character-stream-p* stream))

(defmethod gray-output-stream-p (stream)
  (funcall *old-output-stream-p* stream))

(defmethod gray-open-stream-p (stream)
  (funcall *old-open-stream-p* stream))

(defmethod gray-streamp (stream)
  (funcall *old-streamp* stream))

(defun gray-write-sequence (sequence stream &key (start 0) end)
  (if (old-streamp stream)
      (funcall *old-write-sequence* sequence stream :start start :end end)
      (stream-write-sequence stream sequence start end)))

(defun gray-read-sequence (sequence stream &key (start 0) end)
  (if (old-streamp stream)
      (funcall *old-read-sequence* sequence stream :start start :end end)
      (stream-read-sequence stream sequence start end)))

#|
(defstruct (two-way-stream-g (:include stream))
  input-stream output-stream)

(defun gray-make-two-way-stream (in out)
  (if (and (old-streamp in) (old-streamp out))
      (funcall *old-make-two-way-stream* in out)
      (make-two-way-stream-g :input-stream in :output-stream out)))

(defun gray-two-way-stream-input-stream (stream)
  (if (old-streamp stream)
      (funcall *old-two-way-stream-input-stream* stream)
      (two-way-stream-g-input-stream stream)))

(defun gray-two-way-stream-output-stream (stream)
  (if (old-streamp stream)
      (funcall *old-two-way-stream-output-stream* stream)
      (two-way-stream-g-output-stream stream)))

|#

(setf (symbol-function 'common-lisp::read-char) #'gray-read-char)
(setf (symbol-function 'common-lisp::peek-char) #'gray-peek-char)
(setf (symbol-function 'common-lisp::unread-char) #'gray-unread-char)
(setf (symbol-function 'common-lisp::read-line) #'gray-read-line)
(setf (symbol-function 'common-lisp::clear-input) #'gray-clear-input)
(setf (symbol-function 'common-lisp::read-char-no-hang) #'gray-read-char-no-hang)
(setf (symbol-function 'common-lisp::write-char) #'gray-write-char)
(setf (symbol-function 'common-lisp::fresh-line) #'gray-fresh-line)
(setf (symbol-function 'common-lisp::terpri) #'gray-terpri)
(setf (symbol-function 'common-lisp::write-string) #'gray-write-string)
(setf (symbol-function 'common-lisp::write-line) #'gray-write-line)
(setf (symbol-function 'sys::%force-output) #'gray-force-output)
(setf (symbol-function 'sys::%finish-output) #'gray-finish-output)
(setf (symbol-function 'sys::%clear-output) #'gray-clear-output)
(setf (symbol-function 'common-lisp::read-byte) #'gray-read-byte)
(setf (symbol-function 'common-lisp::write-byte) #'gray-write-byte)
(setf (symbol-function 'common-lisp::stream-column) #'gray-stream-column)
(setf (symbol-function 'common-lisp::stream-element-type) #'gray-stream-element-type)
(setf (symbol-function 'common-lisp::close) #'gray-close)
(setf (symbol-function 'common-lisp::input-stream-p) #'gray-input-stream-p)
(setf (symbol-function 'common-lisp::input-character-stream-p) #'gray-input-character-stream-p)  ;; # fb 1.01
(setf (symbol-function 'common-lisp::output-stream-p) #'gray-output-stream-p)
(setf (symbol-function 'common-lisp::open-stream-p) #'gray-open-stream-p)
(setf (symbol-function 'common-lisp::streamp) #'gray-streamp)
(setf (symbol-function 'common-lisp::read-sequence) #'gray-read-sequence)
(setf (symbol-function 'common-lisp::write-sequence) #'gray-write-sequence)

#|
(setf (symbol-function 'common-lisp::make-two-way-stream) #'gray-make-two-way-stream)
(setf (symbol-function 'common-lisp::two-way-stream-input-stream) #'gray-two-way-stream-input-stream)
(setf (symbol-function 'common-lisp::two-way-stream-output-stream) #'gray-two-way-stream-output-stream)
|#

(provide 'gray-streams)
