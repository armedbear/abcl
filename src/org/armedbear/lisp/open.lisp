;;; open.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
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

;;; Adapted from SBCL.

(in-package #:system)

(defun upgraded-element-type-bits (bits)
  (if (zerop (mod bits 8))
      bits
      (+ bits (- 8 (mod bits 8)))))

(defun upgraded-element-type (element-type)
  (setf element-type (normalize-type element-type))
  (let ((ok nil))
    (if (atom element-type)
        (case element-type
          ((character base-char)
           (setf ok t))
          ((unsigned-byte signed-byte)
           (setf element-type (list element-type 8)
                 ok t))
          (bit
           (setf element-type (list 'unsigned-byte (upgraded-element-type-bits 1))
                 ok t))
          (integer
           (setf element-type '(signed-byte 8)
                 ok t)))
        (cond ((eq (%car element-type) 'or)
               (let ((types (mapcar #'upgraded-element-type (%cdr element-type)))
                     (result '(unsigned-byte 8)))
                 (dolist (type types)
                   (when (eq (car type) 'signed-byte)
                     (setf (car result) 'signed-byte))
                   (setf (cadr result) (max (cadr result) (cadr type))))
                 (setf element-type result
                       ok t)))
              ((and (= (length element-type) 2)
                    (memq (%car element-type) '(unsigned-byte signed-byte)))
               (let ((type (car element-type))
                     (width (cadr element-type)))
                 (setf element-type (list type
                                          (upgraded-element-type-bits width))
                       ok t)))
              ((eq (car element-type) 'integer)
               (case (length element-type)
                 (2
                  (setf element-type '(signed-byte 8)
                        ok t))
                 (3
                  (let ((low (cadr element-type))
                        (high (caddr element-type)))
                    (when (consp low)
                      (setf low (1+ (%car low))))
                    (when (consp high)
                      (setf high (1- (%car high))))
                    (setf element-type
                          (cond ((eq high '*)
                                 (if (minusp low) '(signed-byte 8) '(unsigned-byte 8)))
                                ((minusp low)
                                 (list 'signed-byte
                                       (upgraded-element-type-bits (max (1+ (integer-length low))
                                                                        (integer-length high)))))
                                (t
                                 (list 'unsigned-byte
                                       (upgraded-element-type-bits (integer-length high)))))
                          ok t)))))))
    (if ok
        element-type
        (error 'file-error
               :format-control "Unsupported element type ~S."
               :format-arguments (list element-type)))))

(defun open (filename
	     &key
	     (direction :input)
	     (element-type 'character)
	     (if-exists nil if-exists-given)
	     (if-does-not-exist nil if-does-not-exist-given)
	     (external-format :default))
;  (declare (ignore external-format)) ; FIXME
  (setf element-type (case element-type
                       ((character base-char)
                        'character)
                       (:default
                        '(unsigned-byte 8))
                       (t
                        (upgraded-element-type element-type))))
  (let* ((pathname (merge-pathnames filename))
         (namestring (namestring (if (typep pathname 'logical-pathname)
                                     (translate-logical-pathname pathname)
                                     pathname))))
    (when (wild-pathname-p pathname)
      (error 'file-error
	     :pathname pathname
	     :format-control "Bad place for a wild pathname."))
    (when (memq direction '(:output :io))
      (unless if-exists-given
        (setf if-exists
              (if (eq (pathname-version pathname) :newest)
                  :new-version
                  :error))))
    (unless if-does-not-exist-given
      (setf if-does-not-exist
            (cond ((eq direction :input) :error)
                  ((and (memq direction '(:output :io))
                        (memq if-exists '(:overwrite :append)))
                   :error)
                  ((eq direction :probe)
                   nil)
                  (t
                   :create))))
    (case direction
      (:input
       (case if-does-not-exist
         (:error
          (unless (probe-file pathname)
            (error 'file-error
                   :pathname pathname
                   :format-control "The file ~S does not exist."
                   :format-arguments (list namestring)))))
       (make-file-stream pathname namestring element-type :input nil external-format))
      (:probe
       (case if-does-not-exist
         (:error
          (unless (probe-file pathname)
            (error 'file-error
                   :pathname pathname
                   :format-control "The file ~S does not exist."
                   :format-arguments (list namestring))))
         (:create
          ;; CREATE-NEW-FILE "atomically creates a new, empty file named by
          ;; this abstract pathname if and only if a file with this name does
          ;; not yet exist." See java.io.File.createNewFile().
          (create-new-file namestring)))
       (let ((stream (make-file-stream pathname namestring element-type
                                       :input nil external-format)))
         (when stream
           (close stream))
         stream))
      ((:output :io)
       (case if-does-not-exist
         (:error
          (unless (probe-file pathname)
            (error 'file-error
                   :pathname pathname
                   :format-control "The file ~S does not exist."
                   :format-arguments (list namestring))))
         ((nil)
          (unless (probe-file pathname)
            (return-from open nil))))
       (case if-exists
         (:error
          (when (probe-file pathname)
            (error 'file-error
                   :pathname pathname
                   :format-control "The file ~S already exists."
                   :format-arguments (list namestring))))
         ((nil)
          (when (probe-file pathname)
            (return-from open nil)))
         ((:rename :rename-and-delete)
          (when (probe-file pathname)
            ;; Make sure the original file is not a directory.
            (when (probe-directory pathname)
              (error 'file-error
                     :pathname pathname
                     :format-control "The file ~S is a directory."
                     :format-arguments (list namestring)))
            (let ((backup-name (concatenate 'string namestring ".bak")))
              (when (probe-file backup-name)
                (when (probe-directory backup-name)
                  (error 'file-error
                         :pathname pathname
                         :format-control "Unable to rename ~S."
                         :format-arguments (list namestring)))
                (delete-file backup-name))
              (rename-file pathname backup-name))))
         ((:new-version :supersede :overwrite :append)) ; OK to proceed.
         (t
          (error 'simple-error
                 :format-control "Option not supported: ~S."
                 :format-arguments (list if-exists))))
       (let ((stream (make-file-stream pathname namestring element-type
                                       direction if-exists external-format)))
         (unless stream
           (error 'file-error
                  :pathname pathname
                  :format-control "Unable to open ~S."
                  :format-arguments (list namestring)))
         stream))
      (t
       (error 'simple-error
              :format-control ":DIRECTION ~S not supported."
              :format-arguments (list direction))))))
