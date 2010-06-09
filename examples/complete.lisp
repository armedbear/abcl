;;; complete.lisp
;;;
;;; Copyright (C) 2004 Peter Graves
;;; $Id: complete.lisp,v 1.2 2004-09-05 00:12:25 piso Exp $
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

(in-package "J")

(export 'complete)

(defvar *prefix* nil)
(defvar *completions* ())
(defvar *completion-index* 0)

(defun compound-prefix-match (prefix target)
  (let ((tlen (length target))
        (tpos 0))
    (dotimes (i (length prefix))
      (when (>= tpos tlen)
        (return-from compound-prefix-match nil))
      (let ((ch (schar prefix i)))
        (if (char= ch #\-)
            (unless (setf tpos (position #\- target :start tpos))
              (return-from compound-prefix-match nil))
            (unless (char-equal ch (schar target tpos))
              (return-from compound-prefix-match nil)))
        (incf tpos)))
    t))

(defun completion-set (prefix)
  (let ((result ()))
    (do-external-symbols (symbol "CL")
      (let ((name (symbol-name symbol)))
        (when (compound-prefix-match prefix name)
          (push symbol result))))
    result))

(defun completion-prefix ()
  (let* ((string (line-chars (current-line)))
         (end (mark-charpos (current-point))))
    (do ((start (1- end) (1- start)))
        ((< start 0) (subseq string 0 end))
      (let ((ch (schar string start)))
        (when (or (eql ch #\space) (eql ch #\())
          (incf start)
          (return-from completion-prefix (subseq string start end)))))))

(defun complete ()
  (cond ((eq *last-command* 'complete)
         (unless (> (length *completions*) 1)
           (return-from complete))
         (undo)
         (incf *completion-index*)
         (when (> *completion-index* (1- (length *completions*)))
           (setf *completion-index* 0)))
        (t
         (setf *prefix* (completion-prefix)
               *completions* nil
               *completion-index* 0)
         (when *prefix*
           (setf *completions* (completion-set *prefix*)))))
  (when *completions*
    (let ((completion (string-downcase (nth *completion-index* *completions*)))
          (point (current-point)))
      (with-single-undo
        (goto-char (make-mark (mark-line point)
                              (- (mark-charpos point) (length *prefix*))))
        (set-mark point)
        (delete-region)
        (insert completion)))
    (setf *current-command* 'complete))
  (values))

(map-key-for-mode "Ctrl Space" "(complete)" "Lisp")
(map-key-for-mode "Ctrl Space" "(complete)" "Lisp Shell")
