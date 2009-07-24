;;; threads.lisp
;;;
;;; Copyright (C) 2009 Erik Huelsmann <ehuelsmann@common-lisp.net>
;;;
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

(in-package #:threads)


;;
;; Mailbox implementation
;;

;; this export statement is also in autoloads.lisp
(export '(mailbox-send mailbox-empty-p mailbox-read mailbox-peek))

(defstruct mailbox
  queue)

(defun mailbox-send (mailbox item)
  "Sends an item into the mailbox, notifying 1 waiter
to wake up for retrieval of that object."
  (threads:synchronized-on mailbox
     (push (mailbox-queue mailbox) item)
     (threads:object-notify mailbox)))

(defun mailbox-empty-p (mailbox)
  "Returns non-NIL if the mailbox can be read from, NIL otherwise."
  ;; Because we're just checking the value of an object reference,
  ;; (which are atomically gotten and set) we don't need to lock
  ;; the mailbox before operating on it.
  (null (mailbox-queue mailbox)))

(defun mailbox-read (mailbox)
  "Blocks on the mailbox until an item is available for reading.
When an item is available, it is returned."
  (threads:synchronized-on mailbox
     (loop
        (unless (mailbox-empty-p mailbox)
          (return))
        (object-wait mailbox))
     (pop (mailbox-queue mailbox))))

(defun mailbox-peek (mailbox)
  "Returns two values. The second returns non-NIL when the mailbox
is empty. The first is the next item to be read from the mailbox
if the first is NIL.

Note that due to multi-threading, the first value returned upon
peek, may be different from the one returned upon next read in the
calling thread."
  (threads:synchronized-on mailbox
     (values (car (mailbox-queue mailbox))
             (null (mailbox-queue mailbox)))))



;;
;; Lock implementation
;;

(defun make-thread-lock ()
  "Returns an object to be used with the `with-thread-lock' macro."
  (gensym))

(defmacro with-thread-lock ((lock) &body body)
  (let ((glock (gensym)))
    `(let ((,glock ,lock))
       (synchronized-on ,glock
          ,@body))))

(defun thread-lock (lock)
  "Deprecated; due for removal in 0.22"
  (declare (ignore lock)))
(defun thread-unlock (lock)
  "Deprecated; due for removal in 0.22"
  (declare (ignore lock)))
