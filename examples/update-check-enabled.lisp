;;; update-check-enabled.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves
;;; $Id: update-check-enabled.lisp,v 1.2 2006-03-03 14:26:59 piso Exp $
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

(in-package "CL-USER")

;; In minutes.
(defparameter check-enabled-timeout 5)

;; Don't resolve autoloads in the background thread!
(sys::resolve 'get-internal-real-time)

(defun update-check-enabled ()
  (loop
    (sleep 60) ; 1 minute
    (let* ((last-event-time (get-last-event-internal-time))
           (current-time (get-internal-real-time))
           (timeout (* check-enabled-timeout 60 internal-time-units-per-second))
           (enable (if (> current-time (+ last-event-time timeout)) nil t)))
      (unless (eq (get-global-property 'check-enabled) enable)
        (set-global-property 'check-enabled enable)
        (log-debug "check-enabled => ~A" (get-global-property 'check-enabled))))))

;; Fire it up.
(make-thread #'update-check-enabled)
