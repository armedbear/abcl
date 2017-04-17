;;; time.lisp
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

(defconstant seconds-in-week (* 60 60 24 7))
(defconstant weeks-offset 2145)
(defconstant seconds-offset 432000)
(defconstant minutes-per-day (* 24 60))
(defconstant quarter-days-per-year (1+ (* 365 4)))
(defconstant quarter-days-per-century 146097)
(defconstant november-17-1858 678882)
(defconstant weekday-november-17-1858 2)

;;; decode-universal-time universal-time &optional time-zone
;;; => second minute hour date month year day daylight-p zone
;;; If time-zone is not supplied, it defaults to the current time zone adjusted
;;; for daylight saving time. If time-zone is supplied, daylight saving time
;;; information is ignored. The daylight saving time flag is nil if time-zone
;;; is supplied.
(defun decode-universal-time (universal-time &optional time-zone)
  (let (seconds-west daylight)
    (if time-zone
        (setf seconds-west (* time-zone 3600)
              daylight nil)
        (multiple-value-bind (time-zone daylight-p) (ext:get-time-zone universal-time)
          (setf seconds-west (* time-zone 3600)
                daylight daylight-p)))
    (multiple-value-bind (weeks secs)
        (truncate (+ (- universal-time seconds-west) seconds-offset)
                  seconds-in-week)
      (let ((weeks (+ weeks weeks-offset)))
        (multiple-value-bind (t1 second)
            (truncate secs 60)
          (let ((tday (truncate t1 minutes-per-day)))
            (multiple-value-bind (hour minute)
                (truncate (- t1 (* tday minutes-per-day)) 60)
              (let* ((t2 (1- (* (+ (* weeks 7) tday november-17-1858) 4)))
                     (tcent (truncate t2 quarter-days-per-century)))
                (setq t2 (mod t2 quarter-days-per-century))
                (setq t2 (+ (- t2 (mod t2 4)) 3))
                (let* ((year (+ (* tcent 100)
                                (truncate t2 quarter-days-per-year)))
                       (days-since-mar0
                        (1+ (truncate (mod t2 quarter-days-per-year) 4)))
                       (day (mod (+ tday weekday-november-17-1858) 7))
                       (t3 (+ (* days-since-mar0 5) 456)))
                  (cond ((>= t3 1989)
                         (setq t3 (- t3 1836))
                         (setq year (1+ year))))
                  (multiple-value-bind (month t3)
                      (truncate t3 153)
                    (let ((date (1+ (truncate t3 5))))
                      (values second minute hour date month year day
                              daylight
                              (if daylight
                                  (1+ (/ seconds-west 3600))
                                  (/ seconds-west 3600))))))))))))))

(defun get-decoded-time ()
  (decode-universal-time (get-universal-time)))

(defun pick-obvious-year (year)
  (declare (type (mod 100) year))
  (let* ((current-year (nth-value 5 (get-decoded-time)))
	 (guess (+ year (* (truncate (- current-year 50) 100) 100))))
    (declare (type (integer 1900 9999) current-year guess))
    (if (> (- current-year guess) 50)
	(+ guess 100)
	guess)))

(defun leap-years-before (year)
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
	  (truncate years 100))
       (truncate (+ years 300) 400))))

(defvar *days-before-month*
  #.(let ((reversed-result nil)
	  (sum 0))
      (push nil reversed-result)
      (dolist (days-in-month '(31 28 31 30 31 30 31 31 30 31 30 31))
	(push sum reversed-result)
	(incf sum days-in-month))
      (coerce (nreverse reversed-result) 'simple-vector)))

(defun encode-universal-time (second minute hour date month year
				     &optional time-zone)
  (let* ((year (if (< year 100)
		   (pick-obvious-year year)
		   year))
	 (days (+ (1- date)
		  (aref *days-before-month* month)
		  (if (> month 2)
		      (leap-years-before (1+ year))
		      (leap-years-before year))
		  (* (- year 1900) 365)))
	 (hours (+ hour (* days 24))))
    (cond (time-zone
           (+ second (* (+ minute (* (+ hours time-zone) 60)) 60)))
          ((> year 2037)
           (labels ((leap-year-p (year)
                      (cond ((zerop (mod year 400)) t)
                            ((zerop (mod year 100)) nil)
                            ((zerop (mod year 4)) t)
                            (t nil))))
             (let* ((fake-year (if (leap-year-p year) 2036 2037))
                    (fake-time (encode-universal-time second minute hour
                                                      date month fake-year)))
               (+ fake-time
                 (* 86400 (+ (* 365 (- year fake-year))
                             (- (leap-years-before year)
                                (leap-years-before fake-year))))))))
          (t
           (let* ((tz-guess (ext:get-time-zone (* hours 3600)))
		  (guess (+ second (* 60 (+ minute (* 60 (+ hours tz-guess))))))
		  (tz (get-time-zone guess)))
	     (+ guess (* 3600 (- tz tz-guess))))))))
