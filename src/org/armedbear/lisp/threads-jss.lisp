;;;; Copyright (C) 2011 Mark Evenson

(in-package #:threads)

(require 'abcl-contrib)
(eval-when (:compile-toplevel)
           (require 'jss))

(defparameter *server-running-p* nil)

;;; XXX possibly need multiple thread pools
(defparameter *thread-pool* nil)
(defparameter *scheduled-futures* nil)
(defparameter *incoming-scheduled-future* nil)
(defparameter *watch-queue-future* nil)


;;;; Configure the directories for a threadpool from these defaults.
(defparameter *root* #p"/var/tmp/abcl-threads/")

(defparameter *logs* (merge-pathnames "logs/" *root*))

(defparameter *incoming* (merge-pathnames "incoming/" *root*))
(defparameter *dirs* (list *incoming*))

;;;; A simple logging abstraction.

(defconstant +month-names+ '("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
                             "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defparameter *log* *standard-output*)

(defun format-time (universal-time)
    (multiple-value-bind 
          (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time universal-time)
      (format nil "~&~A ~A ~2,'0D:~2,'0D:~2,'0D" 
              (nth (1- month) +month-names+) date hour minute second)))

(defmacro log (message &rest parameters)
  `(when *log*
     (format *log* "~A " (format-time (get-universal-time)))
     (format *log* ,message ,@parameters)
     (format *log* "~&")))

;;; Start a pool of hungry philosophers.
(defun start-server () 
  (when *server-running-p*
    (error "Server not recorded as stopped."))
  (unless 
      (mapcar #'ensure-directories-exist *dirs*)
    (error "Failed to create directories under '~A'." *root*))
  (let ((logfile (merge-pathnames "abcl-threads.log" *logs*)))
    (setf *log* 
          (open logfile :direction :output :if-exists :append :if-does-not-exist :create))
    (format *standard-output* "Logging to ~A." logfile))
  (log "Starts.")
  (schedule-threads)
  (setf *server-running-p* t))

(defun stop-server (&key (force nil))
  (unless force
    (unless *server-running-p*
      (error "Server not recorded as running.")))
  (log "Stopping the server.")
  (dolist (future `(,*incoming-scheduled-future* ,*watch-queue-future* ,@*scheduled-futures*))
    (when (not (or (#"isCancelled" future)
                   (#"isDone" future)))
      (#"cancel" future t)))
  (#"shutdown" *thread-pool*)
  (close *log*)
  (setf *server-running-p* nil))

(defun schedule-threads ()
  (log "Starting thread pool.")
  (when *thread-pool*
    (log "Removing existing incoming thread pool."))
  (setf *thread-pool*
        (#"newScheduledThreadPool" 'java.util.concurrent.Executors 1))
  (#"setExecuteExistingDelayedTasksAfterShutdownPolicy" *thread-pool* nil)
  (initialize-queue)
  (log "Scheduling queue watcher.")
  (setf *watch-queue-future* 
        (#"scheduleWithFixedDelay" 
         *thread-pool*
         (make-watch-queue) 10 1 +seconds+))
  (log "Scheduling incoming watcher.")
  (setf *incoming-scheduled-future*
        (#"scheduleWithFixedDelay" 
         *thread-pool*
         (make-process-incoming) 1 1 +seconds+)))

