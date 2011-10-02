;;;; Copyright (C) 2011 Mark Evenson

(in-package #:threads)

(require 'abcl-contrib)
(eval-when (:compile-toplevel)
           (require 'jss))

(defparameter *server-running-p* nil)

;;; XXX possibly need multiple thread pools
(defparameter *thread-pool* nil
  "The current JVM class implementing the ScheduledThreadPool abstraction.")
(defparameter *scheduled-futures* nil)
(defparameter *incoming-scheduled-future* nil)
(defparameter *watch-queue-future* nil)


;;;; Configure the directories for a threadpool from these defaults.
(defparameter *root* #p"/var/tmp/abcl-threads/")

(defparameter *logs* (merge-pathnames "logs/" *root*))

(defparameter *incoming* (merge-pathnames "incoming/" *root*))
(defparameter *dirs* (list *incoming*))

(defparameter *queue* (merge-pathnames "queue/" *root*))

(defparameter *processed* (merge-pathnames "processed/" *root*))


;;;; A simple logging abstraction.

(defconstant +month-names+ '("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
                             "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defconstant +seconds+ (java:jfield "java.util.concurrent.TimeUnit" "SECONDS"))
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

(defun make-process-incoming ()
  (java:jinterface-implementation "java.lang.Runnable" "run" #'process-incoming))

(defun process-incoming ()
  (flet ((reject-input (file invalid) 
           (warn (format nil "~A is ~A" file invalid))))
  (let ((incoming (directory (merge-pathnames *incoming* "*"))))
    (unless incoming
      (return-from process-incoming))
    (log "Processing ~A incoming items." (length incoming))
    (let (table error)
      (dolist (file incoming)
        (setf error nil)
        (log "Analyzing ~A." file)
        (setf table
              (handler-case 
                  (load-table file)
                (t (e) 
                  (log "Failed to process ~A because ~A" file e)
                  (setf error e))))
        (if error 
            (reject-input file (if (listp error) error (list error)))
            (multiple-value-bind (valid invalid)
                (validate table)
              (if invalid 
                  (progn 
                    (log "Rejecting ~A because of invalid rows." file)
                    (reject-input file invalid))
                  (let ((incoming 
                         (make-pathname :defaults *queue* 
                                        :name (pathname-name file)
                                        :type (pathname-type file))))
                    (log "Inserting ~A." incoming)
                    (rename-file file incoming))))))))))

