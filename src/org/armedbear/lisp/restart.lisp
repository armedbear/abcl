;;; restart.lisp
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

;;; Adapted from CMUCL/SBCL.

(in-package #:system)

(defun read-evaluated-form ()
  (fresh-line *query-io*)
  (%format *query-io* "Enter a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defvar *restart-clusters* ())

(defvar *condition-restarts* ())

(defstruct restart
  name
  function
  report-function
  interactive-function
  (test-function #'(lambda (c) (declare (ignore c)) t)))

(defmacro restart-bind (bindings &body forms)
  `(let ((*restart-clusters*
          (cons (list
                 ,@(mapcar #'(lambda (binding)
                              `(make-restart
                                :name ',(car binding)
                                :function ,(cadr binding)
                                ,@(cddr binding)))
                           bindings))
                *restart-clusters*)))
     ,@forms))

(defun compute-restarts (&optional condition)
  (let ((associated ())
	(other ()))
    (dolist (alist *condition-restarts*)
      (if (eq (car alist) condition)
	  (setq associated (cdr alist))
	  (setq other (append (cdr alist) other))))
    (let ((res ()))
      (dolist (restart-cluster *restart-clusters*)
        (dolist (restart restart-cluster)
          (when (and (or (not condition)
                         (member restart associated)
                         (not (member restart other)))
                     (funcall (restart-test-function restart) condition))
            (push restart res))))
      (nreverse res))))

(defun restart-report (restart stream)
  (funcall (or (restart-report-function restart)
	       (let ((name (restart-name restart)))
		 (lambda (stream)
		   (if name (%format stream "~S" name)
		       (%format stream "~S" restart)))))
	   stream))

(defun print-restart (restart stream)
  (if *print-escape*
      (print-unreadable-object (restart stream :type t :identity t)
                               (prin1 (restart-name restart) stream))
      (restart-report restart stream)))

(defun find-restart (name &optional condition)
  (let ((restarts (compute-restarts condition)))
    (dolist (restart restarts)
      (when (or (eq restart name) (eq (restart-name restart) name))
        (return-from find-restart restart)))))

(defun find-restart-or-control-error (identifier &optional condition)
  (or (find-restart identifier condition)
      (error 'control-error
	     :format-control "Restart ~S is not active."
	     :format-arguments (list identifier))))

(defun invoke-restart (restart &rest values)
  (let ((real-restart (find-restart-or-control-error restart)))
    (apply (restart-function real-restart) values)))

(defun interactive-restart-arguments (real-restart)
  (let ((interactive-function (restart-interactive-function real-restart)))
    (if interactive-function
        (funcall interactive-function)
        '())))

(defun invoke-restart-interactively (restart)
  (let* ((real-restart (find-restart-or-control-error restart))
         (args (interactive-restart-arguments real-restart)))
    (apply (restart-function real-restart) args)))


(defun parse-keyword-pairs (list keys)
  (do ((l list (cddr l))
       (k '() (list* (cadr l) (car l) k)))
      ((or (null l) (not (member (car l) keys)))
       (values (nreverse k) l))))

(defmacro with-keyword-pairs ((names expression &optional keywords-var) &body forms)
  (let ((temp (member '&rest names)))
    (unless (= (length temp) 2)
      (error "&REST keyword is ~:[missing~;misplaced~]." temp))
    (let ((key-vars (ldiff names temp))
          (key-var (or keywords-var (gensym)))
          (rest-var (cadr temp)))
      (let ((keywords (mapcar #'(lambda (x) (intern (string x) (find-package "KEYWORD")))
                              key-vars)))
        `(multiple-value-bind (,key-var ,rest-var)
           (parse-keyword-pairs ,expression ',keywords)
           (let ,(mapcar #'(lambda (var keyword) `(,var (getf ,key-var ,keyword)))
                         key-vars keywords)
             ,@forms))))))

(defun transform-keywords (&key report interactive test)
  (let ((result ()))
    (when report
      (setf result (list* (if (stringp report)
                              `#'(lambda (stream)
                                  (write-string ,report stream))
                              `#',report)
                          :report-function
                          result)))
    (when interactive
      (setf result (list* `#',interactive
                          :interactive-function
                          result)))
    (when test
      (setf result (list* `#',test :test-function result)))
    (nreverse result)))


;; "If the restartable-form is a list whose car is any of the symbols SIGNAL,
;; ERROR, CERROR, or WARN (or is a macro form which macroexpands into such a
;; list), then WITH-CONDITION-RESTARTS is used implicitly to associate the
;; indicated restarts with the condition to be signaled."
(defun munge-restart-case-expression (expression env)
  (let ((exp (macroexpand expression env)))
    (if (consp exp)
	(let* ((name (car exp))
	       (args (if (eq name 'cerror) (cddr exp) (cdr exp))))
	  (if (member name '(SIGNAL ERROR CERROR WARN))
              (let ((n-cond (gensym)))
                `(let ((,n-cond (coerce-to-condition ,(first args)
                                                     (list ,@(rest args))
                                                     ',(case name
                                                         (WARN 'simple-warning)
                                                         (SIGNAL 'simple-condition)
                                                         (t 'simple-error))
                                                     ',name)))
                   (with-condition-restarts
                     ,n-cond
                     (car *restart-clusters*)
                     ,(if (eq name 'cerror)
                          `(cerror ,(second exp) ,n-cond)
                          `(,name ,n-cond)))))
              expression))
        expression)))

(defmacro restart-case (expression &body clauses &environment env)
  (let ((block-tag (gensym))
        (temp-var (gensym))
        (data
         (mapcar #'(lambda (clause)
                    (with-keyword-pairs ((report interactive test
                                                 &rest forms)
                                         (cddr clause))
                      (list (car clause)
                            (gensym)
                            (transform-keywords :report report
                                                :interactive interactive
                                                :test test)
                            (cadr clause)
                            forms)))
                 clauses)))
    `(block ,block-tag
            (let ((,temp-var nil))
              (tagbody
               (restart-bind
                ,(mapcar #'(lambda (datum)
                            (let ((name (nth 0 datum))
                                  (tag  (nth 1 datum))
                                  (keys (nth 2 datum)))
                              `(,name #'(lambda (&rest temp)
                                         (setq ,temp-var temp)
                                         (go ,tag))
                                      ,@keys)))
                         data)
                (return-from ,block-tag
                  ,(munge-restart-case-expression expression env)))
               ,@(mapcan #'(lambda (datum)
                            (let ((tag  (nth 1 datum))
                                  (bvl  (nth 3 datum))
                                  (body (nth 4 datum)))
                              (list tag
                                    `(return-from ,block-tag
                                                  (apply #'(lambda ,bvl ,@body)
                                                         ,temp-var)))))
                         data))))))

(defmacro with-simple-restart ((restart-name format-string
                                             &rest format-arguments)
                               &body forms)
  `(restart-case (progn ,@forms)
                 (,restart-name ()
                                :report (lambda (stream)
                                          (simple-format stream ,format-string ,@format-arguments))
                                (values nil t))))

(defmacro with-condition-restarts (condition-form restarts-form &body body)
  (let ((n-cond (gensym)))
    `(let ((*condition-restarts*
	    (cons (let ((,n-cond ,condition-form))
		    (cons ,n-cond
			  (append ,restarts-form
				  (cdr (assoc ,n-cond *condition-restarts*)))))
		  *condition-restarts*)))
       ,@body)))

(defun abort (&optional condition)
  (invoke-restart (find-restart-or-control-error 'abort condition))
  (error 'control-error
         :format-control "ABORT restart failed to transfer control dynamically."))

(defun muffle-warning (&optional condition)
  (invoke-restart (find-restart-or-control-error 'muffle-warning condition)))

(defun continue (&optional condition)
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

(defun store-value (value &optional condition)
  (let ((restart (find-restart 'store-value condition)))
    (when restart
      (invoke-restart restart value))))

(defun use-value (value &optional condition)
  (let ((restart (find-restart 'use-value condition)))
    (when restart
      (invoke-restart restart value))))

(defun warn (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-warning 'warn)))
    (require-type condition 'warning)
    (restart-case (signal condition)
      (muffle-warning ()
        :report "Skip warning."
        (return-from warn nil)))
    (let ((badness (etypecase condition
                     (style-warning 'style-warning)
                     (warning 'warning))))
      (fresh-line *error-output*)
      (simple-format *error-output* "~S: ~A~%" badness condition)))
  nil)

(defun style-warn (format-control &rest format-arguments)
  (warn 'style-warning
        :format-control format-control
        :format-arguments format-arguments))

(defun cerror (continue-string datum &rest arguments)
  (with-simple-restart (continue "~A" (apply #'simple-format nil continue-string arguments))
    (let ((condition (coerce-to-condition datum arguments 'simple-error 'error)))
      (with-condition-restarts condition (list (find-restart 'continue))
        (signal condition)
        (invoke-debugger condition))))
  nil)

(defun query-function ()
  (format *query-io* "~&Enter a form to be evaluated: ")
  (force-output *query-io*)
  (multiple-value-list (eval (read *query-io*))))

(defun undefined-function-called (name arguments)
  (finish-output)
  (loop
    (restart-case
        (error 'undefined-function :name name)
      (continue ()
        :report "Try again.")
      (use-value (value)
        :report "Specify a function to call instead."
        :interactive query-function
        (return-from undefined-function-called
                     (apply value arguments)))
      (return-value (&rest values)
        :report (lambda (stream)
                  (format stream "Return one or more values from the call to ~S." name))
        :interactive query-function
        (return-from undefined-function-called
                     (values-list values))))
    (when (fboundp name)
      (return (apply name arguments)))))
