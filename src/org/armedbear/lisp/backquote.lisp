;;; backquote.lisp
;;;
;;; Copyright (C) 2004-2005 Peter Graves
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

;;;; the backquote reader macro

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package #:system)

;;; The flags passed back by BACKQUOTIFY can be interpreted as follows:
;;;
;;;   |`,|: [a] => a
;;;    NIL: [a] => a		;the NIL flag is used only when a is NIL
;;;      T: [a] => a		;the T flag is used when a is self-evaluating
;;;  QUOTE: [a] => (QUOTE a)
;;; APPEND: [a] => (APPEND . a)
;;;  NCONC: [a] => (NCONC . a)
;;;   LIST: [a] => (LIST . a)
;;;  LIST*: [a] => (LIST* . a)
;;;
;;; The flags are combined according to the following set of rules:
;;;  ([a] means that a should be converted according to the previous table)
;;;
;;;   \ car  ||    otherwise    |    QUOTE or     |     |`,@|      |     |`,.|
;;;cdr \     ||                 |    T or NIL     |                |
;;;================================================================================
;;;  |`,|    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a [d])
;;;  NIL     || LIST    ([a])   | QUOTE    (a)    | <hair>    a    | <hair>    a
;;;QUOTE or T|| LIST* ([a] [d]) | QUOTE  (a . d)  | APPEND (a [d]) | NCONC (a [d])
;;; APPEND   || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a . d) | NCONC (a [d])
;;; NCONC    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC (a . d)
;;;  LIST    || LIST  ([a] . d) | LIST  ([a] . d) | APPEND (a [d]) | NCONC (a [d])
;;;  LIST*   || LIST* ([a] . d) | LIST* ([a] . d) | APPEND (a [d]) | NCONC  (a [d])
;;;
;;;<hair> involves starting over again pretending you had read ".,a)" instead
;;; of ",@a)"

;; (%defvar '*backquote-count* 0) ; defined in Java, q.v. Lisp.java:2754
(%defvar '*bq-comma-flag* '(|,|))
(%defvar '*bq-at-flag* '(|,@|))
(%defvar '*bq-dot-flag* '(|,.|))
;; (%defvar '*bq-vector-flag* '(|bqv|)) ; defined in Java, q.v. Lisp.java:2757

;;; the actual character macro
(defun backquote-macro (stream ignore)
  (declare (ignore ignore))
  (let ((*backquote-count* (1+ *backquote-count*)))
    (multiple-value-bind (flag thing)
	(backquotify stream (read stream t nil t))
      (when (eq flag *bq-at-flag*)
	(%reader-error stream ",@ after backquote in ~S" thing))
      (when (eq flag *bq-dot-flag*)
	(%reader-error stream ",. after backquote in ~S" thing))
      (backquotify-1 flag thing))))

(defun comma-macro (stream ignore)
  (declare (ignore ignore))
  (unless (> *backquote-count* 0)
    (when *read-suppress*
      (return-from comma-macro nil))
    (%reader-error stream "Comma not inside a backquote."))
  (let ((c (read-char stream))
	(*backquote-count* (1- *backquote-count*)))
    (cond ((char= c #\@)
	   (cons *bq-at-flag* (read stream t nil t)))
	  ((char= c #\.)
	   (cons *bq-dot-flag* (read stream t nil t)))
	  (t (unread-char c stream)
	     (cons *bq-comma-flag* (read stream t nil t))))))

;;;
(defun expandable-backq-expression-p (object)
  (and (consp object)
       (let ((flag (%car object)))
         (or (eq flag *bq-at-flag*)
             (eq flag *bq-dot-flag*)))))

;;; This does the expansion from table 2.
(defun backquotify (stream code)
  (cond ((atom code)
	 (cond ((null code) (values nil nil))
	       ((or (consp code)
                    (symbolp code))
		;; Keywords are self-evaluating. Install after packages.
                (values 'quote code))
	       (t (values t code))))
	((or (eq (car code) *bq-at-flag*)
	     (eq (car code) *bq-dot-flag*))
	 (values (car code) (cdr code)))
	((eq (car code) *bq-comma-flag*)
	 (comma (cdr code)))
	((eq (car code) *bq-vector-flag*)
	 (multiple-value-bind (dflag d) (backquotify stream (cdr code))
	   (values 'vector (backquotify-1 dflag d))))
	(t (multiple-value-bind (aflag a) (backquotify stream (car code))
	     (multiple-value-bind (dflag d) (backquotify stream (cdr code))
	       (when (eq dflag *bq-at-flag*)
		 ;; Get the errors later.
		 (%reader-error stream ",@ after dot in ~S" code))
	       (when (eq dflag *bq-dot-flag*)
		 (%reader-error stream ",. after dot in ~S" code))
	       (cond
		((eq aflag *bq-at-flag*)
		 (if (null dflag)
		     (if (expandable-backq-expression-p a)
                         (values 'append (list a))
                         (comma a))
		     (values 'append
			     (cond ((eq dflag 'append)
				    (cons a d ))
				   (t (list a (backquotify-1 dflag d)))))))
		((eq aflag *bq-dot-flag*)
		 (if (null dflag)
		     (if (expandable-backq-expression-p a)
                         (values 'nconc (list a))
                         (comma a))
		     (values 'nconc
			     (cond ((eq dflag 'nconc)
				    (cons a d))
				   (t (list a (backquotify-1 dflag d)))))))
		((null dflag)
		 (if (memq aflag '(quote t nil))
		     (values 'quote (list a))
		     (values 'list (list (backquotify-1 aflag a)))))
		((memq dflag '(quote t))
		 (if (memq aflag '(quote t nil))
		     (values 'quote (cons a d ))
		     (values 'list* (list (backquotify-1 aflag a)
					  (backquotify-1 dflag d)))))
		(t (setq a (backquotify-1 aflag a))
		   (if (memq dflag '(list list*))
		       (values dflag (cons a d))
		       (values 'list*
			       (list a (backquotify-1 dflag d)))))))))))

;;; This handles the <hair> cases.
(defun comma (code)
  (cond ((atom code)
	 (cond ((null code)
		(values nil nil))
	       ((or (numberp code) (eq code t))
		(values t code))
	       (t (values *bq-comma-flag* code))))
	((and (eq (car code) 'quote)
              (not (expandable-backq-expression-p (cadr code))))
         (values (car code) (cadr code)))
	((memq (car code) '(append list list* nconc))
	 (values (car code) (cdr code)))
	((eq (car code) 'cons)
	 (values 'list* (cdr code)))
	(t (values *bq-comma-flag* code))))

;;; This handles table 1.
(defun backquotify-1 (flag thing)
  (cond ((or (eq flag *bq-comma-flag*)
	     (memq flag '(t nil)))
	 thing)
	((eq flag 'quote)
	 (list  'quote thing))
	((eq flag 'list*)
         (cond ((and (null (cddr thing))
                     (not (expandable-backq-expression-p (cadr thing))))
		(cons 'backq-cons thing))
	       ((expandable-backq-expression-p (car (last thing)))
                (list 'backq-append
                      (cons 'backq-list (butlast thing))
                      ;; Can it be optimized further? -- APD, 2001-12-21
                      (car (last thing))))
               (t
		(cons 'backq-list* thing))))
	((eq flag 'vector)
	 (list 'backq-vector thing))
	(t (cons (ecase flag
		   ((list) 'backq-list)
		   ((append) 'backq-append)
		   ((nconc) 'backq-nconc))
		 thing))))

;;;; magic BACKQ- versions of builtin functions

;;; Define synonyms for the lisp functions we use, so that by using
;;; them, the backquoted material will be recognizable to the
;;; pretty-printer.
(defun backq-list   (&rest args) (apply #'list   args))
(defun backq-list*  (&rest args) (apply #'list*  args))
(defun backq-append (&rest args) (apply #'append args))
(defun backq-nconc  (&rest args) (apply #'nconc  args))
(defun backq-cons   (&rest args) (apply #'cons   args))

(defun backq-vector (list)
  (declare (list list))
  (coerce list 'simple-vector))

;;; The pretty-printer needs to know about our special tokens
(%defvar '*backq-tokens*
  '(backq-comma backq-comma-at backq-comma-dot backq-list
    backq-list* backq-append backq-nconc backq-cons backq-vector))

(defun %reader-error (stream control &rest args)
  (error 'reader-error
	 :stream stream
	 :format-control control
	 :format-arguments args))
