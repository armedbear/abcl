;;; fdefinition.lisp
;;;
;;; Copyright (C) 2005-2006 Peter Graves
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

(in-package #:system)

(export '(record-source-information untraced-function))

(defun check-redefinition (name)
  (when (and *warn-on-redefinition* (fboundp name) (not (autoloadp name)))
    (when (and (symbolp name)
               (source-pathname name))
      ;; SOURCE-PATHNAME is badly named as it is either a PATHNAMAE
      ;; or the symbol :TOP-LEVEL
      (let ((old-source 
             (if (keywordp (source-pathname name))
                 (source-pathname name)
                 (probe-file (source-pathname name))))
            (current-source 
             (if (not *source*) 
                 :top-level
                 (probe-file *source*))))
        (cond ((equal old-source 
                      current-source)) ; OK
              (t
               (if (eq current-source :top-level)
                   (style-warn "redefining ~S at top level" name)
                   (let ((*package* +cl-package+))
                     (if (eq old-source :top-level)
                         (style-warn "redefining ~S in ~S (previously defined at top level)"
                                     name current-source)
                         (style-warn "redefining ~S in ~S (previously defined in ~S)"
                                     name current-source old-source))))))))))

;;; DEPRECATED:  to be removed in abcl-1.7
(defun record-source-information (name &optional source-pathname source-position)
  (unless source-pathname
    (setf source-pathname (or *source* :top-level)))
  (unless source-position
    (setf source-position *source-position*))
  (let ((source (if source-position
                    (cons source-pathname source-position)
                    source-pathname)))
    (cond ((symbolp name)
           (put name '%source source)))))

(defun record-source-information-for-type (name type &optional source-pathname source-position)
  "Record source information on the SYS:SOURCE property for symbol with NAME

TYPE is either a symbol or list.

Source information for functions, methods, and generic functions are
represented as lists of the following form:

    (:generic-function function-name)
    (:function function-name)
    (:method method-name qualifiers specializers)

Where FUNCTION-NAME or METHOD-NAME can be a either be of the form
'symbol or '(setf symbol).

Source information for all other forms have a symbol for TYPE which is
one of the following:

:class, :variable, :condition, :constant, :compiler-macro, :macro
:package, :structure, :type, :setf-expander, :source-transform

These values follow SBCL'S implemenation in SLIME
c.f. <https://github.com/slime/slime/blob/bad2acf672c33b913aabc1a7facb9c3c16a4afe9/swank/sbcl.lisp#L748>

"
#|
Modifications are in two places, one at the definitions, calling
record-source-information-by-type and then again in the file-compiler,
which writes forms like

(put 'source name (cons (list type pathname position) (get 'source name)))

In theory this can lead to redundancy if a fasl is loaded again and
again. I'm not sure how to fix this yet. Forms in the __loader__ get
called early in build when many of the sequence functions aren't
present.  Will probably just filter when presenting in slime.

|#  
    (unless source-pathname
      (setf source-pathname (or *source* :top-level)))
    (unless source-position
      (setf source-position *source-position*))
    (let ((source (if source-position
		      (list source-pathname source-position)
		      (list source-pathname))))
      (let ((sym (if (consp name) (second name) name))
            (new `(,type ,(if (symbolp (car source)) (car source) (namestring (car source))) ,(second source))))
        (if (autoloadp 'delete)
 	 (put sym 'sys::source (cons new (get sym  'sys::source nil)))
         (put sym 'sys::source (cons new (delete new (get sym  'sys::source nil) 
               :test (lambda(a b) (and (equalp (car a) (car b)) (equalp (second a) (second b) ))))))))))

;; Redefined in trace.lisp.
(defun trace-redefined-update (&rest args)
  (declare (ignore args)))

;; Redefined in trace.lisp.
(defun untraced-function (name)
  (declare (ignore name))
  nil)

(%defvar '*fset-hooks* nil)

(defun fset (name function &optional source-position arglist documentation)
  (cond ((symbolp name)
         (check-redefinition name)
         (record-source-information name nil source-position)
         (when arglist
           (%set-arglist function arglist))
         (%set-documentation function 'function documentation)
         (%set-symbol-function name function))
        ((setf-function-name-p name)
         (check-redefinition name)
         (record-source-information name nil source-position)
         ;; FIXME arglist documentation
         (setf (get (%cadr name) 'setf-function) function))
        (t
         (require-type name '(or symbol (cons (eql setf) (cons symbol null))))))
  (when (functionp function) ; FIXME Is this test needed?
    (%set-lambda-name function name))
  (dolist (hook *fset-hooks*) (ignore-errors (funcall hook name function)))
  (trace-redefined-update name function)
  function)

(defun fdefinition (name)
  (cond ((symbolp name)
         (symbol-function name))
        ((setf-function-name-p name)
         (or (get (%cadr name) 'setf-function)
             (error 'undefined-function :name name)))
        (t
         (require-type name '(or symbol (cons (eql setf) (cons symbol null)))))))

(defun %set-fdefinition (name function)
  (fset name function))

(defsetf fdefinition %set-fdefinition)
