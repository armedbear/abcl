;;; abcl-script.lisp
;;;
;;; Copyright (C) 2008 Alessio Stalla
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

(in-package :abcl-script)

(defconstant +global-scope+
  (jfield "javax.script.ScriptContext" "GLOBAL_SCOPE"))

(defconstant +engine-scope+
  (jfield "javax.script.ScriptContext" "ENGINE_SCOPE"))

(defconstant +put-binding+ (jmethod "javax.script.Bindings"
				    "put"
				    "java.lang.String"
				    "java.lang.Object"))

(defconstant +get-bindings+ (jmethod "javax.script.ScriptContext"
				     "getBindings"
				     "int"))

(defun generate-bindings (bindings)
  (let ((*package* (find-package :abcl-script-user)))
    (mapcar (lambda (binding) (list (read-from-string (car binding))
				    (cdr binding)))
	    bindings)))

(defun generate-special-declarations (bindings)
  (let ((*package* (find-package :abcl-script-user)))
    `(declare (special
	       ,@(mapcar (lambda (binding) (read-from-string (car binding)))
			 bindings)))))

(defun generate-java-bindings (bindings-list actual-bindings java-bindings)
  (loop :for binding  :in actual-bindings
	:for jbinding :in bindings-list
	:collect `(jcall +put-binding+
		   ,java-bindings ,(car jbinding) ,(car binding))))

(defmacro eval-in-script-context ((global-bindings engine-bindings stdin stdout script-context)
				  body)
  "Sets up a context in which to evaluate a piece of code coming from Java through the JSR-223 methods."
  (let ((actual-global-bindings (gensym))
	(actual-engine-bindings (gensym)))
    `(let ((*package* (find-package :abcl-script-user))
	   (*standard-input* ,stdin)
	   (*standard-output* ,stdout)
	   (,actual-global-bindings (generate-bindings ,global-bindings))
	   (,actual-engine-bindings (generate-bindings ,engine-bindings)))
       (eval `(let (,@,actual-global-bindings)
		(let (,@,actual-engine-bindings)
		  ,(generate-special-declarations ,global-bindings)
		  ,(generate-special-declarations ,engine-bindings)
		  (prog1
		      (progn ,@,body)
		    (finish-output *standard-output*)
		    ,@(generate-java-bindings
		       ,global-bindings 
		       ,actual-global-bindings
		       (jcall +get-bindings+ ,script-context +global-scope+))
		    ,@(generate-java-bindings
		       ,engine-bindings 
		       ,actual-engine-bindings
		       (jcall +get-bindings+ ,script-context +engine-scope+)))))))))
  
(defun eval-function (global-bindings engine-bindings stdin stdout function-and-args script-context)
  (eval-in-script-context (global-bindings engine-bindings stdin stdout script-context)
    `((funcall ,@(mapcar (lambda (arg) `(quote ,arg))
			 function-and-args)))))

(defun eval-script (global-bindings engine-bindings stdin stdout
		    code-string script-context)
  (eval-in-script-context (global-bindings engine-bindings stdin stdout script-context)
    `((with-input-from-string (str ,code-string)
	(sys::load-returning-last-result str)))))

(defun eval-compiled-script (global-bindings engine-bindings stdin stdout
			     function script-context)
  (eval-in-script-context (global-bindings engine-bindings stdin stdout script-context)
    `((funcall ,function))))

(defun compile-script (code-string)
  (let* ((tmp-file (jstatic (jmethod "java.io.File" "createTempFile" "java.lang.String" "java.lang.String")
			    nil "abcl-src-file-" ".lisp"))
	 (tmp-file-path (jcall (jmethod "java.io.File" "getAbsolutePath") tmp-file)))
    (jcall (jmethod "java.io.File" "deleteOnExit") tmp-file) ;to be really-really-really sure...
    (unwind-protect
	 (progn
	   (with-open-file (stream tmp-file-path :direction :output)
	     (princ "(in-package :abcl-script-user)" stream)
	     (princ code-string stream)
	     (finish-output stream))
	   (let ((compiled-file (compile-file tmp-file-path)))
	     (jcall (jmethod "java.io.File" "deleteOnExit")
		    (jnew (jconstructor "java.io.File" "java.lang.String")
			  (namestring compiled-file)))
	     (lambda ()
	       (let ((*package* (find-package :abcl-script-user)))
		 (sys::load-returning-last-result compiled-file)))))
      (delete-file tmp-file-path))))

;;Java interface implementation - TODO

(defvar *interface-implementation-map* (make-hash-table :test #'equal))

(defun find-java-interface-implementation (interface)
  (gethash interface *interface-implementation-map*))

(defun register-java-interface-implementation (interface implementation &optional lisp-this)
  (setf (gethash interface *interface-implementation-map*)
	(jmake-proxy interface implementation lisp-this)))

(defun remove-java-interface-implementation (interface)
  (remhash interface *interface-implementation-map*))

;Let's load it so asdf package is already defined when loading config.lisp
(require 'asdf)