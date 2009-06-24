;;; config.lisp
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

(defparameter *launch-swank-at-startup* nil)

(defparameter *swank-dir* nil)

(defparameter *swank-port* 4005)

(defparameter *use-throwing-debugger* t)

(defun configure-abcl (abcl-script-engine)
  (when *use-throwing-debugger*
    (setf *debugger-hook* #'sys::%debugger-hook-function))
  (when *launch-swank-at-startup*
    (unless *swank-dir*
      (error "Swank directory not specified, please set *swank-dir*"))
    (pushnew *swank-dir* asdf:*central-registry* :test #'equal)
    (asdf:oos 'asdf:load-op :swank)
    (ext:make-thread (lambda () (funcall (find-symbol
					  (symbol-name '#:create-server)
					  :swank)
					 :port *swank-port*))
		     :name "ABCL script - Swank thread")))
