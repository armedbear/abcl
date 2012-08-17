;;; socket.lisp
;;;
;;; Copyright (C) 2004-2006 Peter Graves
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

(in-package "EXTENSIONS")

(export '(make-socket make-server-socket server-socket-close socket-accept
          socket-close get-socket-stream socket-peer-port socket-local-port
          socket-local-address socket-peer-address))


(defun get-socket-stream (socket &key (element-type 'character) (external-format :default))
  ":ELEMENT-TYPE must be CHARACTER or (UNSIGNED-BYTE 8); the default is CHARACTER.
EXTERNAL-FORMAT must be of the same format as specified for OPEN."
  (cond ((eq element-type 'character))
        ((reduce #'equal
                (mapcar #'sys::normalize-type 
                        (list element-type '(unsigned-byte 8)))))
        (t
         (error 'simple-type-error
                :format-control
                ":ELEMENT-TYPE must be CHARACTER or (UNSIGNED-BYTE 8).")))
  (sys::%socket-stream socket element-type external-format))

(defun make-socket (host port)
  (sys::%make-socket host port))

(defun make-server-socket (port)
  (sys::%make-server-socket port))

(defun socket-accept (socket)
  (sys::%socket-accept socket))

(defun socket-close (socket)
  (sys::%socket-close socket))

(defun server-socket-close (socket)
  (sys::%server-socket-close socket))

(declaim (inline %socket-address %socket-port))
(defun %socket-address (socket addressName)
   (java:jcall "getHostAddress" (java:jcall-raw addressName socket)))

(defun %socket-port (socket portName)
   (java:jcall portName socket))

(defun socket-local-address (socket)
   "Returns the local address of the given socket as a dotted quad string."
   (%socket-address socket "getLocalAddress"))

(defun socket-peer-address (socket)
   "Returns the peer address of the given socket as a dotted quad string."
   (%socket-address socket "getInetAddress"))

(defun socket-local-port (socket)
   "Returns the local port number of the given socket."
   (%socket-port socket "getLocalPort"))

(defun socket-peer-port (socket)
   "Returns the peer port number of the given socket."
   (%socket-port socket "getPort"))

(provide '#:socket)
