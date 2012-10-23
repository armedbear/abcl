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
          socket-local-address socket-peer-address
          read-timeout write-timeout))


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
  "Create a TCP socket for client communication to HOST on PORT."
  (sys::%make-socket host port))

(defun make-server-socket (port)
  "Create a TCP server socket listening for clients on PORT."
  (sys::%make-server-socket port))

(defun socket-accept (socket)
  "Block until able to return a new socket for handling a incoming request to the specified server SOCKET."
  (sys::%socket-accept socket))

(defun socket-close (socket)
  "Close the client SOCKET."
  (sys::%socket-close socket))

(defun server-socket-close (socket)
  "Close the server SOCKET."
  (sys::%server-socket-close socket))

(declaim (inline %socket-address %socket-port))
(defun %socket-address (socket address-name)
  "Return the underlying ADDRESS-NAME for SOCKET."
   (java:jcall "getHostAddress" (java:jcall-raw address-name socket)))

(defun %socket-port (socket port-name)
  "Return the PORT-NAME of SOCKET."
   (java:jcall port-name socket))

(defun socket-local-address (socket)
   "Returns the local address of the SOCKET as a dotted quad string."
   (%socket-address socket "getLocalAddress"))

(defun socket-peer-address (socket)
   "Returns the peer address of the SOCKET as a dotted quad string."
   (%socket-address socket "getInetAddress"))

(defun socket-local-port (socket)
   "Returns the local port number of the SOCKET."
   (%socket-port socket "getLocalPort"))

(defun socket-peer-port (socket)
   "Returns the peer port number of the given SOCKET."
   (%socket-port socket "getPort"))

(defun read-timeout (socket seconds)
  "Time in SECONDS to set local implementation of 'SO_RCVTIMEO' on SOCKET."
  (java:jcall (java:jmethod "java.net.Socket" "setSoTimeout"  "int")
              socket
              (/ seconds 1000)))

(defun write-timeout (socket seconds)
  "No-op setting of write timeout to SECONDS on SOCKET."
  (declare (ignore socket seconds))
  (warn "Unimplemented.

Timeouts for writes should be implemented by spawning a guardian
to the thread perfoming the socket write"))

(provide '#:socket)
