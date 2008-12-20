;;; boole.lisp
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

(defun boole (op n1 n2)
  (unless (and (integerp n1) (integerp n2))
    (error 'type-error
           :datum (if (integerp n1) n2 n1)
           :expected-type 'integer))
  (case op
    (#.boole-clr 0)
    (#.boole-set -1)
    (#.boole-1 n1)
    (#.boole-2 n2)
    (#.boole-c1 (lognot n1))
    (#.boole-c2 (lognot n2))
    (#.boole-and (logand n1 n2))
    (#.boole-ior (logior n1 n2))
    (#.boole-xor (logxor n1 n2))
    (#.boole-eqv (logeqv n1 n2))
    (#.boole-nand (lognand n1 n2))
    (#.boole-nor (lognor n1 n2))
    (#.boole-andc1 (logandc1 n1 n2))
    (#.boole-andc2 (logandc2 n1 n2))
    (#.boole-orc1 (logorc1 n1 n2))
    (#.boole-orc2 (logorc2 n1 n2))
    (t
     (error 'type-error
            :datum op
            :expected-type (list 'integer #.boole-clr #.boole-orc2)))))
