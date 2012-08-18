;;; autoloads.lisp
;;;
;;; Copyright (C) 2003-2008 Peter Graves
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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


;; ABOUT THIS FILE

;; In order to avoid loading the full CL system (of which not all functions
;; may be required by the current program), this file makes sure symbols
;; of public functions have their function slots bound to a proxy function
;; which loads the actual functions or macros on invocation.

;; There are two autoloader files: autoload-gen.lisp, which is automatically
;; generated based on the source files, and this file, which is manually
;; maintained for any symbols that can't be automatically detected.




(in-package "SYSTEM")


;; This one must be last, or at least past print-object and clos:
;; we don't want FORMATs executed before we can load those to end us
;; in a debugger. This command replaces the earlier function binding
;; where simple-format calls sys::%format

(autoload 'simple-format "format")