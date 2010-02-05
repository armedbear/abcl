;;; package.lisp
;;;
;;; Copyright (C) 2008 Erik Huelsmann
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

(in-package "SYSTEM")

;; Redefines make-package from boot.lisp

(defun make-package (name &key nicknames use)
  (restart-case
      (progn
        (when (find-package name)
          (error 'simple-error "Package ~A already exists." name))
        (dolist (nick nicknames)
          (when (find-package nick)
            (error 'package-error :package nick)))
        (%make-package name nicknames use))
    (use-existing-package ()
      :report "Use existing package"
      (return-from make-package (find-package name)))))

;; Redefines function from defpackage.lisp, because there it's lacking restart-case

(defun ensure-available-symbols (imports)
  (remove nil
          (mapcar #'(lambda (package-and-symbols)
                      (let* ((package (find-package (designated-package-name (car package-and-symbols))))
                             (new-symbols
                              (remove nil
                                      (mapcar #'(lambda (sym)
                                                  (restart-case
                                                      (progn
                                                        (unless (find-symbol sym package)
                                                          (error 'package-error
                                                                 "The symbol ~A is not present in package ~A." sym (package-name package)))
                                                        sym)
                                                    (skip ()
                                                      :report "Skip this symbol."
                                                      nil)))
                                              (cdr package-and-symbols)))))
                        (when new-symbols
                          (cons package new-symbols))))
                  imports)))




(defun import (symbols &optional (package *package* package-supplied-p))
  (dolist (symbol (if (listp symbols) symbols (list symbols)))
    (let* ((sym-name (string symbol))
           (local-sym (find-symbol sym-name package)))
      (restart-case
          (progn
            (when (and local-sym (not (eql symbol local-sym)))
              (error 'package-error
                     "Different symbol (~A) with the same name already accessible in package ~A."
                     local-sym (package-name package)))
            (if package-supplied-p
                (%import (list symbol) package) ;; in order to pass NIL, wrap in a list
                (%import (list symbol))))
        (unintern-existing ()
          :report (lambda (s) (format s "Unintern ~S and continue" local-sym))
          (unintern local-sym)
          (%import symbol))
        (skip ()
          :report "Skip symbol"))))
  T)

