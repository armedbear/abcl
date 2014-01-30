;;; directory.lisp
;;;
;;; Copyright (C) 2004-2007 Peter Graves
;;; Copyright (C) 2008 Ville Voutilainen
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

(in-package "SYSTEM")

(defun pathname-as-file (pathname)
  (let ((directory (pathname-directory pathname)))
    (make-pathname :host nil
                   :device (pathname-device pathname)
                   :directory (butlast directory)
                   :name (car (last directory))
                   :type nil
                   :version nil)))

(defun wild-inferiors-p (component)
  (eq component :wild-inferiors))

(defun list-directories-with-wildcards (pathname 
                                        wild-inferiors-found
                                        resolve-symlinks)
  (let* ((directory (pathname-directory pathname))
         (first-wild-inferior (and (not wild-inferiors-found) 
                                   (position-if #'wild-inferiors-p directory)))
         (first-wild (position-if #'wild-p directory))
         (wild (when (or first-wild-inferior first-wild)
                 (nthcdr (or first-wild-inferior first-wild) directory)))
         (non-wild (if (or first-wild-inferior first-wild)
                       (nbutlast directory
                                 (- (length directory) 
                                    (or first-wild-inferior first-wild)))
                     directory))
         (newpath (make-pathname :directory non-wild
                                 :name nil :type nil :defaults pathname))
         (entries (list-directory newpath resolve-symlinks)))
    (if (not (or wild wild-inferiors-found))
        entries
        (let ((inferior-entries (when (or wild-inferiors-found first-wild-inferior) entries)))
          (nconc 
           (mapcan (lambda (entry) 
                     (when (pathname-match-p (pathname entry) pathname)
                       (list entry)))
                   inferior-entries)
           (mapcan (lambda (entry)
                     (let* ((pathname (pathname entry))
                            (directory (pathname-directory pathname))
                            (rest-wild (cdr wild)))
                       (unless (pathname-name pathname)
                         (when (pathname-match-p (first (last directory))
                                    (cond ((eql (car wild) :wild)
                                           "*")
                                          ((eql (car wild) :wild-inferiors)
                                           "*")
                                          (wild 
                                           (car wild))
                                          (t "")))
                           (when (and 
                                  (not (or first-wild-inferior 
                                           wild-inferiors-found)) 
                                  rest-wild)
                             (setf directory (nconc directory rest-wild)))
                           (let ((recurse (make-pathname :directory directory
                                                  :defaults newpath)))
                             (when (not (equal recurse newpath))
                               (list-directories-with-wildcards
                                recurse
                                (or first-wild-inferior wild-inferiors-found)
                                resolve-symlinks)))))))
                   entries))))))

;;; XXX Kludge for compatibilty:  hope no one uses.
(defun directory-old (pathspec &key (resolve-symlinks t))
  (warn "Deprecated:  Please use CL:DIRECTORY which has a NIL default for :RESOLVE-SYMLINKS.")
  (directory pathspec :resolve-symlinks resolve-symlinks))

(defun directory (pathspec &key (resolve-symlinks nil))
  "Determines which, if any, files that are present in the file system have names matching PATHSPEC, and returns
a fresh list of pathnames corresponding to the potential truenames of those files.  

With :RESOLVE-SYMLINKS set to nil, not all pathnames returned may
correspond to an existing file.  Symbolic links are considered to be
be valid entries even if they do not currently have a valid file or
directory as a target.  Therefore, subsequent CL:TRUENAME call on
individual pathnames in the list may signal an error, i.e. the
pathnames have been constructed as truenames, without calling the
entire resolution routine of CL:TRUENAME.

If called with :RESOLVE-SYMLINKS set to T, and any of the pathnames
have truenames which do not exist, this routine will signal a file
error to its caller."

  (let ((pathname (merge-pathnames pathspec)))
    (when (logical-pathname-p pathname)
      (setq pathname (translate-logical-pathname pathname)))
    (if (or (position #\* (namestring pathname))
            (wild-pathname-p pathname))
        (if (pathname-jar-p pathname)
            (match-wild-jar-pathname pathname)
            (let ((namestring (directory-namestring pathname)))
              (when (and namestring (> (length namestring) 0))
                (when (featurep :windows)
                  (let ((device (pathname-device pathname)))
                    (when device
                      (setq namestring (concatenate 'string device ":" namestring)))))
                (let ((entries (list-directories-with-wildcards 
                                namestring nil resolve-symlinks))
                      (matching-entries ()))
                  (dolist (entry entries)
					(when 
						(or 
						 (and 
						  (file-directory-p entry)
						  (pathname-match-p (file-namestring (pathname-as-file entry)) 
											(file-namestring pathname)))
						 (pathname-match-p (or (file-namestring entry) "") (file-namestring pathname)))
					  (push 
					   (if resolve-symlinks
						   (truename entry) 
						   entry)
					   matching-entries)))
                  matching-entries))))
        ;; Not wild.
        (let ((truename (probe-file pathname)))
          (if truename
              (list (pathname truename))
              nil)))))
