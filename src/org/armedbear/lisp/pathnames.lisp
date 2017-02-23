;;; pathnames.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves
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

(export '(logical-host-p))

(defun pathname-host (pathname &key (case :local))
  (%pathname-host pathname case))

(defun pathname-device (pathname &key (case :local))
  (%pathname-device pathname case))

(defun pathname-directory (pathname &key (case :local))
  (%pathname-directory pathname case))

(defun pathname-name (pathname &key (case :local))
  (%pathname-name pathname case))

(defun pathname-type (pathname &key (case :local))
  (%pathname-type pathname case))

(defun wild-pathname-p (pathname &optional field-key)
  (%wild-pathname-p pathname field-key))

(defun component-match-wild-p (thing wild ignore-case)
  (let ((testfunc (if ignore-case #'equalp #'equal)))
    (labels ((split-string (delim str)
	       (flet ((finder (char) (find char delim)))
		 (loop  for x = (position-if-not #'finder str) then
		      (position-if-not #'finder str :start (or y (length str)))
		    for y = (position-if #'finder str :start x) then
		      (position-if #'finder str :start (or x (length str))) while x 
		    collect (subseq str x y))))
	     (positions-larger (thing substrings previous-pos)
	       (let ((new-pos (search (car substrings) 
				      thing 
				      :start2 previous-pos
				      :test testfunc)))
		 (or 
		  (not substrings)
		  (and new-pos
		       (>= new-pos previous-pos)
		       (positions-larger thing 
					 (cdr substrings) 
					 new-pos))))))
      (let ((split-result (split-string "*" wild)))
	(and (positions-larger thing split-result 0)
	     (if (eql (elt wild 0) #\*)
		 t
		 (eql (search (first split-result) thing :test testfunc) 0))
	     (if (eql (elt wild (1- (length wild))) #\*)
		 t
		 (let ((last-split-result (first (last split-result))))
		   (eql (search last-split-result thing :from-end t 
				:test testfunc)
			(- (length thing) (length last-split-result))))))))))

(defun component-match-p (thing wild ignore-case)
  (cond ((eq wild :wild)
         t)
        ((null wild)
         t)
        ((and (stringp wild) (position #\* wild))
	 (component-match-wild-p thing wild ignore-case))
        (ignore-case
         (equalp thing wild))
        (t
         (equal thing wild))))

(defun directory-match-components (thing wild ignore-case)
  (loop
    (cond ((endp thing)
           (return (or (endp wild) (equal wild '(:wild-inferiors)))))
          ((endp wild)
           (return nil)))
    (let ((x (car thing))
          (y (car wild)))
      (when (eq y :wild-inferiors)
        (return t))
      (unless (component-match-p x y ignore-case)
        (return nil))
      (setf thing (cdr thing)
            wild  (cdr wild)))))

(defun directory-match-p (thing wild ignore-case)
  (cond ((eq wild :wild)
         t)
        ((null wild)
         t)
        ((and ignore-case (equalp thing wild))
         t)
        ((equal thing wild)
         t)
        ((and (null thing) (equal wild '(:absolute :wild-inferiors)))
         t)
        ((and (consp thing) (consp wild))
         (if (eq (%car thing) (%car wild))
             (directory-match-components (%cdr thing) (%cdr wild) ignore-case)
             nil))
        (t
         nil)))

(defun pathname-match-p (pathname wildcard)
  (setf pathname (pathname pathname)
        wildcard (pathname wildcard))
  (unless (component-match-p (pathname-host pathname) (pathname-host wildcard) nil)
    (return-from pathname-match-p nil))
  (when (and (pathname-jar-p pathname) 
             (pathname-jar-p wildcard))
    (unless 
        (every (lambda (value) (not (null value)))
               (mapcar #'pathname-match-p 
                       (pathname-device pathname)  
                       (pathname-device wildcard)))
      (return-from pathname-match-p nil)))
  (when (or (and (pathname-jar-p pathname)
                 (not (pathname-jar-p wildcard)))
            (and (not (pathname-jar-p pathname))
                 (pathname-jar-p wildcard)))
    (return-from pathname-match-p nil))
  (let* ((windows-p (featurep :windows))
         (ignore-case (or windows-p (typep pathname 'logical-pathname))))
    (cond ((and windows-p
                (not (pathname-jar-p pathname))
                (not (pathname-jar-p wildcard))
                (not (component-match-p (pathname-device pathname)
                                        (pathname-device wildcard)
                                        ignore-case)))
           nil)
          ((not (directory-match-p (pathname-directory pathname)
                                   (pathname-directory wildcard)
                                   ignore-case))
           nil)
          ((not (component-match-p (pathname-name pathname)
                                   (pathname-name wildcard)
                                   ignore-case))
           nil)
          ((not (component-match-p (pathname-type pathname)
                                   (pathname-type wildcard)
                                   ignore-case))
           nil)
          (t
           t))))

(defun wild-p (component)
  (or (eq component :wild)
      (and (stringp component)
           (position #\* component))))

(defun casify (thing case)
  (typecase thing
    (string
     (case case
       (:upcase (string-upcase thing))
       (:downcase (string-downcase thing))
       (t thing)))
    (list
     (let (result)
       (dolist (component thing (nreverse result))
         (push (casify component case) result))))
    (t
     thing)))

(defun translate-component (source from to &optional case)
  (declare (ignore from))
  (cond ((or (eq to :wild) (null to))
         ;; "If the piece in TO-WILDCARD is :WILD or NIL, the piece in source
         ;; is copied into the result."
         (casify source case))
        ((and to (not (wild-p to)))
        ;; "If the piece in TO-WILDCARD is present and not wild, it is copied
        ;; into the result."
         to)
        (t
         ;; "Otherwise, the piece in TO-WILDCARD might be a complex wildcard
         ;; such as "foo*bar" and the piece in FROM-WILDCARD should be wild;
         ;; the portion of the piece in SOURCE that matches the wildcard
         ;; portion of the piece in FROM-WILDCARD replaces the wildcard portion
         ;; of the piece in TO-WILDCARD and the value produced is used in the
         ;; result."
         ;; FIXME
         (error "Unsupported wildcard pattern: ~S" to))))

(defun translate-jar-device (source from to &optional case)
  (declare (ignore case)) ; FIXME
  (unless to
    (return-from translate-jar-device nil))
  (when (not (= (length source) 
                (length from)
                (length to)))
    (error "Unsupported pathname translation for unequal jar ~
  references: ~S != ~S != ~S" source from to))
  (mapcar #'translate-pathname source from to))

(defun translate-directory-components-aux (src from to case)
  (cond
    ((and (null src) (null from) (null to))
     NIL)
    ((and to
          (not (member (car to) '(:wild :wild-inferiors))))
     (cons (casify (car to) case)
           (translate-directory-components-aux 
            src from (cdr to) case)))
    ((and (not src) 
          (eq (car from) :wild-inferiors) 
          (eq (car to) :wild-inferiors))
     (translate-directory-components-aux src (cdr from) (cdr to) case))
    ((not (and src from))
     ;; both are NIL --> TO is a wildcard which can't be matched
     ;; either is NIL --> SRC can't be fully matched against FROM, vice versa
     (throw 'failed-match))
    ((not (member (car from) '(:wild :wild-inferiors)))
     (unless (string= (casify (car src) case) (casify (car from) case))
       (throw 'failed-match)) ;; FROM doesn't match SRC
     (translate-directory-components-aux (cdr src) (cdr from) to case))
    ((not (eq (car from) (car to))) ;; TO is NIL while FROM is not, or
     (throw 'failed-match))         ;; FROM wildcard doesn't match TO wildcard
    ((eq (car to) :wild)  ;; FROM and TO wildcards are :WILD
     (cons (casify (car src) case)
       (translate-directory-components-aux (cdr src) (cdr from) (cdr to) case)))
    ((eq (car to) :wild-inferiors) ;; FROM and TO wildcards are :WILD-INFERIORS
     (do ((src (cdr src) (cdr src))
          (match (list (casify (car src) case))
                 (cons (casify (car src) case) match)))
         (NIL) ;; we'll exit the loop in different ways
       (catch 'failed-match
         (return-from translate-directory-components-aux
           (append (reverse match) 
                   (translate-directory-components-aux
                    src (cdr from) (cdr to) case))))
       (when (and (null src) 
                  (eq (car from) :wild-inferiors)
                  (eq (car to) :wild-inferiors))
         (return-from translate-directory-components-aux nil))
       (when (null src) ;; SRC is NIL and we're still here: error exit
         (throw 'failed-match))))))

(defun translate-directory-components (src from to case)
  (catch 'failed-match
    (return-from translate-directory-components
      (translate-directory-components-aux src from to case)))
  (error "Unsupported case in TRANSLATE-DIRECTORY-COMPONENTS."))


(defun translate-directory (source from to case)
  ;; FIXME The IGNORE-CASE argument to DIRECTORY-MATCH-P should not be nil on
  ;; Windows or if the source pathname is a logical pathname.
  ;; FIXME We can canonicalize logical pathnames to upper case, so we only need
  ;; IGNORE-CASE for Windows.
  (cond ((null source)
         to)
        ((equal source '(:absolute))
         (remove :wild-inferiors to))
        (t
         (translate-directory-components source from to case))))

;; "The resulting pathname is TO-WILDCARD with each wildcard or missing field
;; replaced by a portion of SOURCE."
(defun translate-pathname (source from-wildcard to-wildcard &key)
  (unless (pathname-match-p source from-wildcard)
    (error "~S and ~S do not match." source from-wildcard))
  (let* ((source (pathname source))
         (from   (pathname from-wildcard))
         (to     (pathname to-wildcard))
         (device (if (typep 'to 'logical-pathname)
                     :unspecific
                     (if (pathname-jar-p source)
                         (translate-jar-device (pathname-device source)
                                               (pathname-device from)
                                               (pathname-device to))
                         (translate-component (pathname-device source)
                                              (pathname-device from)
                                              (pathname-device to)))))
         (case   (and (typep source 'logical-pathname)
                      (or (featurep :unix) (featurep :windows))
                      :downcase)))
    (make-pathname :host      (pathname-host to)
                   :device    (cond ((typep to 'logical-pathname)
                                     :unspecific)
                                    ((eq device :unspecific)
                                     nil)
                                    (t
                                     device))
                   :directory (translate-directory (pathname-directory source)
                                                   (pathname-directory from)
                                                   (pathname-directory to)
                                                   case)
                   :name      (translate-component (pathname-name source)
                                                   (pathname-name from)
                                                   (pathname-name to)
                                                   case)
                   :type      (translate-component (pathname-type source)
                                                   (pathname-type from)
                                                   (pathname-type to)
                                                   case)
                   :version   (if (null (pathname-host from))
                                  (if (or (eq (pathname-version to) :wild)
                                          (eq (pathname-version to) nil))
                                      (pathname-version source)
                                      (pathname-version to))
                                  (translate-component (pathname-version source)
                                                       (pathname-version from)
                                                       (pathname-version to))))))

(defun logical-host-p (canonical-host)
  (multiple-value-bind (translations present)
      (gethash canonical-host *logical-pathname-translations*)
    (declare (ignore translations))
    present))

(defun logical-pathname-translations (host)
  (multiple-value-bind (translations present)
      (gethash (canonicalize-logical-host host) *logical-pathname-translations*)
    (unless present
      (error 'type-error
             :datum host
             :expected-type '(and string (satisfies logical-host-p))))
    translations))

(defun canonicalize-logical-pathname-translations (translations host)
  (let (result)
    (dolist (translation translations (nreverse result))
      (let ((from (car translation))
            (to (cadr translation)))
        (push (list (if (typep from 'logical-pathname)
                        from
                        (parse-namestring from host))
                    (pathname to))
              result)))))

(defun %set-logical-pathname-translations (host translations)
  (setf host (canonicalize-logical-host host))
  ;; Avoid undefined host error in CANONICALIZE-LOGICAL-PATHNAME-TRANSLATIONS.
  (unless (logical-host-p host)
    (setf (gethash host *logical-pathname-translations*) nil))
  (setf (gethash host *logical-pathname-translations*)
        (canonicalize-logical-pathname-translations translations host)))

(defsetf logical-pathname-translations %set-logical-pathname-translations)

(defun translate-logical-pathname (pathname &key)
  (typecase pathname
    (logical-pathname
     (let* ((host (pathname-host pathname))
            (translations (logical-pathname-translations host)))
       (dolist (translation translations
                            (error 'file-error
                                   :pathname pathname
                                   :format-control "No translation for ~S"
                                   :format-arguments (list pathname)))
         (let ((from-wildcard (car translation))
               (to-wildcard (cadr translation)))
           (when (pathname-match-p pathname from-wildcard)
             (return (translate-logical-pathname
                      (translate-pathname pathname from-wildcard to-wildcard))))))))
    (pathname pathname)
    (t
     (translate-logical-pathname (pathname pathname)))))

(defun load-logical-pathname-translations (host)
  (declare (type string host))
  (multiple-value-bind (ignore found)
      (gethash (canonicalize-logical-host host)
               *logical-pathname-translations*)
    (declare (ignore ignore))
    (unless found
      (error "The logical host ~S was not found." host))))

(defun logical-pathname (pathspec)
  (typecase pathspec
    (logical-pathname pathspec)
    (string
     (%make-logical-pathname pathspec))
    (stream
     (let ((result (pathname pathspec)))
       (if (typep result 'logical-pathname)
           result
           (error 'simple-type-error
                  :datum result
                  :expected-type 'logical-pathname))))
    (t
     (error 'type-error
            :datum pathspec
            :expected-type '(or logical-pathname string stream)))))

(defun parse-namestring (thing
                         &optional host (default-pathname *default-pathname-defaults*)
                         &key (start 0) end junk-allowed)
  (declare (ignore junk-allowed)) ; FIXME
  (cond ((eq host :unspecific)
         (setf host nil))
        ((consp host)) ;; A URL 
        (host
         (setf host (canonicalize-logical-host host))))
  (typecase thing
    (stream
     (values (pathname thing) start))
    (pathname
     (values thing start))
    (string
     (unless end
       (setf end (length thing)))
     (%parse-namestring (subseq thing start end) host default-pathname))
    (t
     (error 'type-error
            :format-control "~S cannot be converted to a pathname."
            :format-arguments (list thing)))))


;;; Functions for dealing with URL Pathnames

(in-package :extensions)

(defun url-pathname-scheme (p)
  (unless (pathname-url-p p)
    (error "~A is not a URL pathname." p))
  (getf (pathname-host p) :scheme))

(defun set-url-pathname-scheme (p v)
  (unless (pathname-url-p p)
    (error "~A is not a URL pathname." p))
  (let ((host (pathname-host p)))
    (setf (getf host :scheme) v))
  (%invalidate-namestring p))

(defsetf url-pathname-scheme set-url-pathname-scheme)

(defun url-pathname-authority (p)
  (unless (pathname-url-p p)
    (error "~A is not a URL pathname." p))
  (getf (pathname-host p) :authority))

(defun set-url-pathname-authority (p v)
  (unless (pathname-url-p p)
    (error "~A is not a URL pathname." p))
  (let ((host (pathname-host p)))
    (setf (getf host :authority) v))
  (%invalidate-namestring p))

(defsetf url-pathname-authority set-url-pathname-authority)

(defun url-pathname-query (p)
  (unless (pathname-url-p p)
    (error "~A is not a URL pathname." p))
  (getf (pathname-host p) :query))

(defun set-url-pathname-query (p v)
  (unless (pathname-url-p p)
    (error "~A is not a URL pathname." p))
  (let ((host (pathname-host p)))
    (setf (getf host :query) v))
  (%invalidate-namestring p))

(defsetf url-pathname-query set-url-pathname-query)

(defun url-pathname-fragment (p)
  (unless (pathname-url-p p)
    (error "~A is not a URL pathname." p))
  (getf (pathname-host p) :fragment))

(defun set-url-pathname-fragment (p v)
  (unless (pathname-url-p p)
    (error "~A is not a URL pathname." p))
  (let ((host (pathname-host p)))
    (setf (getf host :fragment) v))
  (%invalidate-namestring p))

(defsetf url-pathname-fragment set-url-pathname-fragment)

(export '(url-pathname-scheme
          url-pathname-authority
          url-pathname-query
          url-pathname-fragment) 
        'ext)
