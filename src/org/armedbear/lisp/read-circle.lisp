;;; read-circle.lisp
;;;
;;; Copyright (C) 2009 Erik Huelsmann
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


;;; Reading circular data: the #= and ## reader macros (from SBCL)

;;; Objects already seen by CIRCLE-SUBST.
(defvar *sharp-equal-circle-table*)

;; This function is kind of like NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists. The first arg is an
;; alist of the things to be replaced assoc'd with the things to replace them.
(defun circle-subst (old-new-alist tree)
  (macrolet ((recursable-element-p (subtree)
                `(typep ,subtree
                       '(or cons (array t) structure-object standard-object)))
             (element-replacement (subtree)
               `(let ((entry (find ,subtree old-new-alist :key #'second)))
                  (if entry (third entry) ,subtree))))
  (cond ((not (recursable-element-p tree))
         (element-replacement tree))
        ((null (gethash tree *sharp-equal-circle-table*))
         (cond
          ((typep tree 'structure-object)
           (setf (gethash tree *sharp-equal-circle-table*) t)
           (do ((i 0 (1+ i))
                (end (structure-length tree)))
               ((= i end))
             (let* ((old (structure-ref tree i))
                    (new (circle-subst old-new-alist old)))
               (unless (eq old new)
                 (structure-set tree i new)))))
;;           ((typep tree 'standard-object)
;;            (setf (gethash tree *sharp-equal-circle-table*) t)
;;            (do ((i 1 (1+ i))
;;                 (end (%instance-length tree)))
;;                ((= i end))
;;              (let* ((old (%instance-ref tree i))
;;                     (new (circle-subst old-new-alist old)))
;;                (unless (eq old new)
;;                  (setf (%instance-ref tree i) new)))))
          ((arrayp tree)
           (setf (gethash tree *sharp-equal-circle-table*) t)
           (do ((i 0 (1+ i))
                (end (array-total-size tree)))
               ((>= i end))
             (let* ((old (row-major-aref tree i))
                    (new (circle-subst old-new-alist old)))
               (unless (eq old new)
                 (setf (row-major-aref tree i) new)))))
         (t ;; being CONSP as all the other cases have been handled
            (do ((subtree tree (cdr subtree)))
                ((or (not (consp subtree))
                     (gethash subtree *sharp-equal-circle-table*)))
                ;; CDR no longer a CONS; no need to recurse any further:
                ;; the case where the CDR is a symbol to be replaced
                ;; has been handled in the last iteration
              (setf (gethash subtree *sharp-equal-circle-table*) t)
              (let* ((c (car subtree))
                     (d (cdr subtree))
                     (a (if (recursable-element-p c)
                            (circle-subst old-new-alist c)
                            (element-replacement c)))
                     (b (cond
                         ((consp d) d) ;; CONSes handled in the loop
                         ((recursable-element-p d)
                          ;; ARRAY, STRUCTURE-OBJECT and STANDARD-OBJECT
                          ;; handled in recursive calls
                          (circle-subst old-new-alist d))
                         (t
                          (element-replacement d)))))
                (unless (eq a c)
                  (rplaca subtree a))
                (unless (eq d b)
                  (rplacd subtree b))))))
        tree)
  (t tree))))

;;; Sharp-equal works as follows. When a label is assigned (i.e. when
;;; #= is called) we GENSYM a symbol is which is used as an
;;; unforgeable tag. *SHARP-SHARP-ALIST* maps the integer tag to this
;;; gensym.
;;;
;;; When SHARP-SHARP encounters a reference to a label, it returns the
;;; symbol assoc'd with the label. Resolution of the reference is
;;; deferred until the read done by #= finishes. Any already resolved
;;; tags (in *SHARP-EQUAL-ALIST*) are simply returned.
;;;
;;; After reading of the #= form is completed, we add an entry to
;;; *SHARP-EQUAL-ALIST* that maps the gensym tag to the resolved
;;; object. Then for each entry in the *SHARP-SHARP-ALIST, the current
;;; object is searched and any uses of the gensysm token are replaced
;;; with the actual value.

(defvar *sharp-sharp-alist* ())

(defun sharp-equal (stream label readtable)
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (error 'reader-error
           :stream stream
           :format-control "Missing label for #="))
  (when (or (assoc label *sharp-sharp-alist*)
            (assoc label *sharp-equal-alist*))
    (error 'reader-error
           :stream stream
           :format-control "Multiply defined label: #~D="
           :format-arguments (list label)))
  (let* ((tag (gensym))
         (*sharp-sharp-alist* (cons (list label tag nil) *sharp-sharp-alist*))
         (obj (let ((*readtable* readtable))
                (read stream t nil t))))
    (when (eq obj tag)
      (error 'reader-error
             :stream stream
             :format-control "Must tag something more than just #~D#"
             :format-arguments (list label)))
    (push (list label tag obj) *sharp-equal-alist*)
    (when (third (car *sharp-sharp-alist*)) ;; set to T on circularity
      (let ((*sharp-equal-circle-table* (make-hash-table :test 'eq :size 20)))
        (circle-subst *sharp-equal-alist* obj)))
    obj))

()

(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp nil))
  (unless label
    (error 'reader-error :stream stream :format-control "Missing label for ##"))
  (let ((entry (assoc label *sharp-equal-alist*)))
    (if entry
        (third entry)
        (let ((pair (assoc label *sharp-sharp-alist*)))
          (unless pair
            (error 'reader-error
                   :stream stream
                   :format-control "Object is not labelled #~S#"
                   :format-arguments (list label)))
          (setf (third pair) t)
          (second pair)))))

(set-dispatch-macro-character #\# #\= #'(lambda (stream ignore label)
                                          (declare (ignore ignore))
                                          (sharp-equal stream label
                                                       *readtable*))
                              +standard-readtable+)
(set-dispatch-macro-character #\# #\# #'sharp-sharp +standard-readtable+)

(set-dispatch-macro-character #\# #\= #'(lambda (stream ignore label)
                                          (declare (ignore ignore))
                                          (sharp-equal stream label
                                                       (get-fasl-readtable)))
                              (get-fasl-readtable))
(set-dispatch-macro-character #\# #\# #'sharp-sharp (get-fasl-readtable))

