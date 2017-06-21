(in-package :jss)

(defun set-to-list (set)
  "Convert the java.util.Set named in SET to a Lisp list."
  (declare (optimize (speed 3) (safety 0)))
  (with-constant-signature ((iterator "iterator" t) (hasnext "hasNext") (next "next"))
    (loop with iterator = (iterator set)
       while (hasNext iterator)
       for item = (next iterator)
       collect item)))

(defun jlist-to-list (list)
  "Convert a LIST implementing java.util.List to a Lisp list."
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :from 0 :below (jcall "size" list)
     :collecting (jcall "get" list i)))

(defun jarray-to-list (jarray)
  "Convert the Java array named by JARRARY into a Lisp list."
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :from 0 :below (jarray-length jarray)
     :collecting (jarray-ref jarray i)))

;;; Deprecated 
;;; 
;;; XXX unclear what sort of list this would actually work on, as it
;;; certainly doesn't seem to be any of the Java collection types
;;; (what implements getNext())?
(defun list-to-list (list)
  (declare (optimize (speed 3) (safety 0)))
  (with-constant-signature ((isEmpty "isEmpty") (getfirst "getFirst")
                            (getNext "getNext"))
    (loop until (isEmpty list)
       collect (getFirst list)
       do (setq list (getNext list)))))

;; Contribution of Luke Hope. (Thanks!)

(defun iterable-to-list (iterable)
  "Return the items contained the java.lang.Iterable ITERABLE as a list."
  (declare (optimize (speed 3) (safety 0)))
  (let ((it (#"iterator" iterable)))
    (with-constant-signature ((has-next "hasNext")
                              (next "next"))
      (loop :while (has-next it)
         :collect (next it)))))

(defun vector-to-list (vector)
  "Return the elements of java.lang.Vector VECTOR as a list."
  (declare (optimize (speed 3) (safety 0)))
  (with-constant-signature ((has-more "hasMoreElements")
                            (next "nextElement"))
    (let ((elements (#"elements" vector)))
      (loop :while (has-more elements)
         :collect (next elements)))))

(defun hashmap-to-hashtable (hashmap &rest rest &key (keyfun #'identity) (valfun #'identity) (invert? nil)
                                                  table 
                             &allow-other-keys )
  "Converts the a HASHMAP reference to a java.util.HashMap object to a Lisp hashtable.

The REST paramter specifies arguments to the underlying MAKE-HASH-TABLE call.

KEYFUN and VALFUN specifies functions to be run on the keys and values
of the HASHMAP right before they are placed in the hashtable.

If INVERT? is non-nil than reverse the keys and values in the resulting hashtable."
  (let ((keyset (#"keySet" hashmap))
        (table (or table (apply 'make-hash-table
                                (loop for (key value) on rest by #'cddr
                                   unless (member key '(:invert? :valfun :keyfun :table)) 
                                   collect key and collect value)))))
    (with-constant-signature ((iterator "iterator" t) (hasnext "hasNext") (next "next"))
      (loop with iterator = (iterator keyset)
         while (hasNext iterator)
         for item = (next iterator)
         do (if invert?
                (setf (gethash (funcall valfun (#"get" hashmap item)) table) (funcall keyfun item))
                (setf (gethash (funcall keyfun item) table) (funcall valfun (#"get" hashmap item)))))
      table)))

;; ****************************************************************
;; But needing to remember is annoying

;; Here's a summary I gleaned:

;; java.util.Dictionary -> #"elements" yields java.util.Collections$3
;; java.util.AbstractCollection -> #"iterator" yields java.util.Iterator?
;; org.apache.felix.framework.util.CompoundEnumeration  -> implements java.util.Enumeration
;; java.util.Collections -> doc says #"iterator" yields java.util.Iterator
;; java.util.Collections$1) -> implements java.util.Iterator
;; java.util.Collections$2) -> implements java.util.Spliterator (#"iterator" (#"stream" 'StreamSupport <a spliterator>)) -> java.util.Iterator
;; java.util.Collections$3) -> implements java.util.Enumeration
;; java.util.Iterator
;;   ("next" "hasNext")
;; java.util.Enumeration)
;;   ("nextElement" "hasMoreElements")


(defun j2list (thing)
  "Attempt to construct a Lisp list out of a Java THING.

THING may be a wide range of Java collection types, their common
iterators or a Java array."
  (declare (optimize (speed 3) (safety 0)))
  (flet ((iterator-collect (iterator)
	   (with-constant-signature ((has-next "hasNext")
				     (next "next"))
	     (loop :while (has-next iterator)
		   :collect (next iterator))))
	 (enumeration-collect (enumeration)
	   (with-constant-signature ((has-next "hasMoreElements")
				     (next "nextElement"))
	     (loop :while (has-next enumeration)
		   :collect (next enumeration))))
	 (map-collect (map)
	   (with-constant-signature ((has-next "hasMoreElements")
				     (next "nextElement"))
	     (let ((keyiterator (#"iterator" (#"keyset" map))))
	       (loop :while (has-next keyiterator)
		     :for key = (next keyiterator)
                  :collect (cons key (#"get" map key)))))))
    (let ((isinstance
           (load-time-value (jmethod "java.lang.Class" "isInstance" "java.lang.Object"))))
      (cond
        ((jcall isinstance (load-time-value (ignore-errors (jclass "java.util.AbstractCollection"))) thing)
         (iterator-collect (#"iterator" thing)))
        ((jcall isinstance (load-time-value (ignore-errors (jclass "java.util.Iterator"))) thing)
         (iterator-collect thing))
        ((jcall isinstance (load-time-value (ignore-errors (jclass "java.util.Enumeration"))) thing)
         (enumeration-collect thing))
        ((jcall isinstance (load-time-value (ignore-errors (jclass "java.util.AbstractMap"))) thing)
         (map-collect thing))
        ((jcall isinstance (load-time-value (ignore-errors (jclass "java.util.Collections"))) thing)
         (iterator-collect (#"iterator" thing)))
        ((jcall isinstance (load-time-value (ignore-errors (jclass "java.util.Spliterator"))) thing)
         (iterator-collect (#"iterator" (#"stream" 'StreamSupport thing))))
        ((jcall isinstance (load-time-value (ignore-errors (jclass "java.util.Dictionary"))) thing)
         (iterator-collect (#"elements" thing)))
        ((ignore-errors (#"toArray" thing))
         (coerce (#"toArray" thing) 'list))
        (t
         (error "yet another iteration type - fix it: ~a" (jclass-name (jobject-class thing))))))))

(defun to-hashset (list)
  (let ((set (new 'hashset)))
    (loop for l in list do (#"add" set l))
    set))
