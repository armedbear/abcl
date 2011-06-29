(require "CLOS")
(require "JAVA")
(require "EXTENSIBLE-SEQUENCES")

(in-package :java)

(let* ((jclass (jclass "java.util.List"))
       (class (%find-java-class jclass)))
  (if class
      (error "java.util.List is already registered as a Lisp class; since JAVA-CLASSes can't be redefined, I can't inject SEQUENCE in its class precedence list. Ensure that you require :java-collections before specializing any method on java.util.List and in general before using java.util.List as a CLOS class.")
      ;;The code below is adapted from ensure-java-class in java.lisp
      (%register-java-class
       jclass (mop::ensure-class
               (make-symbol (jclass-name jclass))
               :metaclass (find-class 'java-class)
               :direct-superclasses
               (let ((supers
                      (mapcar #'ensure-java-class
                              (delete nil
                                      (concatenate 'list
                                                   (list (jclass-superclass jclass))
                                                   (jclass-interfaces jclass))))))
                 (append supers (list (find-class 'sequence)) (jclass-additional-superclasses jclass)))
               :java-class jclass))))

(defmethod print-object ((coll (jclass "java.util.Collection")) stream)
  (print-unreadable-object (coll stream :type t :identity t)
    (format stream "~A ~A"
	    (jclass-of coll)
	    (jcall "toString" coll))))

;;Lists (java.util.List) are the Java counterpart to Lisp SEQUENCEs.
(defun jlist-add (list item)
  (jcall (jmethod "java.util.List" "add" "java.lang.Object")
	 list item))

(defun jlist-set (list index item)
  (jcall (jmethod "java.util.List" "set" "int" "java.lang.Object")
	 list index item))

(defun jlist-get (list index)
  (jcall (jmethod "java.util.List" "get" "int")
	 list index))

(defmethod sequence:length ((s (jclass "java.util.List")))
  (jcall (jmethod "java.util.Collection" "size") s))

(defmethod sequence:elt ((s (jclass "java.util.List")) index)
  (jlist-get s index))

(defmethod (setf sequence:elt) (value (list (jclass "java.util.List")) index)
  (jlist-set list index value)
  value)

(defmethod sequence:make-sequence-like
    ((s (jclass "java.util.List")) length
     &rest args &key initial-element initial-contents)
  (declare (ignorable initial-element initial-contents))
  (apply #'make-jsequence-like s length #'jlist-add args))

(defun make-jsequence-like
    (s length add-fn &key (initial-element nil iep) (initial-contents nil icp))
  (let ((seq (jnew (jclass-of s))))
    (cond
      ((and icp iep)
       (error "Can't specify both :initial-element and :initial-contents"))
      (icp
       (dotimes (i length)
	 (funcall add-fn seq (elt initial-contents i)))) ;;TODO inefficient, use iterator
      (t
       (dotimes (i length)
	 (funcall add-fn seq initial-element))))
    seq))

;;TODO: destruct doesn't signal an error for too-many-args for its options
;;e.g. this didn't complain:
;;(defstruct (jlist-iterator (:type list :conc-name #:jlist-it-))
(defstruct (jlist-iterator (:type list) (:conc-name #:jlist-it-))
  (native-iterator (error "Native iterator required") :read-only t)
  element
  index)

(defmethod sequence:make-simple-sequence-iterator
    ((s (jclass "java.util.List")) &key from-end (start 0) end)
  (let* ((end (or end (length s)))
	 (index (if from-end end start))
	 (it (jcall "listIterator" s index))
	 (iter (make-jlist-iterator :native-iterator it
				    :index (if from-end (1+ index) (1- index))))
	 (limit (if from-end (1+ start) (1- end))))
    ;;CL iterator semantics are that first element is present from the start
    (unless (sequence:iterator-endp s iter limit from-end)
      (sequence:iterator-step s iter from-end))
    (values iter limit from-end)))

;;Collection, and not List, because we want to reuse this for Set when applicable
(defmethod sequence:iterator-step
    ((s (jclass "java.util.Collection")) it from-end)
  (let ((native-it (jlist-it-native-iterator it)))
    (if from-end
	(progn
	  (setf (jlist-it-element it)
		(when (jcall "hasPrevious" native-it)
		  (jcall "previous" native-it)))
	  (decf (jlist-it-index it)))
	(progn
	  (setf (jlist-it-element it)
		(when (jcall "hasNext" native-it)
		  (jcall "next" native-it)))
	  (incf (jlist-it-index it)))))
  it)

(defmethod sequence:iterator-endp
    ((s (jclass "java.util.Collection")) it limit from-end)
  (if from-end
      (< (jlist-it-index it) limit)
      (> (jlist-it-index it) limit)))

(defmethod sequence:iterator-element
    ((s (jclass "java.util.Collection")) iterator)
  (declare (ignore s))
  (jlist-it-element iterator))

(defmethod (setf sequence:iterator-element)
    (new-value (s (jclass "java.util.Collection")) it)
  (jcall "set" (jlist-it-native-iterator it) new-value))

(defmethod sequence:iterator-index
    ((s (jclass "java.util.Collection")) iterator)
  (declare (ignore s))
  (jlist-it-index iterator))

(defmethod sequence:iterator-copy ((s (jclass "java.util.Collection")) iterator)
  (declare (ignore s iterator))
  (error "iterator-copy not supported for Java iterators."))

;;It makes sense to have some sequence functions available for Sets
;;(java.util.Set) too, even if they're not sequences.
(defun jset-add (set item)
  (jcall (jmethod "java.util.Set" "add" "java.lang.Object")
	 set item))

(defmethod sequence:length ((s (jclass "java.util.Set")))
  (jcall (jmethod "java.util.Collection" "size") s))

(defmethod sequence:make-sequence-like
    ((s (jclass "java.util.Set")) length
     &rest args &key initial-element initial-contents)
  (declare (ignorable initial-element initial-contents))
  (apply #'make-jsequence-like s length #'jset-add args))

(defmethod sequence:make-simple-sequence-iterator
    ((s (jclass "java.util.Set")) &key from-end (start 0) end)
  (when (or from-end (not (= start 0)))
    (error "Java Sets can only be iterated from the start."))
  (let* ((end (or end (length s)))
	 (it (jcall "iterator" s))
	 (iter (make-jlist-iterator :native-iterator it
				    :index -1))
	 (limit (1- end)))
    ;;CL iterator semantics are that first element is present from the start
    (unless (sequence:iterator-endp s iter limit nil)
      (sequence:iterator-step s iter nil))
    (values iter limit nil)))

(provide :java-collections)