;;; cas.lisp
;;;
;;; Copyright (C) 2026 David John Cooper
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

;;; Compare-and-swap, following SBCL's protocol (SB-EXT:COMPARE-AND-SWAP,
;;; GET-CAS-EXPANSION, DEFINE-CAS-EXPANDER, DEFCAS and the ATOMIC-
;;; macros), on the primitives in Atomic.java: VarHandles on Java 9 and
;;; later, striped locks on Java 8 (see COMPARE-AND-SWAP-IMPLEMENTATION).

(in-package "EXTENSIONS")

(export '(compare-and-swap get-cas-expansion define-cas-expander defcas
          atomic-update atomic-incf atomic-decf atomic-push atomic-pop))

(defmacro define-cas-expander (accessor lambda-list &body body)
  "Define how COMPARE-AND-SWAP expands for places of the form (ACCESSOR ...).

LAMBDA-LIST destructures the arguments of such a place.  BODY returns the
six values that GET-CAS-EXPANSION returns."
  (let ((place (gensym "PLACE"))
        (env (gensym "ENV")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',accessor 'cas-expander)
             (lambda (,place ,env)
               (declare (ignorable ,env))
               (destructuring-bind ,lambda-list (rest ,place)
                 ,@body)))
       ',accessor)))

(defmacro defcas (accessor lambda-list cas-function &optional documentation)
  "Make (COMPARE-AND-SWAP (ACCESSOR . ARGS) OLD NEW) call CAS-FUNCTION with
ARGS, OLD and NEW, which must return the value it found in the place.
LAMBDA-LIST may contain only required and &OPTIONAL parameters, as in
the short form of DEFSETF."
  (declare (ignore documentation))
  (let ((required (loop for p in lambda-list
                        until (member p lambda-list-keywords)
                        collect p))
        (optional (rest (member '&optional lambda-list))))
    (when (set-difference (remove '&optional lambda-list)
                          (append required optional))
      (error "DEFCAS lambda list ~S has more than required and ~
              optional parameters." lambda-list))
    `(define-cas-expander ,accessor (&rest args)
       (let ((temps (mapcar (lambda (arg)
                              (declare (ignore arg))
                              (gensym))
                            args))
             (old (gensym "OLD"))
             (new (gensym "NEW")))
         (unless (<= ,(length required) (length args)
                     ,(+ (length required) (length optional)))
           (error "Wrong number of arguments in ~S."
                  (cons ',accessor args)))
         (values temps args old new
                 `(,',cas-function ,@temps ,old ,new)
                 `(,',accessor ,@temps))))))

(defun get-cas-expansion (place &optional environment)
  "Analogous to GET-SETF-EXPANSION.  Return six values: a list of temporary
variables, a list of the forms whose values they are bound to, a variable
for the old value of PLACE, a variable for the new value, a form that
uses these to compare and swap PLACE and returns the value it found
there, and a form that reads PLACE."
  (loop
    (let ((expander (and (consp place) (symbolp (car place))
                         (get (car place) 'cas-expander)))
          (structure-slot (and (consp place) (symbolp (car place))
                               (get (car place) 'sys::cas-structure-slot))))
      (cond (expander
             (return (funcall expander place environment)))
            (structure-slot
             (return (structure-slot-cas-expansion place structure-slot)))
            ((and (symbolp place)
                  (not (nth-value 1 (macroexpand-1 place environment)))
                  (special-variable-p place))
             (setf place `(symbol-value ',place)))
            (t
             (multiple-value-bind (expansion expanded)
                 (macroexpand-1 place environment)
               (unless expanded
                 (error "~S is not a place COMPARE-AND-SWAP supports." place))
               (setf place expansion)))))))

(defun structure-slot-cas-expansion (place structure-slot)
  (destructuring-bind (structure-name index type) structure-slot
    (unless (= (length place) 2)
      (error "Wrong number of arguments in ~S." place))
    (let ((instance (gensym "INSTANCE"))
          (old (gensym "OLD"))
          (new (gensym "NEW")))
      (values (list instance) (list (second place)) old new
              `(sys:%compare-and-swap-structure-ref
                (the ,structure-name ,instance) ,index ,old
                ,(if (eq type t) new `(the ,type ,new)))
              `(sys:structure-ref (the ,structure-name ,instance) ,index)))))

(defmacro compare-and-swap (place old new &environment env)
  "Atomically store NEW in PLACE if PLACE holds a value EQL to OLD.
Return the value PLACE held before; the swap happened if and only if it
is EQ to OLD.

PLACE may be (CAR x), (CDR x), (FIRST x), (REST x), (SVREF v i), a
structure slot accessor, (SYMBOL-VALUE s) or a special variable,
(SLOT-VALUE o name), (MOP:STANDARD-INSTANCE-ACCESS o location), or a
place given an expansion by DEFINE-CAS-EXPANDER or DEFCAS.  Like SBCL's
SB-EXT:COMPARE-AND-SWAP, but comparing with EQL, because ABCL boxes
fixnums and so two EQL fixnums need not be EQ.

SLOT-VALUE swaps the slot's storage directly, bypassing
SLOT-VALUE-USING-CLASS.  On Java 8 the swap takes a lock and is atomic
only with respect to other COMPARE-AND-SWAP operations."
  (multiple-value-bind (temps values old-var new-var cas-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list temps values)
            (,old-var ,old)
            (,new-var ,new))
       ,cas-form)))

(defcas car (cons) sys:%compare-and-swap-car)
(defcas cdr (cons) sys:%compare-and-swap-cdr)
(defcas first (cons) sys:%compare-and-swap-car)
(defcas rest (cons) sys:%compare-and-swap-cdr)
(defcas svref (simple-vector index) sys:%compare-and-swap-svref)
(defcas symbol-value (symbol) sys:%compare-and-swap-symbol-value)
(defcas slot-value (instance slot-name) sys:%compare-and-swap-slot-value)
(defcas sys:standard-instance-access (instance location)
  sys:%compare-and-swap-standard-instance-access)

(defmacro atomic-update (place update-fn &rest arguments &environment env)
  "Atomically set PLACE to the result of applying UPDATE-FN to ARGUMENTS
and the value of PLACE, and return that new value.  UPDATE-FN may be
called more than once."
  (multiple-value-bind (temps values old new cas-form read-form)
      (get-cas-expansion place env)
    (let ((fn (gensym "FN"))
          (args (loop repeat (length arguments) collect (gensym "ARG"))))
      `(let* (,@(mapcar #'list temps values)
              (,fn ,update-fn)
              ,@(mapcar #'list args arguments)
              (,old ,read-form))
         (loop
           (let ((,new (funcall ,fn ,@args ,old)))
             (if (eq ,old (setf ,old ,cas-form))
                 (return ,new))))))))

(defmacro atomic-incf (place &optional (delta 1) &environment env)
  "Atomically add DELTA to the number in PLACE and return PLACE's old value.
PLACE is any place COMPARE-AND-SWAP supports."
  (multiple-value-bind (temps values old new cas-form read-form)
      (get-cas-expansion place env)
    (let ((d (gensym "DELTA")))
      `(let* (,@(mapcar #'list temps values)
              (,d ,delta)
              (,old ,read-form))
         (loop
           (let ((,new (+ ,old ,d)))
             (if (eq ,old (setf ,old ,cas-form))
                 (return ,old))))))))

(defmacro atomic-decf (place &optional (delta 1))
  "Atomically subtract DELTA from the number in PLACE and return PLACE's
old value."
  `(atomic-incf ,place (- ,delta)))

(defmacro atomic-push (obj place)
  "Like PUSH, but atomic: PLACE is any place COMPARE-AND-SWAP supports.
Returns the new value of PLACE."
  (let ((o (gensym "OBJ")))
    `(let ((,o ,obj))
       (atomic-update ,place (lambda (list) (cons ,o list))))))

(defmacro atomic-pop (place &environment env)
  "Like POP, but atomic: PLACE is any place COMPARE-AND-SWAP supports."
  (multiple-value-bind (temps values old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list temps values)
            (,old ,read-form))
       (loop
         (let ((,new (cdr ,old)))
           (if (eq ,old (setf ,old ,cas-form))
               (return (car ,old))))))))
