;;; jvm.lisp
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

(in-package "JVM")

(export '(compile-defun *catch-errors* jvm-compile-package
          derive-compiler-type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "LOOP")
  (require "FORMAT")
  (require "CLOS")
  (require "PRINT-OBJECT")
  (require "COMPILER-TYPES")
  (require "COMPILER-ERROR")
  (require "KNOWN-FUNCTIONS")
  (require "DUMP-FORM")
  (require "JVM-INSTRUCTIONS")
  (require "JVM-CLASS-FILE")
  (require "KNOWN-SYMBOLS")
  (require "JAVA")
  (require "COMPILER-PASS1")
  (require "COMPILER-PASS2"))

(defvar *closure-variables* nil)

(defvar *enable-dformat* nil)
(defvar *callbacks* nil
  "A list of functions to be called by the compiler and code generator
in order to generate 'compilation events'.")

(declaim (inline invoke-callbacks))
(defun invoke-callbacks (&rest args)
  (dolist (cb *callbacks*)
    (apply cb args)))

#+nil
(defun dformat (destination control-string &rest args)
  (when *enable-dformat*
    (apply #'sys::%format destination control-string args)))

(defmacro dformat (&rest ignored)
  (declare (ignore ignored)))

(declaim (inline u2 s1 s2))

(defknown u2 (fixnum) cons)
(defun u2 (n)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) n))
  (when (not (<= 0 n 65535))
    (error "u2 argument ~A out of 65k range." n))
  (list (logand (ash n -8) #xff)
        (logand n #xff)))

(defknown s1 (fixnum) fixnum)
(defun s1 (n)
  (declare (optimize speed))
  (declare (type (signed-byte 8) n))
  (when (not (<= -128 n 127))
    (error "s1 argument ~A out of 8-bit signed range." n))
  (if (< n 0)
      (1+ (logxor (- n) #xFF))
      n))


(defknown s2 (fixnum) cons)
(defun s2 (n)
  (declare (optimize speed))
  (declare (type (signed-byte 16) n))
  (when (not (<= -32768 n 32767))
    (error "s2 argument ~A out of 16-bit signed range." n))
  (u2 (if (< n 0) (1+ (logxor (- n) #xFFFF))
          n)))





(defmacro with-saved-compiler-policy (&body body)
  "Saves compiler policy variables, restoring them after evaluating `body'."
  `(let ((*speed* *speed*)
         (*space* *space*)
         (*safety* *safety*)
         (*debug* *debug*)
         (*explain* *explain*)
         (*inline-declarations* *inline-declarations*))
     ,@body))



(defvar *compiler-debug* nil)

(defvar *pool* nil)
(defvar *static-code* ())
(defvar *class-file* nil)

(defvar *externalized-objects* nil)
(defvar *declared-functions* nil)

(defstruct (abcl-class-file (:include class-file)
                            (:constructor %make-abcl-class-file))
  pathname ; pathname of output file
  class-name
  lambda-name
  lambda-list ; as advertised
  static-initializer
  constructor
  objects ;; an alist of externalized objects and their field names
  (functions (make-hash-table :test 'equal)) ;; because of (SETF ...) functions
  )

(defun class-name-from-filespec (filespec)
  (let* ((name (pathname-name filespec)))
    (declare (type string name))
    (dotimes (i (length name))
      (declare (type fixnum i))
      (when (or (char= (char name i) #\-)
                (char= (char name i) #\Space))
        (setf (char name i) #\_)))
    (make-jvm-class-name
     (concatenate 'string "org.armedbear.lisp." name))))

(defun make-unique-class-name ()
  "Creates a random class name for use with a `class-file' structure's
`class' slot."
  (make-jvm-class-name
   (concatenate 'string "abcl_"
                (substitute #\_ #\-
                            (java:jcall (java:jmethod "java.util.UUID"
                                                      "toString")
                                        (java:jstatic "randomUUID"
                                                      "java.util.UUID"))))))

(defun make-abcl-class-file (&key pathname lambda-name lambda-list)
  "Creates a `class-file' structure. If `pathname' is non-NIL, it's
used to derive a class name. If it is NIL, a random one created
using `make-unique-class-name'."
  (let* ((class-name (if pathname
                         (class-name-from-filespec  pathname)
                         (make-unique-class-name)))
         (class-file (%make-abcl-class-file :pathname pathname
                                            :class class-name ; to be finalized
                                            :class-name class-name
                                            :lambda-name lambda-name
                                            :lambda-list lambda-list
                                            :access-flags '(:public :final))))
    (when *file-compilation*
      (let ((source-attribute
             (make-source-file-attribute
              :filename (file-namestring *compile-file-truename*))))
        (class-add-attribute class-file source-attribute)))
    class-file))

(defmacro with-class-file (class-file &body body)
  (let ((var (gensym)))
    `(let* ((,var                   ,class-file)
            (*class-file*           ,var)
            (*pool*                 (abcl-class-file-constants ,var))
            (*externalized-objects* (abcl-class-file-objects ,var))
            (*declared-functions*   (abcl-class-file-functions ,var)))
       (progn ,@body)
       (setf (abcl-class-file-objects ,var)      *externalized-objects*
             (abcl-class-file-functions ,var)    *declared-functions*))))

(defstruct compiland
  name
  lambda-expression
  arg-vars          ; variables for lambda arguments
  free-specials     ;
  arity             ; number of args, or NIL if the number of args can vary.
  p1-result         ; the parse tree as created in pass 1
  parent            ; the parent for compilands which defined within another
  (children 0       ; Number of local functions
            :type fixnum) ; defined with FLET, LABELS or LAMBDA
  blocks            ; TAGBODY, PROGV, BLOCK, etc. blocks
  argument-register
  closure-register
  environment-register
  class-file ; class-file object
  (%single-valued-p t))

(defknown compiland-single-valued-p (t) t)
(defun compiland-single-valued-p (compiland)
  (unless (compiland-parent compiland)
    (let ((name (compiland-name compiland)))
      (when name
        (let ((result-type
               (or (function-result-type name)
                   (and (proclaimed-ftype name)
                        (ftype-result-type (proclaimed-ftype name))))))
          (when result-type
            (return-from compiland-single-valued-p
                         (cond ((eq result-type '*)
                                nil)
                               ((atom result-type)
                                t)
                               ((eq (%car result-type) 'VALUES)
                                (= (length result-type) 2))
                               (t
                                t))))))))
  ;; Otherwise...
  (compiland-%single-valued-p compiland))

(defvar *current-compiland* nil)

(defvar *this-class* nil)

;; All tags visible at the current point of compilation, some of which may not
;; be in the current compiland.
(defvar *visible-tags* ())

;; The next available register.
(defvar *register* 0)

;; Total number of registers allocated.
(defvar *registers-allocated* 0)

;; Variables visible at the current point of compilation.
(defvar *visible-variables* nil
  "All variables visible to the form currently being
processed, including free specials.")

;; All variables seen so far.
(defvar *all-variables* nil
  "All variables in the lexical scope (thus excluding free specials)
of the compilands being processed (p1: so far; p2: in total).")

;; Undefined variables that we've already warned about.
(defvar *undefined-variables* nil)

(defvar *dump-variables* nil)

(defun dump-1-variable (variable)
  (sys::%format t "  ~S special-p = ~S register = ~S binding-reg = ~S index = ~S declared-type = ~S~%"
           (variable-name variable)
           (variable-special-p variable)
           (variable-register variable)
           (variable-binding-register variable)
           (variable-index variable)
           (variable-declared-type variable)))

(defun dump-variables (list caption &optional (force nil))
  (when (or force *dump-variables*)
    (write-string caption)
    (if list
        (dolist (variable list)
          (dump-1-variable variable))
        (sys::%format t "  None.~%"))))

(defstruct (variable-info (:conc-name variable-)
                          (:constructor make-variable)
                          (:predicate variable-p))
  name
  initform
  (declared-type :none)
  (derived-type :none)
  ignore-p
  ignorable-p
  representation
  special-p     ; indicates whether a variable is special

;; A variable can be stored in a number of locations.
;;  1. if it's passed as a normal argument, it'll be in a register (max 8)
;;     the same is true if the variable is a local variable (at any index)
;;  2. if it's passed in the argument array, it'll be in the array in
;;     register 1 (register 0 contains the function object)
;;  3. if the variable is part of a closure, it'll be in the closure array
;;  4. if the variable is part of the outer scope of a function with a
;;     non-null lexical environment, the variable is to be looked up
;;     from a lexical environment object
;;  5. the variable is a special variable and its binding has been looked
;;     up and cached in a local register (binding-register)

;; a variable can be either special-p *or* have a register *or*
;; have an index *or* a closure-index *or* an environment

  register      ; register number for a local variable
  binding-register ; register number containing the binding reference
  index         ; index number for a variable in the argument array
  closure-index ; index number for a variable in the closure context array
  environment   ; the environment for the variable, if we're compiling in
                ; a non-null lexical environment with variables

  (reads 0 :type fixnum)
  (writes 0 :type fixnum)
  references
  (references-allowed-p t) ; NIL if this is a symbol macro in the enclosing
                           ; lexical environment
  used-non-locally-p
  (compiland *current-compiland*)
  block)

(defstruct (var-ref (:constructor make-var-ref (variable)))
  ;; The variable this reference refers to. Will be NIL if the VAR-REF has been
  ;; rewritten to reference a constant value.
  variable
  ;; True if the VAR-REF has been rewritten to reference a constant value.
  constant-p
  ;; The constant value of this VAR-REF.
  constant-value)

;; obj can be a symbol or variable
;; returns variable or nil
(declaim (ftype (function (t) t) unboxed-fixnum-variable))
(defun unboxed-fixnum-variable (obj)
  (cond ((symbolp obj)
         (let ((variable (find-visible-variable obj)))
           (if (and variable
                    (eq (variable-representation variable) :int))
               variable
               nil)))
        ((variable-p obj)
         (if (eq (variable-representation obj) :int)
             obj
             nil))
        (t
         nil)))

(defvar *child-p* nil
  "True for local functions created by FLET, LABELS and (NAMED-)LAMBDA")

(defknown find-variable (symbol list) t)
(defun find-variable (name variables)
  (dolist (variable variables)
    (when (eq name (variable-name variable))
      (return variable))))

(defknown find-visible-variable (t) t)
(defun find-visible-variable (name)
  (dolist (variable *visible-variables*)
    (when (eq name (variable-name variable))
      (return variable))))

(defknown representation-size (t) (integer 0 65535))
(defun representation-size (representation)
  (ecase representation
    ((NIL :int :boolean :float :char) 1)
    ((:long :double) 2)))

(defknown allocate-register (t) (integer 0 65535))
(defun allocate-register (representation)
  (let ((register *register*))
    (incf *register* (representation-size representation))
    (setf *registers-allocated*
          (max *registers-allocated* *register*))
    register))


(defstruct local-function
  name
  definition
  compiland
  inline-expansion
  variable    ;; the variable which contains the loaded compiled function
              ;; or compiled closure
  environment ;; the environment in which the function is stored in
              ;; case of a function from an enclosing lexical environment
              ;; which itself isn't being compiled
  (references-allowed-p t) ;;whether a reference to the function CAN be captured
  (references-needed-p nil) ;;whether a reference to the function NEEDS to be
                            ;;captured, because the function name is used in a
                            ;;(function ...) form. Obviously implies
                            ;;references-allowed-p.
  )

(defvar *local-functions* ())

(defknown find-local-function (t) t)
(defun find-local-function (name)
  (dolist (local-function *local-functions* nil)
    (when (equal name (local-function-name local-function))
        (return local-function))))

(defvar *using-arg-array* nil)
(defvar *hairy-arglist-p* nil)


(defvar *block* nil
  "The innermost block applicable to the current lexical environment.")
(defvar *blocks* ()
  "The list of blocks in effect in the current lexical environment.

The top node does not need to be equal to the value of `*block*`. E.g.
when processing the bindings of a LET form, `*block*` is bound to the node
of that LET, while the block is not considered 'in effect': that only happens
until the body is being processed.")

(defstruct node
  form
  children
  (compiland *current-compiland*))
;; No need for a special constructor: nobody instantiates
;; nodes directly

(declaim (inline add-node-child))
(defun add-node-child (parent child)
  "Add a child node to the `children` list of a parent node,
if that parent belongs to the same compiland."
  (when parent
    (when (eq (node-compiland parent) *current-compiland*)
      (push child (node-children parent)))))

;; control-transferring blocks: TAGBODY, CATCH, to do: BLOCK

(defstruct (control-transferring-node (:include node))
  ;; If non-nil, the TAGBODY contains local blocks which "contaminate" the
  ;; environment, with GO forms in them which target tags in this TAGBODY
  ;; Non-nil if and only if the block doesn't modify the environment
  needs-environment-restoration
  )
;; No need for a special constructor: nobody instantiates
;; control-transferring-nodes directly

(defstruct (tagbody-node (:conc-name tagbody-)
                         (:include control-transferring-node)
                         (:constructor %make-tagbody-node ()))
  ;; True if a tag in this tagbody is the target of a non-local GO.
  non-local-go-p
  ;; Tags in the tagbody form; a list of tag structures
  tags
  ;; Contains a variable whose value uniquely identifies the
  ;; lexical scope from this block, to be used by GO
  id-variable)
(defknown make-tagbody-node () t)
(defun make-tagbody-node ()
  (let ((block (%make-tagbody-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defstruct (catch-node (:conc-name catch-)
                       (:include control-transferring-node)
                       (:constructor %make-catch-node ()))
  ;; The catch tag-form is evaluated, meaning we
  ;; have no predefined value to store here
  )
(defknown make-catch-node () t)
(defun make-catch-node ()
  (let ((block (%make-catch-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defstruct (block-node (:conc-name block-)
                       (:include control-transferring-node)
                       (:constructor %make-block-node (name)))
  name  ;; Block name
  (exit (gensym))
  target
  ;; True if there is a non-local RETURN from this block.
  non-local-return-p
  ;; Contains a variable whose value uniquely identifies the
  ;; lexical scope from this block, to be used by RETURN-FROM
  id-variable
  ;; A list of all RETURN-FROM value forms associated with this block
  return-value-forms)

(defknown make-block-node (t) t)
(defun make-block-node (name)
  (let ((block (%make-block-node name)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defstruct (jump-node (:conc-name jump-)
                      (:include node)
                      (:constructor
                       %make-jump-node (non-local-p target-block target-tag)))
  non-local-p
  target-block
  target-tag)
(defun make-jump-node (form non-local-p target-block &optional target-tag)
  (let ((node (%make-jump-node non-local-p target-block target-tag)))
    ;; Don't push into compiland blocks, as this as a node rather than a block
    (setf (node-form node) form)
    (add-node-child *block* node)
    node))


;; binding blocks: LET, LET*, FLET, LABELS, M-V-B, PROGV, LOCALLY
;;
;; Binding blocks can carry references to local (optionally special) variable bindings,
;;  contain free special bindings or both

(defstruct (binding-node (:include node))
  ;; number of the register of the saved dynamic env, or NIL if none
  environment-register
  ;; Not used for LOCALLY and FLET; LABELS uses vars to store its functions
  vars
  free-specials)
;; nobody instantiates any binding nodes directly, so there's no reason
;; to create a constructor with the approprate administration code

(defstruct (let/let*-node (:conc-name let-)
                          (:include binding-node)
                          (:constructor %make-let/let*-node ())))
(defknown make-let/let*-node () t)
(defun make-let/let*-node ()
  (let ((block (%make-let/let*-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defstruct (flet-node (:conc-name flet-)
                      (:include binding-node)
                      (:constructor %make-flet-node ())))
(defknown make-flet-node () t)
(defun make-flet-node ()
  (let ((block (%make-flet-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defstruct (labels-node (:conc-name labels-)
                        (:include binding-node)
                        (:constructor %make-labels-node ())))
(defknown make-labels-node () t)
(defun make-labels-node ()
  (let ((block (%make-labels-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defstruct (m-v-b-node (:conc-name m-v-b-)
                       (:include binding-node)
                       (:constructor %make-m-v-b-node ())))
(defknown make-m-v-b-node () t)
(defun make-m-v-b-node ()
  (let ((block (%make-m-v-b-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defstruct (progv-node (:conc-name progv-)
                       (:include binding-node)
                       (:constructor %make-progv-node ())))
(defknown make-progv-node () t)
(defun make-progv-node ()
  (let ((block (%make-progv-node)))
    (push block (compiland-blocks *current-compiland*))
    block))

(defstruct (locally-node (:conc-name locally-)
                         (:include binding-node)
                         (:constructor %make-locally-node ())))
(defknown make-locally-node () t)
(defun make-locally-node ()
  (let ((block (%make-locally-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

;; blocks requiring non-local exits: UNWIND-PROTECT, SYS:SYNCHRONIZED-ON

(defstruct (protected-node (:include node)
                           (:constructor %make-protected-node ())))
(defknown make-protected-node () t)
(defun make-protected-node ()
  (let ((block (%make-protected-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defstruct (unwind-protect-node (:conc-name unwind-protect-)
                                (:include protected-node)
                                (:constructor %make-unwind-protect-node ())))
(defknown make-unwind-protect-node () t)
(defun make-unwind-protect-node ()
  (let ((block (%make-unwind-protect-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defstruct (synchronized-node (:conc-name synchronized-)
                              (:include protected-node)
                              (:constructor %make-synchronized-node ())))
(defknown make-synchronized-node () t)
(defun make-synchronized-node ()
  (let ((block (%make-synchronized-node)))
    (push block (compiland-blocks *current-compiland*))
    (add-node-child *block* block)
    block))

(defun find-block (name)
  (dolist (block *blocks*)
    (when (and (block-node-p block)
               (eq name (block-name block)))
      (return block))))

(defun %find-enclosed-blocks (form)
  "Helper function for `find-enclosed-blocks`, implementing the actual
algorithm specified there."
  (cond
   ((node-p form) (list form))
   ((atom form) nil)
   (t
    ;; We can't use MAPCAN or DOLIST here: they'll choke on dotted lists
    (do* ((tail form (cdr tail))
          blocks)
         ((null tail) blocks)
      (setf blocks
            (nconc (%find-enclosed-blocks (if (consp tail)
                                              (car tail) tail))
                   blocks))
      (when (not (listp tail))
        (return blocks))))))

(defun find-enclosed-blocks (form)
  "Returns the immediate enclosed blocks by searching the form's subforms.

More deeply nested blocks can be reached through the `node-children`
field of the immediate enclosed blocks."
  (when *blocks*
    ;; when the innermost enclosing block doesn't have node-children,
    ;;  there's really nothing to search for.
    (let ((first-enclosing-block (car *blocks*)))
      (when (and (eq *current-compiland*
                     (node-compiland first-enclosing-block))
                 (null (node-children first-enclosing-block)))
        (return-from find-enclosed-blocks))))

  (%find-enclosed-blocks form))


(defun some-nested-block (predicate blocks)
  "Applies `predicate` recursively to the `blocks` and its children,
until predicate returns non-NIL, returning that value.

`blocks` may be a single block or a list of blocks."
  (when blocks
    (some #'(lambda (b)
              (or (funcall predicate b)
                  (some-nested-block predicate (node-children b))))
          (if (listp blocks)
              blocks
            (list blocks)))))

(defknown node-constant-p (t) boolean)
(defun node-constant-p (object)
  (cond ((node-p object)
         nil)
        ((var-ref-p object)
         nil)
        ((constantp object)
         t)
        (t
         nil)))

(defknown block-requires-non-local-exit-p (t) boolean)
(defun block-requires-non-local-exit-p (object)
  "A block which *always* requires a 'non-local-exit' is a block which
requires a transfer control exception to be thrown: e.g. Go and Return.

Non-local exits are required by blocks which do more in their cleanup
than just restore the lastSpecialBinding (= dynamic environment).
"
  (or (unwind-protect-node-p object)
      (catch-node-p object)
      (synchronized-node-p object)))

(defun node-opstack-unsafe-p (node)
  (or (when (jump-node-p node)
        (let ((target-block (jump-target-block node)))
          (and (null (jump-non-local-p node))
               (member target-block *blocks*))))
      (when (tagbody-node-p node) (tagbody-non-local-go-p node))
      (when (block-node-p node) (block-non-local-return-p node))
      (catch-node-p node)))

(defknown block-creates-runtime-bindings-p (t) boolean)
(defun block-creates-runtime-bindings-p (block)
  ;; FIXME: This may be false, if the bindings to be
  ;; created are a quoted list
  (progv-node-p block))

(defknown enclosed-by-runtime-bindings-creating-block-p (t) boolean)
(defun enclosed-by-runtime-bindings-creating-block-p (outermost-block)
  "Indicates whether the code being compiled/analyzed is enclosed in a
block which creates special bindings at runtime."
  (dolist (enclosing-block *blocks*)
    (when (eq enclosing-block outermost-block)
      (return-from enclosed-by-runtime-bindings-creating-block-p nil))
    (when (block-creates-runtime-bindings-p enclosing-block)
      (return-from enclosed-by-runtime-bindings-creating-block-p t))))

(defknown enclosed-by-protected-block-p (&optional t) boolean)
(defun enclosed-by-protected-block-p (&optional outermost-block)
  "Indicates whether the code being compiled/analyzed is enclosed in
a block which requires a non-local transfer of control exception to
be generated.
"
  (dolist (enclosing-block *blocks*)
    (when (eq enclosing-block outermost-block)
      (return-from enclosed-by-protected-block-p nil))
    (when (block-requires-non-local-exit-p enclosing-block)
      (return-from enclosed-by-protected-block-p t))))

(defknown enclosed-by-environment-setting-block-p (&optional t) boolean)
(defun enclosed-by-environment-setting-block-p (&optional outermost-block)
  (dolist (enclosing-block *blocks*)
    (when (eq enclosing-block outermost-block)
      (return nil))
    (when (and (binding-node-p enclosing-block)
               (binding-node-environment-register enclosing-block))
      (return t))))

(defknown environment-register-to-restore (&optional t) t)
(defun environment-register-to-restore (&optional outermost-block)
  "Returns the environment register which contains the
saved environment from the outermost enclosing block:

That's the one which contains the environment used in the outermost block."
  (flet ((outermost-register (last-register block)
           (when (eq block outermost-block)
             (return-from environment-register-to-restore last-register))
           (or (and (binding-node-p block)
                    (binding-node-environment-register block))
               last-register)))
    (reduce #'outermost-register *blocks*
            :initial-value nil)))

(defstruct tag
  ;; The symbol (or integer) naming the tag
  name
  ;; The symbol which is the jump target in JVM byte code
  label
  ;; The associated TAGBODY
  block
  (compiland *current-compiland*)
  used
  used-non-locally)

(defknown find-tag (t) t)
(defun find-tag (name)
  (dolist (tag *visible-tags*)
    (when (eql name (tag-name tag))
      (return tag))))

(defun process-ignore/ignorable (declaration names variables)
  (when (memq declaration '(IGNORE IGNORABLE))
    (let ((what (if (eq declaration 'IGNORE) "ignored" "ignorable")))
      (dolist (name names)
        (unless (and (consp name) (eq (car name) 'FUNCTION))
          (let ((variable (find-variable name variables)))
            (cond ((null variable)
                   (compiler-style-warn "Declaring unknown variable ~S to be ~A."
                                        name what))
                  ((variable-special-p variable)
                   (compiler-style-warn "Declaring special variable ~S to be ~A."
                                        name what))
                  ((eq declaration 'IGNORE)
                   (setf (variable-ignore-p variable) t))
                  (t
                   (setf (variable-ignorable-p variable) t)))))))))

(defun finalize-generic-functions ()
  (dolist (sym '(make-instance
                 initialize-instance
                 shared-initialize))
    (let ((gf (and (fboundp sym) (fdefinition sym))))
      (when (typep gf 'generic-function)
        (unless (compiled-function-p gf)
          (mop::finalize-generic-function gf))))))

(finalize-generic-functions)

(provide 'jvm)
