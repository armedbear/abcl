;;; format.lisp
;;;
;;; Copyright (C) 2004-2007 Peter Graves
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

;;; Adapted from CMUCL/SBCL.

(in-package "SYSTEM")

;;; From primordial-extensions.lisp.

;;; Concatenate together the names of some strings and symbols,
;;; producing a symbol in the current package.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest things)
    (let ((name (apply #'concatenate 'string (mapcar #'string things))))
      (values (intern name)))))

;;; a helper function for various macros which expect clauses of a
;;; given length, etc.
;;;
;;; Return true if X is a proper list whose length is between MIN and
;;; MAX (inclusive).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun proper-list-of-length-p (x min &optional (max min))
    ;; FIXME: This implementation will hang on circular list
    ;; structure. Since this is an error-checking utility, i.e. its
    ;; job is to deal with screwed-up input, it'd be good style to fix
    ;; it so that it can deal with circular list structure.
    (cond ((minusp max) nil)
          ((null x) (zerop min))
          ((consp x)
           (and (plusp max)
                (proper-list-of-length-p (cdr x)
                                         (if (plusp (1- min))
                                             (1- min)
                                             0)
                                         (1- max))))
          (t nil))))

;;; From early-extensions.lisp.

(defconstant form-feed-char-code 12)

(defmacro named-let (name binds &body body)
  (dolist (x binds)
    (unless (proper-list-of-length-p x 2)
      (error "malformed NAMED-LET variable spec: ~S" x)))
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

;;;; ONCE-ONLY
;;;;
;;;; "The macro ONCE-ONLY has been around for a long time on various
;;;; systems [..] if you can understand how to write and when to use
;;;; ONCE-ONLY, then you truly understand macro." -- Peter Norvig,
;;;; _Paradigms of Artificial Intelligence Programming: Case Studies
;;;; in Common Lisp_, p. 853

;;; ONCE-ONLY is a utility useful in writing source transforms and
;;; macros. It provides a concise way to wrap a LET around some code
;;; to ensure that some forms are only evaluated once.
;;;
;;; Create a LET* which evaluates each value expression, binding a
;;; temporary variable to the result, and wrapping the LET* around the
;;; result of the evaluation of BODY. Within the body, each VAR is
;;; bound to the corresponding temporary variable.
(defmacro once-only (specs &body body)
  (named-let frob ((specs specs)
		   (body body))
             (if (null specs)
                 `(progn ,@body)
                 (let ((spec (first specs)))
                   ;; FIXME: should just be DESTRUCTURING-BIND of SPEC
                   (unless (proper-list-of-length-p spec 2)
                     (error "malformed ONCE-ONLY binding spec: ~S" spec))
                   (let* ((name (first spec))
                          (exp-temp (gensym (symbol-name name))))
                     `(let ((,exp-temp ,(second spec))
                            (,name (gensym "ONCE-ONLY-")))
                        `(let ((,,name ,,exp-temp))
                           ,,(frob (rest specs) body))))))))

;;; From print.lisp.

;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does
;;; most of the work for all printing of floating point numbers in the
;;; printer and in FORMAT. It converts a floating point number to a
;;; string in a free or fixed format with no exponent. The
;;; interpretation of the arguments is as follows:
;;;
;;;     X	- The floating point number to convert, which must not be
;;;		negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;		of fraction digits to produce if the FDIGITS parameter
;;;		is unspecified or NIL. If the non-fraction digits and the
;;;		decimal point alone exceed this width, no fraction digits
;;;		will be produced unless a non-NIL value of FDIGITS has been
;;;		specified. Field overflow is not considerd an error at this
;;;		level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;		trailing zeroes may be introduced as needed. May be
;;;		unspecified or NIL, in which case as many digits as possible
;;;		are generated, subject to the constraint that there are no
;;;		trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-NIL, then the number
;;;		printed is (* x (expt 10 scale)). This scaling is exact,
;;;		and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-NIL, is the minimum
;;;		number of fraction digits which will be produced, regardless
;;;		of the value of WIDTH or FDIGITS. This feature is used by
;;;		the ~E format directive to prevent complete loss of
;;;		significance in the printed value due to a bogus choice of
;;;		scale factor.
;;;
;;; Most of the optional arguments are for the benefit for FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;		       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;		       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;		       point. Zero indicates point before first digit.
;;;
;;; NOTE: FLONUM-TO-STRING goes to a lot of trouble to guarantee
;;; accuracy. Specifically, the decimal number printed is the closest
;;; possible approximation to the true value of the binary number to
;;; be printed from among all decimal representations with the same
;;; number of digits. In free-format output, i.e. with the number of
;;; digits unconstrained, it is guaranteed that all the information is
;;; preserved, so that a properly- rounding reader can reconstruct the
;;; original binary number, bit-for-bit, from its printed decimal
;;; representation. Furthermore, only as many digits as necessary to
;;; satisfy this condition will be printed.
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.
;;; The algorithm is essentially that of algorithm Dragon4 in "How to
;;; Print Floating-Point Numbers Accurately" by Steele and White. The
;;; current (draft) version of this paper may be found in
;;; [CMUC]<steele>tradix.press. DO NOT EVEN THINK OF ATTEMPTING TO
;;; UNDERSTAND THIS CODE WITHOUT READING THE PAPER!

(defun flonum-to-string (x &optional width fdigits scale fmin)
  (declare (ignore fmin)) ; FIXME
  (cond ((zerop x)
	 ;; Zero is a special case which FLOAT-STRING cannot handle.
	 (if fdigits
	     (let ((s (make-string (1+ fdigits) :initial-element #\0)))
	       (setf (schar s 0) #\.)
	       (values s (length s) t (zerop fdigits) 0))
	     (values "." 1 t t 0)))
	(t
         (when scale
           (setf x (* x (expt 10 scale))))
         (let* ((s (float-string x))
                (length (length s))
                (index (position #\. s)))
           (when (and (< x 1)
                      (> length 0)
                      (eql (schar s 0) #\0))
             (setf s (subseq s 1)
                   length (length s)
                   index (position #\. s)))
           (when fdigits
             ;; "Leading zeros are not permitted, except that a single zero
             ;; digit is output before the decimal point if the printed value
             ;; is less than one, and this single zero digit is not output at
             ;; all if w=d+1."
             (let ((actual-fdigits (- length index 1)))
               (cond ((< actual-fdigits fdigits)
                      ;; Add the required number of trailing zeroes.
                      (setf s (concatenate 'string s
                                           (make-string (- fdigits actual-fdigits)
                                                        :initial-element #\0))
                            length (length s)))
                     ((> actual-fdigits fdigits)
                      (let* ((desired-length (+ index 1 fdigits))
                             (c (schar s desired-length)))
                        (setf s (subseq s 0 (+ index 1 fdigits))
                              length (length s)
                              index (position #\. s))
                        (when (char>= c #\5)
                          (setf s (round-up s)
                                length (length s)
                                index (position #\. s))))))))
           (when (and width (> length width))
             ;; The string is too long. Shorten it by removing insignificant
             ;; trailing zeroes if possible.
             (let ((minimum-width (+ (1+ index) (or fdigits 0))))
               (when (< minimum-width width)
                 (setf minimum-width width))
               (when (> length minimum-width)
                 ;; But we don't want to shorten e.g. "1.7d100"...
                 (when (every #'digit-char-p (subseq s (1+ index)))
                   (let ((c (schar s minimum-width)))
                     (setf s (subseq s 0 minimum-width)
                           length minimum-width)
                     (when (char>= c #\5)
                       (setf s (round-up s)
                             length (length s)
                             index (position #\. s))))))))
           (values s length (eql index 0) (eql index (1- length)) index)))))

(defun round-up (string)
  (let* ((index (position #\. string))
         (n (read-from-string (setf string (remove #\. string))))
         (s (princ-to-string (incf n))))
    (loop for char across string
      while (equal char #\0)
      do (setf s (concatenate 'string "0" s)))
    (cond ((null index)
           s)
          (t
           (when (> (length s) (length string))
             ;; Rounding up made the string longer, which means we went from (say) 99
             ;; to 100. Drop the trailing #\0 and move the #\. one character to the
             ;; right.
             (setf s (subseq s 0 (1- (length s))))
             (incf index))
           (concatenate 'string (subseq s 0 index) "." (subseq s index))))))


(defun scale-exponent (original-x)
  (let* ((x (coerce original-x 'long-float)))
    (multiple-value-bind (sig exponent) (decode-float x)
      (declare (ignore sig))
      (if (= x 0.0l0)
	  (values (float 0.0l0 original-x) 1)
	  (let* ((ex (locally (declare (optimize (safety 0)))
                       (the fixnum
                            (round (* exponent (log 2l0 10))))))
		 (x (if (minusp ex)
			(if (float-denormalized-p x)
			    (* x 1.0l16 (expt 10.0l0 (- (- ex) 16)))
			    (* x 10.0l0 (expt 10.0l0 (- (- ex) 1))))
			(/ x 10.0l0 (expt 10.0l0 (1- ex))))))
	    (do ((d 10.0l0 (* d 10.0l0))
		 (y x (/ x d))
		 (ex ex (1+ ex)))
		((< y 1.0l0)
		 (do ((m 10.0l0 (* m 10.0l0))
		      (z y (* y m))
		      (ex ex (1- ex)))
		     ((>= z 0.1l0)
		      (values (float z original-x) ex))
                   (declare (long-float m) (integer ex))))
              (declare (long-float d))))))))

(defconstant double-float-exponent-byte
  (byte 11 20))

(defun float-denormalized-p (x)
  "Return true if the double-float X is denormalized."
  (and (zerop (ldb double-float-exponent-byte (double-float-high-bits x)))
       (not (zerop x))))

;;; From early-format.lisp.

(in-package #:format)

(defparameter *format-whitespace-chars*
  (vector #\space
	  #\newline
          #\tab))

(defvar *format-directive-expanders*
  (make-hash-table :test #'eq))
(defvar *format-directive-interpreters*
  (make-hash-table :test #'eq))

(defvar *default-format-error-control-string* nil)
(defvar *default-format-error-offset* nil)

;;;; specials used to communicate information

;;; Used both by the expansion stuff and the interpreter stuff. When it is
;;; non-NIL, up-up-and-out (~:^) is allowed. Otherwise, ~:^ isn't allowed.
(defvar *up-up-and-out-allowed* nil)

;;; Used by the interpreter stuff. When it's non-NIL, it's a function
;;; that will invoke PPRINT-POP in the right lexical environemnt.
(declaim (type (or null function) *logical-block-popper*))
(defvar *logical-block-popper* nil)

;;; Used by the expander stuff. This is bindable so that ~<...~:>
;;; can change it.
(defvar *expander-next-arg-macro* 'expander-next-arg)

;;; Used by the expander stuff. Initially starts as T, and gets set to NIL
;;; if someone needs to do something strange with the arg list (like use
;;; the rest, or something).
(defvar *only-simple-args*)

;;; Used by the expander stuff. We do an initial pass with this as NIL.
;;; If someone doesn't like this, they (THROW 'NEED-ORIG-ARGS NIL) and we try
;;; again with it bound to T. If this is T, we don't try to do anything
;;; fancy with args.
(defvar *orig-args-available* nil)

;;; Used by the expander stuff. List of (symbol . offset) for simple args.
(defvar *simple-args*)

;;; From late-format.lisp.

(in-package #:format)

(define-condition format-error (error)
  ((complaint :reader format-error-complaint :initarg :complaint)
   (args :reader format-error-args :initarg :args :initform nil)
   (control-string :reader format-error-control-string
		   :initarg :control-string
		   :initform *default-format-error-control-string*)
   (offset :reader format-error-offset :initarg :offset
	   :initform *default-format-error-offset*)
   (print-banner :reader format-error-print-banner :initarg :print-banner
		 :initform t))
  (:report %print-format-error))

(defun %print-format-error (condition stream)
  (format stream
	  "~:[~;error in format: ~]~
           ~?~@[~%  ~A~%  ~V@T^~]"
	  (format-error-print-banner condition)
	  (format-error-complaint condition)
	  (format-error-args condition)
	  (format-error-control-string condition)
	  (format-error-offset condition)))

(defun missing-arg ()
  (error "Missing argument in format directive"))

(defstruct format-directive
  (string (missing-arg) :type simple-string)
  (start (missing-arg) :type (and unsigned-byte fixnum))
  (end (missing-arg) :type (and unsigned-byte fixnum))
  (character (missing-arg) :type base-char)
  (colonp nil :type (member t nil))
  (atsignp nil :type (member t nil))
  (params nil :type list))
(defmethod print-object ((x format-directive) stream)
  (print-unreadable-object (x stream)
                           (write-string (format-directive-string x)
                                         stream
                                         :start (format-directive-start x)
                                         :end (format-directive-end x))))

;;;; TOKENIZE-CONTROL-STRING

(defun tokenize-control-string (string)
  (declare (simple-string string))
  (let ((index 0)
	(end (length string))
	(result nil)
	(in-block nil)
	(pprint nil)
	(semi nil)
	(justification-semi 0))
    (declare (type index fixnum))
    (loop
      (let ((next-directive (or (position #\~ string :start index) end)))
        (declare (type index next-directive))
	(when (> next-directive index)
	  (push (subseq string index next-directive) result))
	(when (= next-directive end)
	  (return))
	(let* ((directive (parse-directive string next-directive))
	       (directive-char (format-directive-character directive)))
          (declare (type character directive-char))
	  ;; We are looking for illegal combinations of format
	  ;; directives in the control string.  See the last paragraph
	  ;; of CLHS 22.3.5.2: "an error is also signaled if the
	  ;; ~<...~:;...~> form of ~<...~> is used in the same format
	  ;; string with ~W, ~_, ~<...~:>, ~I, or ~:T."
	  (cond ((char= #\< directive-char)
		 ;; Found a justification or logical block
		 (setf in-block t))
		((and in-block (char= #\; directive-char))
		 ;; Found a semi colon in a justification or logical block
		 (setf semi t))
		((char= #\> directive-char)
		 ;; End of justification or logical block.  Figure out which.
		 (setf in-block nil)
		 (cond ((format-directive-colonp directive)
			;; A logical-block directive.  Note that fact, and also
			;; note that we don't care if we found any ~;
			;; directives in the block.
			(setf pprint t)
			(setf semi nil))
		       (semi
			;; A justification block with a ~; directive in it.
			(incf justification-semi))))
		((and (not in-block)
		      (or (and (char= #\T directive-char) (format-directive-colonp directive))
			  (char= #\W directive-char)
			  (char= #\_ directive-char)
			  (char= #\I directive-char)))
		 (setf pprint t)))
	  (push directive result)
	  (setf index (format-directive-end directive)))))
    (when (and pprint (plusp justification-semi))
      (error 'format-error
	     :complaint "A justification directive cannot be in the same format string~%~
             as ~~W, ~~I, ~~:T, or a logical-block directive."
	     :control-string string
	     :offset 0))
    (nreverse result)))

(defun parse-directive (string start)
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
	(end (length string)))
    (flet ((get-char ()
                     (if (= posn end)
                         (error 'format-error
                                :complaint "String ended before directive was found."
                                :control-string string
                                :offset start)
                         (schar string posn)))
	   (check-ordering ()
                           (when (or colonp atsignp)
                             (error 'format-error
                                    :complaint "parameters found after #\\: or #\\@ modifier"
                                    :control-string string
                                    :offset posn))))
      (loop
	(let ((char (get-char)))
	  (cond ((or (char<= #\0 char #\9) (char= char #\+) (char= char #\-))
		 (check-ordering)
		 (multiple-value-bind (param new-posn)
                   (parse-integer string :start posn :junk-allowed t)
		   (push (cons posn param) params)
		   (setf posn new-posn)
		   (case (get-char)
		     (#\,)
		     ((#\: #\@)
		      (decf posn))
		     (t
		      (return)))))
		((or (char= char #\v)
		     (char= char #\V))
		 (check-ordering)
		 (push (cons posn :arg) params)
		 (incf posn)
		 (case (get-char)
		   (#\,)
		   ((#\: #\@)
		    (decf posn))
		   (t
		    (return))))
		((char= char #\#)
		 (check-ordering)
		 (push (cons posn :remaining) params)
		 (incf posn)
		 (case (get-char)
		   (#\,)
		   ((#\: #\@)
		    (decf posn))
		   (t
		    (return))))
		((char= char #\')
		 (check-ordering)
		 (incf posn)
		 (push (cons posn (get-char)) params)
		 (incf posn)
		 (unless (char= (get-char) #\,)
		   (decf posn)))
		((char= char #\,)
		 (check-ordering)
		 (push (cons posn nil) params))
		((char= char #\:)
		 (if colonp
		     (error 'format-error
			    :complaint "too many colons supplied"
			    :control-string string
			    :offset posn)
		     (setf colonp t)))
		((char= char #\@)
		 (if atsignp
		     (error 'format-error
			    :complaint "too many #\\@ characters supplied"
			    :control-string string
			    :offset posn)
		     (setf atsignp t)))
		(t
		 (when (and (char= (schar string (1- posn)) #\,)
			    (or (< posn 2)
				(char/= (schar string (- posn 2)) #\')))
		   (check-ordering)
		   (push (cons (1- posn) nil) params))
		 (return))))
	(incf posn))
      (let ((char (get-char)))
	(when (char= char #\/)
	  (let ((closing-slash (position #\/ string :start (1+ posn))))
	    (if closing-slash
		(setf posn closing-slash)
		(error 'format-error
		       :complaint "no matching closing slash"
		       :control-string string
		       :offset posn))))
	(make-format-directive
	 :string string :start start :end (1+ posn)
	 :character (char-upcase char)
	 :colonp colonp :atsignp atsignp
	 :params (nreverse params))))))

;;;; FORMATTER stuff

(defmacro formatter (control-string)
  `#',(%formatter control-string))

(defun %formatter (control-string)
  (block nil
    (catch 'need-orig-args
      (let* ((*simple-args* nil)
	     (*only-simple-args* t)
	     (guts (expand-control-string control-string))
	     (args nil))
	(dolist (arg *simple-args*)
	  (push `(,(car arg)
		  (error
		   'format-error
		   :complaint "required argument missing"
		   :control-string ,control-string
		   :offset ,(cdr arg)))
		args))
	(return `(lambda (stream &optional ,@args &rest args)
		   ,guts
		   args))))
    (let ((*orig-args-available* t)
	  (*only-simple-args* nil))
      `(lambda (stream &rest orig-args)
	 (let ((args orig-args))
	   ,(expand-control-string control-string)
	   args)))))

(defun expand-control-string (string)
  (let* ((string (etypecase string
		   (simple-string
		    string)
		   (string
		    (coerce string 'simple-string))))
	 (*default-format-error-control-string* string)
	 (directives (tokenize-control-string string)))
    `(block nil
       ,@(expand-directive-list directives))))

(defun expand-directive-list (directives)
  (let ((results nil)
	(remaining-directives directives))
    (loop
      (unless remaining-directives
	(return))
      (multiple-value-bind (form new-directives)
        (expand-directive (car remaining-directives)
                          (cdr remaining-directives))
	(push form results)
	(setf remaining-directives new-directives)))
    (reverse results)))

(defun expand-directive (directive more-directives)
  (etypecase directive
    (format-directive
     (let ((expander
	    (gethash (format-directive-character directive)
                     *format-directive-expanders*))
	   (*default-format-error-offset*
	    (1- (format-directive-end directive))))
       (declare (type (or null function) expander))
       (if expander
	   (funcall expander directive more-directives)
	   (error 'format-error
		  :complaint "unknown directive ~@[(character: ~A)~]"
		  :args (list (char-name (format-directive-character directive)))))))
    (simple-string
     (values `(write-string ,directive stream)
	     more-directives))))

(defmacro expander-next-arg (string offset)
  `(if args
       (pop args)
       (error 'format-error
	      :complaint "no more arguments"
	      :control-string ,string
	      :offset ,offset)))

(defun expand-next-arg (&optional offset)
  (if (or *orig-args-available* (not *only-simple-args*))
      `(,*expander-next-arg-macro*
	,*default-format-error-control-string*
	,(or offset *default-format-error-offset*))
      (let ((symbol (gensym "FORMAT-ARG-")))
	(push (cons symbol (or offset *default-format-error-offset*))
	      *simple-args*)
	symbol)))

(defmacro expand-bind-defaults (specs params &body body)
  (sys::once-only ((params params))
                  (if specs
                      (collect ((expander-bindings) (runtime-bindings))
                               (dolist (spec specs)
                                 (destructuring-bind (var default) spec
                                                     (let ((symbol (gensym)))
                                                       (expander-bindings
                                                        `(,var ',symbol))
                                                       (runtime-bindings
                                                        `(list ',symbol
                                                               (let* ((param-and-offset (pop ,params))
                                                                      (offset (car param-and-offset))
                                                                      (param (cdr param-and-offset)))
                                                                 (case param
                                                                   (:arg `(or ,(expand-next-arg offset)
                                                                              ,,default))
                                                                   (:remaining
                                                                    (setf *only-simple-args* nil)
                                                                    '(length args))
                                                                   ((nil) ,default)
                                                                   (t param))))))))
                               `(let ,(expander-bindings)
                                  `(let ,(list ,@(runtime-bindings))
                                     ,@(if ,params
                                           (error
                                            'format-error
                                            :complaint
                                            "too many parameters, expected no more than ~W"
                                            :args (list ,(length specs))
                                            :offset (caar ,params)))
                                     ,,@body)))
                      `(progn
                         (when ,params
                           (error 'format-error
                                  :complaint "too many parameters, expected none"
                                  :offset (caar ,params)))
                         ,@body))))

;;;; format directive machinery

;;; FIXME: only used in this file, could be SB!XC:DEFMACRO in EVAL-WHEN
(defmacro def-complex-format-directive (char lambda-list &body body)
  (let ((defun-name
          (intern (concatenate 'string
                               (let ((name (char-name char)))
                                 (cond (name
                                        (string-capitalize name))
                                       (t
                                        (string char))))
                               "-FORMAT-DIRECTIVE-EXPANDER")))
	(directive (gensym))
	(directives (if lambda-list (car (last lambda-list)) (gensym))))
    `(progn
       (defun ,defun-name (,directive ,directives)
	 ,@(if lambda-list
	       `((let ,(mapcar (lambda (var)
				 `(,var
				   (,(sys::symbolicate "FORMAT-DIRECTIVE-" var)
				    ,directive)))
			       (butlast lambda-list))
		   ,@body))
	       `((declare (ignore ,directive ,directives))
		 ,@body)))
       (%set-format-directive-expander ,char #',defun-name))))

;;; FIXME: only used in this file, could be SB!XC:DEFMACRO in EVAL-WHEN
(defmacro def-format-directive (char lambda-list &body body)
  (let ((directives (gensym))
	(declarations nil)
	(body-without-decls body))
    (loop
      (let ((form (car body-without-decls)))
	(unless (and (consp form) (eq (car form) 'declare))
	  (return))
	(push (pop body-without-decls) declarations)))
    (setf declarations (reverse declarations))
    `(def-complex-format-directive ,char (,@lambda-list ,directives)
       ,@declarations
       (values (progn ,@body-without-decls)
	       ,directives))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun %set-format-directive-expander (char fn)
    (setf (gethash (char-upcase char) *format-directive-expanders*) fn)
    char)

  (defun %set-format-directive-interpreter (char fn)
    (setf (gethash (char-upcase char) *format-directive-interpreters*) fn)
    char)

  (defun find-directive (directives kind stop-at-semi)
    (if directives
        (let ((next (car directives)))
          (if (format-directive-p next)
              (let ((char (format-directive-character next)))
                (if (or (char= kind char)
                        (and stop-at-semi (char= char #\;)))
                    (car directives)
                    (find-directive
                     (cdr (flet ((after (char)
                                   (member (find-directive (cdr directives)
                                                           char
                                                           nil)
                                           directives)))
                            (case char
                              (#\( (after #\)))
                              (#\< (after #\>))
                              (#\[ (after #\]))
                              (#\{ (after #\}))
                              (t directives))))
                     kind stop-at-semi)))
              (find-directive (cdr directives) kind stop-at-semi)))))

  ) ; EVAL-WHEN

;;;; format directives for simple output

(def-format-directive #\A (colonp atsignp params)
  (if params
      (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
			     (padchar #\space))
                            params
                            `(format-princ stream ,(expand-next-arg) ',colonp ',atsignp
                                           ,mincol ,colinc ,minpad ,padchar))
      `(princ ,(if colonp
		   `(or ,(expand-next-arg) "()")
		   (expand-next-arg))
	      stream)))

(def-format-directive #\S (colonp atsignp params)
  (cond (params
	 (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
				(padchar #\space))
                               params
                               `(format-prin1 stream ,(expand-next-arg) ,colonp ,atsignp
                                              ,mincol ,colinc ,minpad ,padchar)))
	(colonp
	 `(let ((arg ,(expand-next-arg)))
	    (if arg
		(prin1 arg stream)
		(princ "()" stream))))
	(t
	 `(prin1 ,(expand-next-arg) stream))))

(def-format-directive #\C (colonp atsignp params)
  (expand-bind-defaults () params
                        (if colonp
                            `(format-print-named-character ,(expand-next-arg) stream)
                            (if atsignp
                                `(prin1 ,(expand-next-arg) stream)
                                `(write-char ,(expand-next-arg) stream)))))

(def-format-directive #\W (colonp atsignp params)
  (expand-bind-defaults () params
                        (if (or colonp atsignp)
                            `(let (,@(when colonp
                                       '((*print-pretty* t)))
                                   ,@(when atsignp
                                       '((*print-level* nil)
                                         (*print-length* nil))))
                               (sys::output-object ,(expand-next-arg) stream))
                            `(sys::output-object ,(expand-next-arg) stream))))

;;;; format directives for integer output

(defun expand-format-integer (base colonp atsignp params)
  (if (or colonp atsignp params)
      (expand-bind-defaults
       ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
       params
       `(format-print-integer stream ,(expand-next-arg) ,colonp ,atsignp
                              ,base ,mincol ,padchar ,commachar
                              ,commainterval))
      `(write ,(expand-next-arg) :stream stream :base ,base :radix nil
	      :escape nil)))

(def-format-directive #\D (colonp atsignp params)
  (expand-format-integer 10 colonp atsignp params))

(def-format-directive #\B (colonp atsignp params)
  (expand-format-integer 2 colonp atsignp params))

(def-format-directive #\O (colonp atsignp params)
  (expand-format-integer 8 colonp atsignp params))

(def-format-directive #\X (colonp atsignp params)
  (expand-format-integer 16 colonp atsignp params))

(def-format-directive #\R (colonp atsignp params)
  (expand-bind-defaults
   ((base nil) (mincol 0) (padchar #\space) (commachar #\,)
    (commainterval 3))
   params
   (let ((n-arg (gensym)))
     `(let ((,n-arg ,(expand-next-arg)))
        (if ,base
            (format-print-integer stream ,n-arg ,colonp ,atsignp
                                  ,base ,mincol
                                  ,padchar ,commachar ,commainterval)
            ,(if atsignp
                 (if colonp
                     `(format-print-old-roman stream ,n-arg)
                     `(format-print-roman stream ,n-arg))
                 (if colonp
                     `(format-print-ordinal stream ,n-arg)
                     `(format-print-cardinal stream ,n-arg))))))))

;;;; format directive for pluralization

(def-format-directive #\P (colonp atsignp params end)
  (expand-bind-defaults () params
                        (let ((arg (cond
                                    ((not colonp)
                                     (expand-next-arg))
                                    (*orig-args-available*
                                     `(if (eq orig-args args)
                                          (error 'format-error
                                                 :complaint "no previous argument"
                                                 :offset ,(1- end))
                                          (do ((arg-ptr orig-args (cdr arg-ptr)))
                                              ((eq (cdr arg-ptr) args)
                                               (car arg-ptr)))))
                                    (*only-simple-args*
                                     (unless *simple-args*
                                       (error 'format-error
                                              :complaint "no previous argument"))
                                     (caar *simple-args*))
                                    (t
                                     (throw 'need-orig-args nil)))))
                          (if atsignp
                              `(write-string (if (eql ,arg 1) "y" "ies") stream)
                              `(unless (eql ,arg 1) (write-char #\s stream))))))

;;;; format directives for floating point output

(def-format-directive #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "The colon modifier cannot be used with this directive."))
  (expand-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space)) params
                        `(format-fixed stream ,(expand-next-arg) ,w ,d ,k ,ovf ,pad ,atsignp)))

(def-format-directive #\E (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "The colon modifier cannot be used with this directive."))
  (expand-bind-defaults
   ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
   params
   `(format-exponential stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark
                        ,atsignp)))

(def-format-directive #\G (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "The colon modifier cannot be used with this directive."))
  (expand-bind-defaults
   ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (mark nil))
   params
   `(format-general stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark ,atsignp)))

(def-format-directive #\$ (colonp atsignp params)
  (expand-bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
                        `(format-dollars stream ,(expand-next-arg) ,d ,n ,w ,pad ,colonp
                                         ,atsignp)))

;;;; format directives for line/page breaks etc.

(def-format-directive #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "The colon and atsign modifiers cannot be used with this directive."
	   ))
  (if params
      (expand-bind-defaults ((count 1)) params
                            `(dotimes (i ,count)
                               (terpri stream)))
      '(terpri stream)))

(def-format-directive #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "The colon and atsign modifiers cannot be used with this directive."
	   ))
  (if params
      (expand-bind-defaults ((count 1)) params
                            `(progn
                               (fresh-line stream)
                               (dotimes (i (1- ,count))
                                 (terpri stream))))
      '(fresh-line stream)))

(def-format-directive #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "The colon and atsign modifiers cannot be used with this directive."
	   ))
  (if params
      (expand-bind-defaults ((count 1)) params
                            `(dotimes (i ,count)
                               (write-char (code-char sys::form-feed-char-code) stream)))
      '(write-char (code-char sys::form-feed-char-code) stream)))

(def-format-directive #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "The colon and atsign modifiers cannot be used with this directive."
	   ))
  (if params
      (expand-bind-defaults ((count 1)) params
                            `(dotimes (i ,count)
                               (write-char #\~ stream)))
      '(write-char #\~ stream)))

(def-complex-format-directive #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
	   :complaint "both colon and atsign modifiers used simultaneously"))
  (values (expand-bind-defaults () params
                                (if atsignp
                                    '(write-char #\newline stream)
                                    nil))
	  (if (and (not colonp)
		   directives
		   (simple-string-p (car directives)))
	      (cons (string-left-trim *format-whitespace-chars*
				      (car directives))
		    (cdr directives))
	      directives)))

;;;; format directives for tabs and simple pretty printing

(def-format-directive #\T (colonp atsignp params)
  (if colonp
      (expand-bind-defaults ((n 1) (m 1)) params
                            `(pprint-tab ,(if atsignp :section-relative :section)
                                         ,n ,m stream))
      (if atsignp
	  (expand-bind-defaults ((colrel 1) (colinc 1)) params
                                `(format-relative-tab stream ,colrel ,colinc))
	  (expand-bind-defaults ((colnum 1) (colinc 1)) params
                                `(format-absolute-tab stream ,colnum ,colinc)))))

(def-format-directive #\_ (colonp atsignp params)
  (expand-bind-defaults () params
                        `(pprint-newline ,(if colonp
                                              (if atsignp
                                                  :mandatory
                                                  :fill)
                                              (if atsignp
                                                  :miser
                                                  :linear))
                                         stream)))

(def-format-directive #\I (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint
	   "cannot use the at-sign modifier with this directive"))
  (expand-bind-defaults ((n 0)) params
                        `(pprint-indent ,(if colonp :current :block) ,n stream)))

;;;; format directive for ~*

(def-format-directive #\* (colonp atsignp params end)
  (if atsignp
      (if colonp
	  (error 'format-error
		 :complaint
		 "both colon and atsign modifiers used simultaneously")
	  (expand-bind-defaults ((posn 0)) params
                                (unless *orig-args-available*
                                  (throw 'need-orig-args nil))
                                `(if (<= 0 ,posn (length orig-args))
                                     (setf args (nthcdr ,posn orig-args))
                                     (error 'format-error
                                            :complaint "Index ~W out of bounds. Should have been ~
                                            between 0 and ~W."
                                            :args (list ,posn (length orig-args))
                                            :offset ,(1- end)))))
      (if colonp
	  (expand-bind-defaults ((n 1)) params
                                (unless *orig-args-available*
                                  (throw 'need-orig-args nil))
                                `(do ((cur-posn 0 (1+ cur-posn))
                                      (arg-ptr orig-args (cdr arg-ptr)))
                                     ((eq arg-ptr args)
                                      (let ((new-posn (- cur-posn ,n)))
                                        (if (<= 0 new-posn (length orig-args))
                                            (setf args (nthcdr new-posn orig-args))
                                            (error 'format-error
                                                   :complaint
                                                   "Index ~W is out of bounds; should have been ~
                                                    between 0 and ~W."
                                                   :args (list new-posn (length orig-args))
                                                   :offset ,(1- end)))))))
	  (if params
	      (expand-bind-defaults ((n 1)) params
                                    (setf *only-simple-args* nil)
                                    `(dotimes (i ,n)
                                       ,(expand-next-arg)))
	      (expand-next-arg)))))

;;;; format directive for indirection

(def-format-directive #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error
	   :complaint "cannot use the colon modifier with this directive"))
  (expand-bind-defaults () params
                        `(handler-bind
                           ((format-error
                             (lambda (condition)
                               (error 'format-error
                                      :complaint
                                      "~A~%while processing indirect format string:"
                                      :args (list condition)
                                      :print-banner nil
                                      :control-string ,string
                                      :offset ,(1- end)))))
                           ,(if atsignp
                                (if *orig-args-available*
                                    `(setf args (%format stream ,(expand-next-arg) orig-args args))
                                    (throw 'need-orig-args nil))
                                `(%format stream ,(expand-next-arg) ,(expand-next-arg))))))

;;;; format directives for capitalization

(def-complex-format-directive #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
	     :complaint "no corresponding close parenthesis"))
    (let* ((posn (position close directives))
	   (before (subseq directives 0 posn))
	   (after (nthcdr (1+ posn) directives)))
      (values
       (expand-bind-defaults () params
                             `(let ((stream (sys::make-case-frob-stream (if (typep stream 'xp::xp-structure)
                                                                             (xp::base-stream stream)
                                                                             stream)
                                                                        ,(if colonp
                                                                             (if atsignp
                                                                                 :upcase
                                                                                 :capitalize)
                                                                             (if atsignp
                                                                                 :capitalize-first
                                                                                 :downcase)))))
                                ,@(expand-directive-list before)))
       after))))

(def-complex-format-directive #\) ()
  (error 'format-error
	 :complaint "no corresponding open parenthesis"))

;;;; format directives and support functions for conditionalization

(def-complex-format-directive #\[ (colonp atsignp params directives)
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (values
     (if atsignp
	 (if colonp
	     (error 'format-error
		    :complaint
		    "both colon and atsign modifiers used simultaneously")
	     (if (cdr sublists)
		 (error 'format-error
			:complaint
			"Can only specify one section")
		 (expand-bind-defaults () params
                   (expand-maybe-conditional (car sublists)))))
	 (if colonp
	     (if (= (length sublists) 2)
		 (expand-bind-defaults () params
                   (expand-true-false-conditional (car sublists)
                                                  (cadr sublists)))
		 (error 'format-error
			:complaint
			"must specify exactly two sections"))
	     (expand-bind-defaults ((index nil)) params
               (setf *only-simple-args* nil)
               (let ((clauses nil)
                     (case `(or ,index ,(expand-next-arg))))
                 (when last-semi-with-colon-p
                   (push `(t ,@(expand-directive-list (pop sublists)))
                         clauses))
                 (let ((count (length sublists)))
                   (dolist (sublist sublists)
                     (push `(,(decf count)
                             ,@(expand-directive-list sublist))
                           clauses)))
                 `(case ,case ,@clauses)))))
     remaining)))

(defun parse-conditional-directive (directives)
  (let ((sublists nil)
	(last-semi-with-colon-p nil)
	(remaining directives))
    (loop
      (let ((close-or-semi (find-directive remaining #\] t)))
	(unless close-or-semi
	  (error 'format-error
		 :complaint "no corresponding close bracket"))
	(let ((posn (position close-or-semi remaining)))
	  (push (subseq remaining 0 posn) sublists)
	  (setf remaining (nthcdr (1+ posn) remaining))
	  (when (char= (format-directive-character close-or-semi) #\])
	    (return))
	  (setf last-semi-with-colon-p
		(format-directive-colonp close-or-semi)))))
    (values sublists last-semi-with-colon-p remaining)))

(defun expand-maybe-conditional (sublist)
  (flet ((hairy ()
           `(let ((prev-args args)
                  (arg ,(expand-next-arg)))
              (when arg
                (setf args prev-args)
                ,@(expand-directive-list sublist)))))
    (if *only-simple-args*
	(multiple-value-bind (guts new-args)
            (let ((*simple-args* *simple-args*))
              (values (expand-directive-list sublist)
                      *simple-args*))
	  (cond ((and new-args (eq *simple-args* (cdr new-args)))
		 (setf *simple-args* new-args)
		 `(when ,(caar new-args)
		    ,@guts))
		(t
		 (setf *only-simple-args* nil)
		 (hairy))))
	(hairy))))

(defun expand-true-false-conditional (true false)
  (let ((arg (expand-next-arg)))
    (flet ((hairy ()
             `(if ,arg
                  (progn
                    ,@(expand-directive-list true))
                  (progn
                    ,@(expand-directive-list false)))))
      (if *only-simple-args*
	  (multiple-value-bind (true-guts true-args true-simple)
            (let ((*simple-args* *simple-args*)
                  (*only-simple-args* t))
              (values (expand-directive-list true)
                      *simple-args*
                      *only-simple-args*))
	    (multiple-value-bind (false-guts false-args false-simple)
              (let ((*simple-args* *simple-args*)
                    (*only-simple-args* t))
                (values (expand-directive-list false)
                        *simple-args*
                        *only-simple-args*))
	      (if (= (length true-args) (length false-args))
		  `(if ,arg
		       (progn
			 ,@true-guts)
		       ,(do ((false false-args (cdr false))
			     (true true-args (cdr true))
			     (bindings nil (cons `(,(caar false) ,(caar true))
						 bindings)))
			    ((eq true *simple-args*)
			     (setf *simple-args* true-args)
			     (setf *only-simple-args*
				   (and true-simple false-simple))
			     (if bindings
				 `(let ,bindings
				    ,@false-guts)
				 `(progn
				    ,@false-guts)))))
		  (progn
		    (setf *only-simple-args* nil)
		    (hairy)))))
	  (hairy)))))

(def-complex-format-directive #\; ()
  (error 'format-error
	 :complaint
	 "~~; directive not contained within either ~~[...~~] or ~~<...~~>"))

(def-complex-format-directive #\] ()
  (error 'format-error
	 :complaint
	 "no corresponding open bracket"))

;;;; format directive for up-and-out

(def-format-directive #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint "cannot use the at-sign modifier with this directive"))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
	   :complaint "attempt to use ~~:^ outside a ~~:{...~~} construct"))
  `(when ,(expand-bind-defaults ((arg1 nil) (arg2 nil) (arg3 nil)) params
                                `(cond (,arg3 (<= ,arg1 ,arg2 ,arg3))
                                       (,arg2 (eql ,arg1 ,arg2))
                                       (,arg1 (eql ,arg1 0))
                                       (t ,(if colonp
                                               '(null outside-args)
                                               (progn
                                                 (setf *only-simple-args* nil)
                                                 '(null args))))))
     ,(if colonp
	  '(return-from outside-loop nil)
	  '(return))))

;;;; format directives for iteration

(def-complex-format-directive #\{ (colonp atsignp params string end directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
	     :complaint "no corresponding close brace"))
    (let* ((closed-with-colon (format-directive-colonp close))
	   (posn (position close directives)))
      (labels
        ((compute-insides ()
           (if (zerop posn)
               (if *orig-args-available*
                   `((handler-bind
                       ((format-error
                         (lambda (condition)
                           (error 'format-error
                                  :complaint
                                  "~A~%while processing indirect format string:"
                                  :args (list condition)
                                  :print-banner nil
                                  :control-string ,string
                                  :offset ,(1- end)))))
                       (setf args
                             (%format stream inside-string orig-args args))))
                   (throw 'need-orig-args nil))
               (let ((*up-up-and-out-allowed* colonp))
                 (expand-directive-list (subseq directives 0 posn)))))
         (compute-loop (count)
           (when atsignp
             (setf *only-simple-args* nil))
           `(loop
              ,@(unless closed-with-colon
                  '((when (null args)
                      (return))))
              ,@(when count
                  `((when (and ,count (minusp (decf ,count)))
                      (return))))
              ,@(if colonp
                    (let ((*expander-next-arg-macro* 'expander-next-arg)
                          (*only-simple-args* nil)
                          (*orig-args-available* t))
                      `((let* ((orig-args ,(expand-next-arg))
                               (outside-args args)
                               (args orig-args))
                          (declare (ignorable orig-args outside-args args))
                          (block nil
                            ,@(compute-insides)))))
                    (compute-insides))
              ,@(when closed-with-colon
                  '((when (null args)
                      (return))))))
         (compute-block (count)
           (if colonp
               `(block outside-loop
                  ,(compute-loop count))
               (compute-loop count)))
         (compute-bindings (count)
            (if atsignp
                (compute-block count)
                `(let* ((orig-args ,(expand-next-arg))
                        (args orig-args))
                   (declare (ignorable orig-args args))
                   ,(let ((*expander-next-arg-macro* 'expander-next-arg)
                          (*only-simple-args* nil)
                          (*orig-args-available* t))
                      (compute-block count))))))
	(values (if params
                    (expand-bind-defaults ((count nil)) params
                                          (if (zerop posn)
                                              `(let ((inside-string ,(expand-next-arg)))
                                                 ,(compute-bindings count))
                                              (compute-bindings count)))
                    (if (zerop posn)
                        `(let ((inside-string ,(expand-next-arg)))
                           ,(compute-bindings nil))
                        (compute-bindings nil)))
		(nthcdr (1+ posn) directives))))))

(def-complex-format-directive #\} ()
  (error 'format-error
	 :complaint "no corresponding open brace"))

;;;; format directives and support functions for justification

(defparameter *illegal-inside-justification*
  (mapcar (lambda (x) (parse-directive x 0))
	  '("~W" "~:W" "~@W" "~:@W"
	    "~_" "~:_" "~@_" "~:@_"
	    "~:>" "~:@>"
	    "~I" "~:I" "~@I" "~:@I"
	    "~:T" "~:@T")))

(defun illegal-inside-justification-p (directive)
  (member directive *illegal-inside-justification*
	  :test (lambda (x y)
		  (and (format-directive-p x)
		       (format-directive-p y)
		       (eql (format-directive-character x) (format-directive-character y))
		       (eql (format-directive-colonp x) (format-directive-colonp y))
		       (eql (format-directive-atsignp x) (format-directive-atsignp y))))))

(def-complex-format-directive #\< (colonp atsignp params string end directives)
  (multiple-value-bind (segments first-semi close remaining)
    (parse-format-justification directives)
    (values
     (if (format-directive-colonp close)
	 (multiple-value-bind (prefix per-line-p insides suffix)
           (parse-format-logical-block segments colonp first-semi
                                       close params string end)
	   (expand-format-logical-block prefix per-line-p insides
					suffix atsignp))
	 (let ((count (reduce #'+ (mapcar (lambda (x) (count-if #'illegal-inside-justification-p x)) segments))))
	   (when (> count 0)
	     ;; ANSI specifies that "an error is signalled" in this
	     ;; situation.
	     (error 'format-error
		    :complaint "~D illegal directive~:P found inside justification block"
		    :args (list count)))
	   (expand-format-justification segments colonp atsignp
                                        first-semi params)))
     remaining)))

(def-complex-format-directive #\> ()
  (error 'format-error
	 :complaint "no corresponding open bracket"))

(defun parse-format-logical-block
  (segments colonp first-semi close params string end)
  (when params
    (error 'format-error
	   :complaint "No parameters can be supplied with ~~<...~~:>."
	   :offset (caar params)))
  (multiple-value-bind (prefix insides suffix)
    (multiple-value-bind (prefix-default suffix-default)
      (if colonp (values "(" ")") (values "" ""))
      (flet ((extract-string (list prefix-p)
                             (let ((directive (find-if #'format-directive-p list)))
                               (if directive
                                   (error 'format-error
                                          :complaint
                                          "cannot include format directives inside the ~
                                           ~:[suffix~;prefix~] segment of ~~<...~~:>"
                                          :args (list prefix-p)
                                          :offset (1- (format-directive-end directive)))
                                   (apply #'concatenate 'string list)))))
	(case (length segments)
	  (0 (values prefix-default nil suffix-default))
	  (1 (values prefix-default (car segments) suffix-default))
	  (2 (values (extract-string (car segments) t)
		     (cadr segments) suffix-default))
	  (3 (values (extract-string (car segments) t)
		     (cadr segments)
		     (extract-string (caddr segments) nil)))
	  (t
	   (error 'format-error
		  :complaint "too many segments for ~~<...~~:>")))))
    (when (format-directive-atsignp close)
      (setf insides
	    (add-fill-style-newlines insides
				     string
				     (if first-semi
					 (format-directive-end first-semi)
					 end))))
    (values prefix
	    (and first-semi (format-directive-atsignp first-semi))
	    insides
	    suffix)))

(defun add-fill-style-newlines (list string offset &optional last-directive)
  (cond
   (list
    (let ((directive (car list)))
      (cond
       ((simple-string-p directive)
        (let* ((non-space (position #\Space directive :test #'char/=))
               (newlinep (and last-directive
                              (char=
                               (format-directive-character last-directive)
                               #\Newline))))
          (cond
           ((and newlinep non-space)
            (nconc
             (list (subseq directive 0 non-space))
             (add-fill-style-newlines-aux
              (subseq directive non-space) string (+ offset non-space))
             (add-fill-style-newlines
              (cdr list) string (+ offset (length directive)))))
           (newlinep
            (cons directive
                  (add-fill-style-newlines
                   (cdr list) string (+ offset (length directive)))))
           (t
            (nconc (add-fill-style-newlines-aux directive string offset)
                   (add-fill-style-newlines
                    (cdr list) string (+ offset (length directive))))))))
       (t
        (cons directive
              (add-fill-style-newlines
               (cdr list) string
               (format-directive-end directive) directive))))))
   (t nil)))

(defun add-fill-style-newlines-aux (literal string offset)
  (let ((end (length literal))
	(posn 0))
    (collect ((results))
             (loop
               (let ((blank (position #\space literal :start posn)))
                 (when (null blank)
                   (results (subseq literal posn))
                   (return))
                 (let ((non-blank (or (position #\space literal :start blank
                                                :test #'char/=)
                                      end)))
                   (results (subseq literal posn non-blank))
                   (results (make-format-directive
                             :string string :character #\_
                             :start (+ offset non-blank) :end (+ offset non-blank)
                             :colonp t :atsignp nil :params nil))
                   (setf posn non-blank))
                 (when (= posn end)
                   (return))))
             (results))))

(defun parse-format-justification (directives)
  (let ((first-semi nil)
	(close nil)
	(remaining directives))
    (collect ((segments))
             (loop
               (let ((close-or-semi (find-directive remaining #\> t)))
                 (unless close-or-semi
                   (error 'format-error
                          :complaint "no corresponding close bracket"))
                 (let ((posn (position close-or-semi remaining)))
                   (segments (subseq remaining 0 posn))
                   (setf remaining (nthcdr (1+ posn) remaining)))
                 (when (char= (format-directive-character close-or-semi)
                              #\>)
                   (setf close close-or-semi)
                   (return))
                 (unless first-semi
                   (setf first-semi close-or-semi))))
             (values (segments) first-semi close remaining))))

(defmacro expander-pprint-next-arg (string offset)
  `(progn
     (when (null args)
       (error 'format-error
	      :complaint "no more arguments"
	      :control-string ,string
	      :offset ,offset))
     (pprint-pop)
     (pop args)))

(defun expand-format-logical-block (prefix per-line-p insides suffix atsignp)
  `(let ((arg ,(if atsignp 'args (expand-next-arg))))
     ,@(when atsignp
	 (setf *only-simple-args* nil)
	 '((setf args nil)))
     (pprint-logical-block
      (stream arg
              ,(if per-line-p :per-line-prefix :prefix) ,prefix
              :suffix ,suffix)
      (let ((args arg)
            ,@(unless atsignp
                `((orig-args arg))))
        (declare (ignorable args ,@(unless atsignp '(orig-args))))
        (block nil
          ,@(let ((*expander-next-arg-macro* 'expander-pprint-next-arg)
                  (*only-simple-args* nil)
                  (*orig-args-available*
                   (if atsignp *orig-args-available* t)))
              (expand-directive-list insides)))))))

(defun expand-format-justification (segments colonp atsignp first-semi params)
  (let ((newline-segment-p
	 (and first-semi
	      (format-directive-colonp first-semi))))
    (expand-bind-defaults
     ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
     params
     `(let ((segments nil)
            ,@(when newline-segment-p
                '((newline-segment nil)
                  (extra-space 0)
                  (line-len 72))))
        (block nil
          ,@(when newline-segment-p
              `((setf newline-segment
                      (with-output-to-string (stream)
                        ,@(expand-directive-list (pop segments))))
                ,(expand-bind-defaults
                  ((extra 0)
                   (line-len '(or #-abcl(sb!impl::line-length stream) 72)))
                  (format-directive-params first-semi)
                  `(setf extra-space ,extra line-len ,line-len))))
          ,@(mapcar (lambda (segment)
                      `(push (with-output-to-string (stream)
                               ,@(expand-directive-list segment))
                             segments))
                    segments))
        (format-justification stream
                              ,@(if newline-segment-p
                                    '(newline-segment extra-space line-len)
                                    '(nil 0 0))
                              segments ,colonp ,atsignp
                              ,mincol ,colinc ,minpad ,padchar)))))

;;;; format directive and support function for user-defined method

(def-format-directive #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-fun-name string start end)))
    (collect ((param-names) (bindings))
             (dolist (param-and-offset params)
               (let ((param (cdr param-and-offset)))
                 (let ((param-name (gensym)))
                   (param-names param-name)
                   (bindings `(,param-name
                               ,(case param
                                  (:arg (expand-next-arg))
                                  (:remaining '(length args))
                                  (t param)))))))
             `(let ,(bindings)
                (,symbol stream ,(expand-next-arg) ,colonp ,atsignp
                 ,@(param-names))))))

(defun extract-user-fun-name (string start end)
  (let ((slash (position #\/ string :start start :end (1- end)
			 :from-end t)))
    (unless slash
      (error 'format-error
	     :complaint "malformed ~~/ directive"))
    (let* ((name (string-upcase (let ((foo string))
				  ;; Hack alert: This is to keep the compiler
				  ;; quiet about deleting code inside the
				  ;; subseq expansion.
				  (subseq foo (1+ slash) (1- end)))))
	   (first-colon (position #\: name))
	   (second-colon (if first-colon (position #\: name :start (1+ first-colon))))
	   (package-name (if first-colon
			     (subseq name 0 first-colon)
			     "COMMON-LISP-USER"))
	   (package (find-package package-name)))
      (unless package
	;; FIXME: should be PACKAGE-ERROR? Could we just use
	;; FIND-UNDELETED-PACKAGE-OR-LOSE?
	(error 'format-error
	       :complaint "no package named ~S"
	       :args (list package-name)))
      (intern (cond
               ((and second-colon (= second-colon (1+ first-colon)))
                (subseq name (1+ second-colon)))
               (first-colon
                (subseq name (1+ first-colon)))
               (t name))
	      package))))

;;; compile-time checking for argument mismatch.  This code is
;;; inspired by that of Gerd Moellmann, and comes decorated with
;;; FIXMEs:
(defun %compiler-walk-format-string (string args)
  (declare (type simple-string string))
  (let ((*default-format-error-control-string* string))
    (macrolet ((incf-both (&optional (increment 1))
                          `(progn
                             (incf min ,increment)
                             (incf max ,increment)))
	       (walk-complex-directive (function)
                                       `(multiple-value-bind (min-inc max-inc remaining)
                                          (,function directive directives args)
                                          (incf min min-inc)
                                          (incf max max-inc)
                                          (setq directives remaining))))
      ;; FIXME: these functions take a list of arguments as well as
      ;; the directive stream.  This is to enable possibly some
      ;; limited type checking on FORMAT's arguments, as well as
      ;; simple argument count mismatch checking: when the minimum and
      ;; maximum argument counts are the same at a given point, we
      ;; know which argument is going to be used for a given
      ;; directive, and some (annotated below) require arguments of
      ;; particular types.
      (labels
        ((walk-justification (justification directives args)
                             (declare (ignore args))
                             (let ((*default-format-error-offset*
                                    (1- (format-directive-end justification))))
                               (multiple-value-bind (segments first-semi close remaining)
                                 (parse-format-justification directives)
                                 (declare (ignore segments first-semi))
                                 (cond
                                  ((not (format-directive-colonp close))
                                   (values 0 0 directives))
                                  ((format-directive-atsignp justification)
                                   (values 0 call-arguments-limit directives))
                                  ;; FIXME: here we could assert that the
                                  ;; corresponding argument was a list.
                                  (t (values 1 1 remaining))))))
         (walk-conditional (conditional directives args)
                           (let ((*default-format-error-offset*
                                  (1- (format-directive-end conditional))))
                             (multiple-value-bind (sublists last-semi-with-colon-p remaining)
                               (parse-conditional-directive directives)
                               (declare (ignore last-semi-with-colon-p))
                               (let ((sub-max
                                      (loop for s in sublists
                                        maximize (nth-value
                                                  1 (walk-directive-list s args)))))
                                 (cond
                                  ((format-directive-atsignp conditional)
                                   (values 1 (max 1 sub-max) remaining))
                                  ((loop for p in (format-directive-params conditional)
                                     thereis (or (integerp (cdr p))
                                                 (memq (cdr p) '(:remaining :arg))))
                                   (values 0 sub-max remaining))
                                  ;; FIXME: if not COLONP, then the next argument
                                  ;; must be a number.
                                  (t (values 1 (1+ sub-max) remaining)))))))
         (walk-iteration (iteration directives args)
                         (declare (ignore args))
                         (let ((*default-format-error-offset*
                                (1- (format-directive-end iteration))))
                           (let* ((close (find-directive directives #\} nil))
                                  (posn (or (position close directives)
                                            (error 'format-error
                                                   :complaint "no corresponding close brace")))
                                  (remaining (nthcdr (1+ posn) directives)))
                             ;; FIXME: if POSN is zero, the next argument must be
                             ;; a format control (either a function or a string).
                             (if (format-directive-atsignp iteration)
                                 (values (if (zerop posn) 1 0)
                                         call-arguments-limit
                                         remaining)
                                 ;; FIXME: the argument corresponding to this
                                 ;; directive must be a list.
                                 (let ((nreq (if (zerop posn) 2 1)))
                                   (values nreq nreq remaining))))))
         (walk-directive-list (directives args)
                              (let ((min 0) (max 0))
                                (loop
                                  (let ((directive (pop directives)))
                                    (when (null directive)
                                      (return (values min (min max call-arguments-limit))))
                                    (when (format-directive-p directive)
                                      (incf-both (count :arg (format-directive-params directive)
                                                        :key #'cdr))
                                      (let ((c (format-directive-character directive)))
                                        (cond
                                         ((find c "ABCDEFGORSWX$/")
                                          (incf-both))
                                         ((char= c #\P)
                                          (unless (format-directive-colonp directive)
                                            (incf-both)))
                                         ((or (find c "IT%&|_();>") (char= c #\Newline)))
                                         ;; FIXME: check correspondence of ~( and ~)
                                         ((char= c #\<)
                                          (walk-complex-directive walk-justification))
                                         ((char= c #\[)
                                          (walk-complex-directive walk-conditional))
                                         ((char= c #\{)
                                          (walk-complex-directive walk-iteration))
                                         ((char= c #\?)
                                          ;; FIXME: the argument corresponding to this
                                          ;; directive must be a format control.
                                          (cond
                                           ((format-directive-atsignp directive)
                                            (incf min)
                                            (setq max call-arguments-limit))
                                           (t (incf-both 2))))
                                         (t (throw 'give-up-format-string-walk nil))))))))))
	(catch 'give-up-format-string-walk
	  (let ((directives (tokenize-control-string string)))
	    (walk-directive-list directives args)))))))

;;; From target-format.lisp.

(in-package #:format)

(defun format (destination control-string &rest format-arguments)
  (etypecase destination
    (null
     (with-output-to-string (stream)
       (%format stream control-string format-arguments)))
    (string
     (with-output-to-string (stream destination)
       (%format stream control-string format-arguments)))
    ((member t)
     (%format *standard-output* control-string format-arguments)
     nil)
    ((or stream xp::xp-structure)
     (%format destination control-string format-arguments)
     nil)))

(defun %format (stream string-or-fun orig-args &optional (args orig-args))
  (if (functionp string-or-fun)
      (apply string-or-fun stream args)
      (catch 'up-and-out
	(let* ((string (etypecase string-or-fun
			 (simple-string
			  string-or-fun)
			 (string
			  (coerce string-or-fun 'simple-string))))
	       (*default-format-error-control-string* string)
	       (*logical-block-popper* nil))
	  (interpret-directive-list stream (tokenize-control-string string)
				    orig-args args)))))

(defun interpret-directive-list (stream directives orig-args args)
  (if directives
      (let ((directive (car directives)))
	(etypecase directive
	  (simple-string
	   (write-string directive stream)
	   (interpret-directive-list stream (cdr directives) orig-args args))
	  (format-directive
	   (multiple-value-bind (new-directives new-args)
             (let* ((character (format-directive-character directive))
                    (function
                     (gethash character *format-directive-interpreters*))
                    (*default-format-error-offset*
                     (1- (format-directive-end directive))))
               (unless function
                 (error 'format-error
                        :complaint "unknown format directive ~@[(character: ~A)~]"
                        :args (list (char-name character))))
               (multiple-value-bind (new-directives new-args)
                 (funcall function stream directive
                          (cdr directives) orig-args args)
                 (values new-directives new-args)))
	     (interpret-directive-list stream new-directives
				       orig-args new-args)))))
      args))

;;;; FORMAT directive definition macros and runtime support

(eval-when (:compile-toplevel :execute)

  ;;; This macro is used to extract the next argument from the current arg list.
  ;;; This is the version used by format directive interpreters.
  (defmacro next-arg (&optional offset)
    `(progn
       (when (null args)
         (error 'format-error
                :complaint "no more arguments"
                ,@(when offset
                    `(:offset ,offset))))
       (when *logical-block-popper*
         (funcall *logical-block-popper*))
       (pop args)))

  (defmacro def-complex-format-interpreter (char lambda-list &body body)
    (let ((defun-name
            (intern (concatenate 'string
                                 (let ((name (char-name char)))
                                   (cond (name
                                          (string-capitalize name))
                                         (t
                                          (string char))))
                                 "-FORMAT-DIRECTIVE-INTERPRETER")))
          (directive (gensym))
          (directives (if lambda-list (car (last lambda-list)) (gensym))))
      `(progn
         (defun ,defun-name (stream ,directive ,directives orig-args args)
           (declare (ignorable stream orig-args args))
           ,@(if lambda-list
                 `((let ,(mapcar (lambda (var)
                                   `(,var
                                     (,(sys::symbolicate "FORMAT-DIRECTIVE-" var)
                                      ,directive)))
                                 (butlast lambda-list))
                     (values (progn ,@body) args)))
                 `((declare (ignore ,directive ,directives))
                   ,@body)))
         (%set-format-directive-interpreter ,char #',defun-name))))

  (defmacro def-format-interpreter (char lambda-list &body body)
    (let ((directives (gensym)))
      `(def-complex-format-interpreter ,char (,@lambda-list ,directives)
         ,@body
         ,directives)))

  (defmacro interpret-bind-defaults (specs params &body body)
    (sys::once-only ((params params))
                    (collect ((bindings))
                             (dolist (spec specs)
                               (destructuring-bind (var default) spec
                                                   (bindings `(,var (let* ((param-and-offset (pop ,params))
                                                                           (offset (car param-and-offset))
                                                                           (param (cdr param-and-offset)))
                                                                      (case param
                                                                        (:arg (or (next-arg offset) ,default))
                                                                        (:remaining (length args))
                                                                        ((nil) ,default)
                                                                        (t param)))))))
                             `(let* ,(bindings)
                                (when ,params
                                  (error 'format-error
                                         :complaint
                                         "too many parameters, expected no more than ~W"
                                         :args (list ,(length specs))
                                         :offset (caar ,params)))
                                ,@body))))

  ) ; EVAL-WHEN

;;;; format interpreters and support functions for simple output

(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  (unless padleft
    (write-string string stream))
  (dotimes (i minpad)
    (write-char padchar stream))
  ;; As of sbcl-0.6.12.34, we could end up here when someone tries to
  ;; print e.g. (FORMAT T "~F" "NOTFLOAT"), in which case ANSI says
  ;; we're supposed to soldier on bravely, and so we have to deal with
  ;; the unsupplied-MINCOL-and-COLINC case without blowing up.
  (when (and mincol colinc)
    (do ((chars (+ (length string) (max minpad 0)) (+ chars colinc)))
	((>= chars mincol))
      (dotimes (i colinc)
	(write-char padchar stream))))
  (when padleft
    (write-string string stream)))

(defun format-princ (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
		      (if (or arg (not colonp))
			  (princ-to-string arg)
			  "()")
		      mincol colinc minpad padchar atsignp))

(def-format-interpreter #\A (colonp atsignp params)
  (if params
      (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
				(padchar #\space))
                               params
                               (format-princ stream (next-arg) colonp atsignp
                                             mincol colinc minpad padchar))
      (princ (if colonp (or (next-arg) "()") (next-arg)) stream)))

(defun format-prin1 (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
		      (if (or arg (not colonp))
			  (prin1-to-string arg)
			  "()")
		      mincol colinc minpad padchar atsignp))

(def-format-interpreter #\S (colonp atsignp params)
  (cond (params
	 (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
				   (padchar #\space))
                                  params
                                  (format-prin1 stream (next-arg) colonp atsignp
                                                mincol colinc minpad padchar)))
	(colonp
	 (let ((arg (next-arg)))
	   (if arg
	       (prin1 arg stream)
	       (princ "()" stream))))
	(t
	 (prin1 (next-arg) stream))))

(def-format-interpreter #\C (colonp atsignp params)
  (interpret-bind-defaults () params
                           (if colonp
                               (format-print-named-character (next-arg) stream)
                               (if atsignp
                                   (prin1 (next-arg) stream)
                                   (write-char (next-arg) stream)))))

(defun format-print-named-character (char stream)
  (let* ((name (char-name char)))
    (cond (name
	   (write-string (string-capitalize name) stream))
	  (t
	   (write-char char stream)))))

(def-format-interpreter #\W (colonp atsignp params)
  (interpret-bind-defaults () params
                           (let ((*print-pretty* (or colonp *print-pretty*))
                                 (*print-level* (unless atsignp *print-level*))
                                 (*print-length* (unless atsignp *print-length*)))
                             (sys::output-object (next-arg) stream))))

;;;; format interpreters and support functions for integer output

;;; FORMAT-PRINT-NUMBER does most of the work for the numeric printing
;;; directives. The parameters are interpreted as defined for ~D.
(defun format-print-integer (stream number print-commas-p print-sign-p
                                    radix mincol padchar commachar commainterval)
  (let ((*print-base* radix)
	(*print-radix* nil))
    (if (integerp number)
	(let* ((text (princ-to-string (abs number)))
	       (commaed (if print-commas-p
			    (format-add-commas text commachar commainterval)
			    text))
	       (signed (cond ((minusp number)
			      (concatenate 'string "-" commaed))
			     (print-sign-p
			      (concatenate 'string "+" commaed))
			     (t commaed))))
	  ;; colinc = 1, minpad = 0, padleft = t
	  (format-write-field stream signed mincol 1 0 padchar t))
	(princ number stream))))

(defun format-add-commas (string commachar commainterval)
  (let ((length (length string)))
    (multiple-value-bind (commas extra) (truncate (1- length) commainterval)
      (let ((new-string (make-string (+ length commas)))
	    (first-comma (1+ extra)))
	(replace new-string string :end1 first-comma :end2 first-comma)
	(do ((src first-comma (+ src commainterval))
	     (dst first-comma (+ dst commainterval 1)))
	    ((= src length))
	  (setf (schar new-string dst) commachar)
	  (replace new-string string :start1 (1+ dst)
		   :start2 src :end2 (+ src commainterval)))
	new-string))))

;;; FIXME: This is only needed in this file, could be defined with
;;; SB!XC:DEFMACRO inside EVAL-WHEN
(defmacro interpret-format-integer (base)
  `(if (or colonp atsignp params)
       (interpret-bind-defaults
        ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
        params
        (format-print-integer stream (next-arg) colonp atsignp ,base mincol
                              padchar commachar commainterval))
       (write (next-arg) :stream stream :base ,base :radix nil :escape nil)))

(def-format-interpreter #\D (colonp atsignp params)
  (interpret-format-integer 10))

(def-format-interpreter #\B (colonp atsignp params)
  (interpret-format-integer 2))

(def-format-interpreter #\O (colonp atsignp params)
  (interpret-format-integer 8))

(def-format-interpreter #\X (colonp atsignp params)
  (interpret-format-integer 16))

(def-format-interpreter #\R (colonp atsignp params)
  (interpret-bind-defaults
   ((base nil) (mincol 0) (padchar #\space) (commachar #\,)
    (commainterval 3))
   params
   (let ((arg (next-arg)))
     (if base
         (format-print-integer stream arg colonp atsignp base mincol
                               padchar commachar commainterval)
         (if atsignp
             (if colonp
                 (format-print-old-roman stream arg)
                 (format-print-roman stream arg))
             (if colonp
                 (format-print-ordinal stream arg)
                 (format-print-cardinal stream arg)))))))

(defparameter *cardinal-ones*
  #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defparameter *cardinal-tens*
  #(nil nil "twenty" "thirty" "forty"
	"fifty" "sixty" "seventy" "eighty" "ninety"))

(defparameter *cardinal-teens*
  #("ten" "eleven" "twelve" "thirteen" "fourteen"  ;;; RAD
          "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(defparameter *cardinal-periods*
  #("" " thousand" " million" " billion" " trillion" " quadrillion"
       " quintillion" " sextillion" " septillion" " octillion" " nonillion"
       " decillion" " undecillion" " duodecillion" " tredecillion"
       " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
       " octodecillion" " novemdecillion" " vigintillion"))

(defparameter *ordinal-ones*
  #(nil "first" "second" "third" "fourth"
	"fifth" "sixth" "seventh" "eighth" "ninth"))

(defparameter *ordinal-tens*
  #(nil "tenth" "twentieth" "thirtieth" "fortieth"
	"fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

(defun format-print-small-cardinal (stream n)
  (multiple-value-bind (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref *cardinal-ones* hundreds) stream)
      (write-string " hundred" stream)
      (when (plusp rem)
	(write-char #\space stream)))
    (when (plusp rem)
      (multiple-value-bind (tens ones) (truncate rem 10)
	(cond ((< 1 tens)
               (write-string (svref *cardinal-tens* tens) stream)
               (when (plusp ones)
                 (write-char #\- stream)
                 (write-string (svref *cardinal-ones* ones) stream)))
              ((= tens 1)
               (write-string (svref *cardinal-teens* ones) stream))
              ((plusp ones)
               (write-string (svref *cardinal-ones* ones) stream)))))))

(defun format-print-cardinal (stream n)
  (cond ((minusp n)
	 (write-string "negative " stream)
	 (format-print-cardinal-aux stream (- n) 0 n))
	((zerop n)
	 (write-string "zero" stream))
	(t
	 (format-print-cardinal-aux stream n 0 n))))

(defun format-print-cardinal-aux (stream n period err)
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (<= period 20)
      (error "number too large to print in English: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux stream beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond)
	(write-char #\space stream))
      (format-print-small-cardinal stream here)
      (write-string (svref *cardinal-periods* period) stream))))

(defun format-print-ordinal (stream n)
  (when (minusp n)
    (write-string "negative " stream))
  (let ((number (abs n)))
    (multiple-value-bind (top bot) (truncate number 100)
      (unless (zerop top)
	(format-print-cardinal stream (- number bot)))
      (when (and (plusp top) (plusp bot))
	(write-char #\space stream))
      (multiple-value-bind (tens ones) (truncate bot 10)
	(cond ((= bot 12) (write-string "twelfth" stream))
	      ((= tens 1)
	       (write-string (svref *cardinal-teens* ones) stream);;;RAD
	       (write-string "th" stream))
	      ((and (zerop tens) (plusp ones))
	       (write-string (svref *ordinal-ones* ones) stream))
	      ((and (zerop ones)(plusp tens))
	       (write-string (svref *ordinal-tens* tens) stream))
	      ((plusp bot)
	       (write-string (svref *cardinal-tens* tens) stream)
	       (write-char #\- stream)
	       (write-string (svref *ordinal-ones* ones) stream))
	      ((plusp number)
	       (write-string "th" stream))
	      (t
	       (write-string "zeroth" stream)))))))

;;; Print Roman numerals

(defun format-print-old-roman (stream n)
  (unless (< 0 n 5000)
    (error "Number too large to print in old Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (start n (do ((i start (progn
				(write-char cur-char stream)
				(- i cur-val))))
		    ((< i cur-val) i))))
      ((zerop start))))

(defun format-print-roman (stream n)
  (unless (< 0 n 4000)
    (error "Number too large to print in Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (sub-chars '(#\C #\X #\X #\I #\I) (cdr sub-chars))
       (sub-val '(100 10 10 1 1 0) (cdr sub-val))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (cur-sub-char #\C (car sub-chars))
       (cur-sub-val 100 (car sub-val))
       (start n (do ((i start (progn
				(write-char cur-char stream)
				(- i cur-val))))
		    ((< i cur-val)
		     (cond ((<= (- cur-val cur-sub-val) i)
			    (write-char cur-sub-char stream)
			    (write-char cur-char stream)
			    (- i (- cur-val cur-sub-val)))
			   (t i))))))
      ((zerop start))))

;;;; plural

(def-format-interpreter #\P (colonp atsignp params)
  (interpret-bind-defaults () params
                           (let ((arg (if colonp
                                          (if (eq orig-args args)
                                              (error 'format-error
                                                     :complaint "no previous argument")
                                              (do ((arg-ptr orig-args (cdr arg-ptr)))
                                                  ((eq (cdr arg-ptr) args)
                                                   (car arg-ptr))))
                                          (next-arg))))
                             (if atsignp
                                 (write-string (if (eql arg 1) "y" "ies") stream)
                                 (unless (eql arg 1) (write-char #\s stream))))))

;;;; format interpreters and support functions for floating point output

(defun decimal-string (n)
  (write-to-string n :base 10 :radix nil :escape nil))

(def-format-interpreter #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "cannot specify the colon modifier with this directive"))
  (interpret-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space))
			   params
                           (format-fixed stream (next-arg) w d k ovf pad atsignp)))

(defun format-fixed (stream number w d k ovf pad atsign)
  (if (numberp number)
      (if (floatp number)
	  (format-fixed-aux stream number w d k ovf pad atsign)
	  (if (rationalp number)
	      (format-fixed-aux stream
				(coerce number 'single-float)
				w d k ovf pad atsign)
	      (format-write-field stream
				  (decimal-string number)
				  w 1 0 #\space t)))
      (format-princ stream number nil nil w 1 0 pad)))

;;; We return true if we overflowed, so that ~G can output the overflow char
;;; instead of spaces.
(defun format-fixed-aux (stream number w d k ovf pad atsign)
  (cond
   ((and (floatp number)
         (or (sys:float-infinity-p number)
             (sys:float-nan-p number)))
    (prin1 number stream)
    nil)
   (t
    (let ((spaceleft w))
      (when (and w (or atsign (minusp (float-sign number))))
        (decf spaceleft))
      (multiple-value-bind (str len lpoint tpoint)
        (sys::flonum-to-string (abs number) spaceleft d k)
	;;if caller specifically requested no fraction digits, suppress the
	;;optional trailing zero
	(when (and d (zerop d))
          (setf tpoint nil))
	(when w
	  (decf spaceleft len)
	  ;;optional leading zero
	  (when lpoint
	    (if (or (> spaceleft 0) tpoint) ;force at least one digit
		(decf spaceleft)
		(setq lpoint nil)))
	  ;;optional trailing zero
	  (when tpoint
	    (if (> spaceleft 0)
		(decf spaceleft)
		(setq tpoint nil))))
	(cond ((and w (< spaceleft 0) ovf)
	       ;;field width overflow
	       (dotimes (i w) (write-char ovf stream))
	       t)
	      (t
	       (when w (dotimes (i spaceleft) (write-char pad stream)))
	       (cond ((minusp (float-sign number))
                      (write-char #\- stream))
                     (atsign
                      (write-char #\+ stream)))
	       (when lpoint (write-char #\0 stream))
	       (write-string str stream)
	       (when tpoint (write-char #\0 stream))
	       nil)))))))

(def-format-interpreter #\E (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "cannot specify the colon modifier with this directive"))
  (interpret-bind-defaults
   ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
   params
   (format-exponential stream (next-arg) w d e k ovf pad mark atsignp)))

(defun format-exponential (stream number w d e k ovf pad marker atsign)
  (if (numberp number)
      (if (floatp number)
	  (format-exp-aux stream number w d e k ovf pad marker atsign)
	  (if (rationalp number)
	      (format-exp-aux stream
			      (coerce number 'single-float)
			      w d e k ovf pad marker atsign)
	      (format-write-field stream
				  (decimal-string number)
				  w 1 0 #\space t)))
      (format-princ stream number nil nil w 1 0 pad)))

(defun format-exponent-marker (number)
  (if (typep number *read-default-float-format*)
      #\e
      (typecase number
	(single-float #\f)
	(double-float #\d)
	(short-float #\s)
	(long-float #\l))))

;;; Here we prevent the scale factor from shifting all significance out of
;;; a number to the right. We allow insignificant zeroes to be shifted in
;;; to the left right, athough it is an error to specify k and d such that this
;;; occurs. Perhaps we should detect both these condtions and flag them as
;;; errors. As for now, we let the user get away with it, and merely guarantee
;;; that at least one significant digit will appear.

;;; Raymond Toy writes: The Hyperspec seems to say that the exponent
;;; marker is always printed. Make it so. Also, the original version
;;; causes errors when printing infinities or NaN's. The Hyperspec is
;;; silent here, so let's just print out infinities and NaN's instead
;;; of causing an error.
(defun format-exp-aux (stream number w d e k ovf pad marker atsign)
  (if (and (floatp number)
	   (or (sys::float-infinity-p number)
	       (sys::float-nan-p number)))
      (prin1 number stream)
      (multiple-value-bind (num expt) (sys::scale-exponent (abs number))
	(let* ((expt (- expt k))
	       (estr (decimal-string (abs expt)))
	       (elen (if e (max (length estr) e) (length estr)))
	       (fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
	       (fmin (if (minusp k) (- 1 k) nil))
	       (spaceleft (if w
			      (- w 2 elen
				 (if (or atsign (minusp number))
				     1 0))
			      nil)))
	  (if (and w ovf e (> elen e)) ;exponent overflow
	      (dotimes (i w) (write-char ovf stream))
	      (multiple-value-bind (fstr flen lpoint)
                (sys::flonum-to-string num spaceleft fdig k fmin)
		(when w
		  (decf spaceleft flen)
		  (when lpoint
		    (if (> spaceleft 0)
			(decf spaceleft)
			(setq lpoint nil))))
		(cond ((and w (< spaceleft 0) ovf)
		       ;;significand overflow
		       (dotimes (i w) (write-char ovf stream)))
		      (t (when w
			   (dotimes (i spaceleft) (write-char pad stream)))
			 (if (minusp number)
			     (write-char #\- stream)
			     (if atsign (write-char #\+ stream)))
			 (when lpoint (write-char #\0 stream))
			 (write-string fstr stream)
			 (write-char (if marker
					 marker
					 (format-exponent-marker number))
				     stream)
			 (write-char (if (minusp expt) #\- #\+) stream)
			 (when e
			   ;;zero-fill before exponent if necessary
			   (dotimes (i (- e (length estr)))
			     (write-char #\0 stream)))
			 (write-string estr stream)))))))))

(def-format-interpreter #\G (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "cannot specify the colon modifier with this directive"))
  (interpret-bind-defaults
   ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (mark nil))
   params
   (format-general stream (next-arg) w d e k ovf pad mark atsignp)))

(defun format-general (stream number w d e k ovf pad marker atsign)
  (if (numberp number)
      (if (floatp number)
	  (format-general-aux stream number w d e k ovf pad marker atsign)
	  (if (rationalp number)
	      (format-general-aux stream
				  (coerce number 'single-float)
				  w d e k ovf pad marker atsign)
	      (format-write-field stream
				  (decimal-string number)
				  w 1 0 #\space t)))
      (format-princ stream number nil nil w 1 0 pad)))

;;; Raymond Toy writes: same change as for format-exp-aux
(defun format-general-aux (stream number w d e k ovf pad marker atsign)
  (if (and (floatp number)
	   (or (sys::float-infinity-p number)
	       (sys::float-nan-p number)))
      (prin1 number stream)
      (multiple-value-bind (ignore n) (sys::scale-exponent (abs number))
	(declare (ignore ignore))
	;; KLUDGE: Default d if omitted. The procedure is taken directly from
	;; the definition given in the manual, and is not very efficient, since
	;; we generate the digits twice. Future maintainers are encouraged to
	;; improve on this. -- rtoy?? 1998??
	(unless d
	  (multiple-value-bind (str len)
            (sys::flonum-to-string (abs number))
	    (declare (ignore str))
	    (let ((q (if (= len 1) 1 (1- len))))
	      (setq d (max q (min n 7))))))
	(let* ((ee (if e (+ e 2) 4))
	       (ww (if w (- w ee) nil))
	       (dd (- d n)))
	  (cond ((<= 0 dd d)
		 (let ((char (if (format-fixed-aux stream number ww dd nil
						   ovf pad atsign)
				 ovf
				 #\space)))
		   (dotimes (i ee) (write-char char stream))))
		(t
		 (format-exp-aux stream number w d e (or k 1)
				 ovf pad marker atsign)))))))

(def-format-interpreter #\$ (colonp atsignp params)
  (interpret-bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
                           (format-dollars stream (next-arg) d n w pad colonp atsignp)))

(defun format-dollars (stream number d n w pad colon atsign)
  (when (rationalp number)
    ;; This coercion to SINGLE-FLOAT seems as though it gratuitously
    ;; loses precision (why not LONG-FLOAT?) but it's the default
    ;; behavior in the ANSI spec, so in some sense it's the right
    ;; thing, and at least the user shouldn't be surprised.
    (setq number (coerce number 'single-float)))
  (if (floatp number)
      (let* ((signstr (if (minusp number) "-" (if atsign "+" "")))
	     (signlen (length signstr)))
	(multiple-value-bind (str strlen ig2 ig3 pointplace)
          (sys::flonum-to-string number nil d nil)
	  (declare (ignore ig2 ig3 strlen))
	  (when colon
	    (write-string signstr stream))
	  (dotimes (i (- w signlen (max n pointplace) 1 d))
	    (write-char pad stream))
	  (unless colon
	    (write-string signstr stream))
	  (dotimes (i (- n pointplace))
	    (write-char #\0 stream))
	  (write-string str stream)))
      (format-write-field stream
			  (decimal-string number)
			  w 1 0 #\space t)))

;;;; FORMAT interpreters and support functions for line/page breaks etc.

(def-format-interpreter #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "cannot specify either colon or atsign for this directive"))
  (interpret-bind-defaults ((count 1)) params
                           (dotimes (i count)
                             (terpri stream))))

(def-format-interpreter #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "cannot specify either colon or atsign for this directive"))
  (interpret-bind-defaults ((count 1)) params
                           (fresh-line stream)
                           (dotimes (i (1- count))
                             (terpri stream))))

(def-format-interpreter #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "cannot specify either colon or atsign for this directive"))
  (interpret-bind-defaults ((count 1)) params
                           (dotimes (i count)
                             (write-char (code-char sys::form-feed-char-code) stream))))

(def-format-interpreter #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "cannot specify either colon or atsign for this directive"))
  (interpret-bind-defaults ((count 1)) params
                           (dotimes (i count)
                             (write-char #\~ stream))))

(def-complex-format-interpreter #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
	   :complaint
	   "cannot specify both colon and atsign for this directive"))
  (interpret-bind-defaults () params
                           (when atsignp
                             (write-char #\newline stream)))
  (if (and (not colonp)
	   directives
	   (simple-string-p (car directives)))
      (cons (string-left-trim *format-whitespace-chars*
			      (car directives))
	    (cdr directives))
      directives))

;;;; format interpreters and support functions for tabs and simple pretty
;;;; printing

(def-format-interpreter #\T (colonp atsignp params)
  (if colonp
      (interpret-bind-defaults ((n 1) (m 1)) params
                               (pprint-tab (if atsignp :section-relative :section) n m stream))
      (if atsignp
	  (interpret-bind-defaults ((colrel 1) (colinc 1)) params
                                   (format-relative-tab stream colrel colinc))
	  (interpret-bind-defaults ((colnum 1) (colinc 1)) params
                                   (format-absolute-tab stream colnum colinc)))))

(defun output-spaces (stream n)
  (let ((spaces #.(make-string 100 :initial-element #\space)))
    (loop
      (when (< n (length spaces))
	(return))
      (write-string spaces stream)
      (decf n (length spaces)))
    (write-string spaces stream :end n)))

(defun format-relative-tab (stream colrel colinc)
  (if (xp::xp-structure-p stream)
      (pprint-tab :line-relative colrel colinc stream)
      (let* ((cur (charpos stream))
	     (spaces (if (and cur (plusp colinc))
			 (- (* (ceiling (+ cur colrel) colinc) colinc) cur)
			 colrel)))
	(output-spaces stream spaces))))

(defun format-absolute-tab (stream colnum colinc)
  (if (xp::xp-structure-p stream)
      (pprint-tab :line colnum colinc stream)
      (let ((cur (charpos stream)))
	(cond ((null cur)
	       (write-string "  " stream))
	      ((< cur colnum)
	       (output-spaces stream (- colnum cur)))
	      (t
	       (unless (zerop colinc)
		 (output-spaces stream
				(- colinc (rem (- cur colnum) colinc)))))))))

(def-format-interpreter #\_ (colonp atsignp params)
  (interpret-bind-defaults () params
                           (pprint-newline (if colonp
                                               (if atsignp
                                                   :mandatory
                                                   :fill)
                                               (if atsignp
                                                   :miser
                                                   :linear))
                                           stream)))

(def-format-interpreter #\I (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint "cannot specify the at-sign modifier"))
  (interpret-bind-defaults ((n 0)) params
                           (pprint-indent (if colonp :current :block) n stream)))

;;;; format interpreter for ~*

(def-format-interpreter #\* (colonp atsignp params)
  (if atsignp
      (if colonp
	  (error 'format-error
		 :complaint "cannot specify both colon and at-sign")
	  (interpret-bind-defaults ((posn 0)) params
                                   (if (<= 0 posn (length orig-args))
                                       (setf args (nthcdr posn orig-args))
                                       (error 'format-error
                                              :complaint "Index ~W is out of bounds. (It should ~
                                              have been between 0 and ~W.)"
                                              :args (list posn (length orig-args))))))
      (if colonp
	  (interpret-bind-defaults ((n 1)) params
                                   (do ((cur-posn 0 (1+ cur-posn))
                                        (arg-ptr orig-args (cdr arg-ptr)))
                                       ((eq arg-ptr args)
                                        (let ((new-posn (- cur-posn n)))
                                          (if (<= 0 new-posn (length orig-args))
                                              (setf args (nthcdr new-posn orig-args))
                                              (error 'format-error
                                                     :complaint
                                                     "Index ~W is out of bounds. (It should
                                                      have been between 0 and ~W.)"
                                                     :args
                                                     (list new-posn (length orig-args))))))))
	  (interpret-bind-defaults ((n 1)) params
                                   (dotimes (i n)
                                     (next-arg))))))

;;;; format interpreter for indirection

(def-format-interpreter #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error
	   :complaint "cannot specify the colon modifier"))
  (interpret-bind-defaults () params
                           (handler-bind
                             ((format-error
                               (lambda (condition)
                                 (error 'format-error
                                        :complaint
                                        "~A~%while processing indirect format string:"
                                        :args (list condition)
                                        :print-banner nil
                                        :control-string string
                                        :offset (1- end)))))
                             (if atsignp
                                 (setf args (%format stream (next-arg) orig-args args))
                                 (%format stream (next-arg) (next-arg))))))

;;;; format interpreters for capitalization

(def-complex-format-interpreter #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
	     :complaint "no corresponding close paren"))
    (interpret-bind-defaults () params
                             (let* ((posn (position close directives))
                                    (before (subseq directives 0 posn))
                                    (after (nthcdr (1+ posn) directives))
                                    (stream (sys::make-case-frob-stream 
                                             (if (typep stream 'xp::xp-structure)
                                                 (xp::base-stream stream)
                                                 stream)
                                             (if colonp
                                                 (if atsignp
                                                     :upcase
                                                     :capitalize)
                                                 (if atsignp
                                                     :capitalize-first
                                                     :downcase)))))
                               (setf args (interpret-directive-list stream before orig-args args))
                               after))))

(def-complex-format-interpreter #\) ()
  (error 'format-error
	 :complaint "no corresponding open paren"))

;;;; format interpreters and support functions for conditionalization

(def-complex-format-interpreter #\[ (colonp atsignp params directives)
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
    (parse-conditional-directive directives)
    (setf args
	  (if atsignp
	      (if colonp
		  (error 'format-error
			 :complaint
                         "cannot specify both the colon and at-sign modifiers")
		  (if (cdr sublists)
		      (error 'format-error
			     :complaint
			     "can only specify one section")
		      (interpret-bind-defaults () params
                                               (let ((prev-args args)
                                                     (arg (next-arg)))
                                                 (if arg
                                                     (interpret-directive-list stream
                                                                               (car sublists)
                                                                               orig-args
                                                                               prev-args)
                                                     args)))))
	      (if colonp
		  (if (= (length sublists) 2)
		      (interpret-bind-defaults () params
                                               (if (next-arg)
                                                   (interpret-directive-list stream (car sublists)
                                                                             orig-args args)
                                                   (interpret-directive-list stream (cadr sublists)
                                                                             orig-args args)))
		      (error 'format-error
			     :complaint
			     "must specify exactly two sections"))
		  (interpret-bind-defaults ((index (next-arg))) params
                                           (let* ((default (and last-semi-with-colon-p
                                                                (pop sublists)))
                                                  (last (1- (length sublists)))
                                                  (sublist
                                                   (if (<= 0 index last)
                                                       (nth (- last index) sublists)
                                                       default)))
                                             (interpret-directive-list stream sublist orig-args
                                                                       args))))))
    remaining))

(def-complex-format-interpreter #\; ()
  (error 'format-error
	 :complaint
	 "~~; not contained within either ~~[...~~] or ~~<...~~>"))

(def-complex-format-interpreter #\] ()
  (error 'format-error
	 :complaint
	 "no corresponding open bracket"))

;;;; format interpreter for up-and-out

(defvar *outside-args*)

(def-format-interpreter #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint "cannot specify the at-sign modifier"))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
	   :complaint "attempt to use ~~:^ outside a ~~:{...~~} construct"))
  (when (interpret-bind-defaults ((arg1 nil) (arg2 nil) (arg3 nil)) params
          (cond (arg3 (<= arg1 arg2 arg3))
                (arg2 (eql arg1 arg2))
                (arg1 (eql arg1 0))
                (t (if colonp
                       (null *outside-args*)
                       (null args)))))
    (throw (if colonp 'up-up-and-out 'up-and-out)
	   args)))

;;;; format interpreters for iteration

(def-complex-format-interpreter #\{
  (colonp atsignp params string end directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
	     :complaint
	     "no corresponding close brace"))
    (interpret-bind-defaults ((max-count nil)) params
      (let* ((closed-with-colon (format-directive-colonp close))
             (posn (position close directives))
             (insides (if (zerop posn)
                          (next-arg)
                          (subseq directives 0 posn)))
             (*up-up-and-out-allowed* colonp))
        (labels
            ((do-guts (orig-args args)
                      (if (zerop posn)
                          (handler-bind
                            ((format-error
                              (lambda (condition)
                                (error
                                 'format-error
                                 :complaint
                                 "~A~%while processing indirect format string:"
                                 :args (list condition)
                                 :print-banner nil
                                 :control-string string
                                 :offset (1- end)))))
                            (%format stream insides orig-args args))
                          (interpret-directive-list stream insides
                                                    orig-args args)))
             (bind-args (orig-args args)
                        (if colonp
                            (let* ((arg (next-arg))
                                   (*logical-block-popper* nil)
                                   (*outside-args* args))
                              (catch 'up-and-out
                                (do-guts arg arg))
                              args)
                            (do-guts orig-args args)))
             (do-loop (orig-args args)
                      (catch (if colonp 'up-up-and-out 'up-and-out)
                        (loop
                          (when (and (not closed-with-colon) (null args))
                            (return))
                          (when (and max-count (minusp (decf max-count)))
                            (return))
                          (setf args (bind-args orig-args args))
                          (when (and closed-with-colon (null args))
                            (return)))
                        args)))
          (if atsignp
              (setf args (do-loop orig-args args))
              (let ((arg (next-arg))
                    (*logical-block-popper* nil))
                (do-loop arg arg)))
          (nthcdr (1+ posn) directives))))))

(def-complex-format-interpreter #\} ()
  (error 'format-error
	 :complaint "no corresponding open brace"))

;;;; format interpreters and support functions for justification

(def-complex-format-interpreter #\<
  (colonp atsignp params string end directives)
  (multiple-value-bind (segments first-semi close remaining)
    (parse-format-justification directives)
    (setf args
	  (if (format-directive-colonp close)
	      (multiple-value-bind (prefix per-line-p insides suffix)
                (parse-format-logical-block segments colonp first-semi
                                            close params string end)
		(interpret-format-logical-block stream orig-args args
						prefix per-line-p insides
						suffix atsignp))
	      (let ((count (reduce #'+ (mapcar (lambda (x) (count-if #'illegal-inside-justification-p x)) segments))))
		(when (> count 0)
		  ;; ANSI specifies that "an error is signalled" in this
		  ;; situation.
		  (error 'format-error
			 :complaint "~D illegal directive~:P found inside justification block"
			 :args (list count)))
		(interpret-format-justification stream orig-args args
						segments colonp atsignp
						first-semi params))))
    remaining))

(defun interpret-format-justification
  (stream orig-args args segments colonp atsignp first-semi params)
  (interpret-bind-defaults
   ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
   params
   (let ((newline-string nil)
         (strings nil)
         (extra-space 0)
         (line-len 0))
     (setf args
           (catch 'up-and-out
             (when (and first-semi (format-directive-colonp first-semi))
               (interpret-bind-defaults
                ((extra 0)
                 (len (or #-abcl(sb!impl::line-length stream) 72)))
                (format-directive-params first-semi)
                (setf newline-string
                      (with-output-to-string (stream)
                        (setf args
                              (interpret-directive-list stream
                                                        (pop segments)
                                                        orig-args
                                                        args))))
                (setf extra-space extra)
                (setf line-len len)))
             (dolist (segment segments)
               (push (with-output-to-string (stream)
                       (setf args
                             (interpret-directive-list stream segment
                                                       orig-args args)))
                     strings))
             args))
     (format-justification stream newline-string extra-space line-len strings
                           colonp atsignp mincol colinc minpad padchar)))
  args)

(defun format-justification (stream newline-prefix extra-space line-len strings
                                    pad-left pad-right mincol colinc minpad padchar)
  (setf strings (reverse strings))
  (let* ((num-gaps (+ (1- (length strings))
		      (if pad-left 1 0)
		      (if pad-right 1 0)))
	 (chars (+ (* num-gaps minpad)
		   (loop
		     for string in strings
		     summing (length string))))
	 (length (if (> chars mincol)
		     (+ mincol (* (ceiling (- chars mincol) colinc) colinc))
		     mincol))
	 (padding (+ (- length chars) (* num-gaps minpad))))
    (when (and newline-prefix
	       (> (+ (or (charpos stream) 0)
		     length extra-space)
		  line-len))
      (write-string newline-prefix stream))
    (flet ((do-padding ()
                       (let ((pad-len (if (zerop num-gaps)
                                          padding
                                          (truncate padding num-gaps))))
                         (decf padding pad-len)
                         (decf num-gaps)
                         (dotimes (i pad-len) (write-char padchar stream)))))
      (when (or pad-left
		(and (not pad-right) (null (cdr strings))))
	(do-padding))
      (when strings
	(write-string (car strings) stream)
	(dolist (string (cdr strings))
	  (do-padding)
	  (write-string string stream)))
      (when pad-right
	(do-padding)))))

(defun interpret-format-logical-block
  (stream orig-args args prefix per-line-p insides suffix atsignp)
  (let ((arg (if atsignp args (next-arg))))
    (if per-line-p
	(pprint-logical-block
         (stream arg :per-line-prefix prefix :suffix suffix)
         (let ((*logical-block-popper* (lambda () (pprint-pop))))
           (catch 'up-and-out
             (interpret-directive-list stream insides
                                       (if atsignp orig-args arg)
                                       arg))))
	(pprint-logical-block (stream arg :prefix prefix :suffix suffix)
                              (let ((*logical-block-popper* (lambda () (pprint-pop))))
                                (catch 'up-and-out
                                  (interpret-directive-list stream insides
                                                            (if atsignp orig-args arg)
                                                            arg))))))
  (if atsignp nil args))

;;;; format interpreter and support functions for user-defined method

(def-format-interpreter #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-fun-name string start end)))
    (collect ((args))
             (dolist (param-and-offset params)
               (let ((param (cdr param-and-offset)))
                 (case param
                   (:arg (args (next-arg)))
                   (:remaining (args (length args)))
                   (t (args param)))))
             (apply (fdefinition symbol) stream (next-arg) colonp atsignp (args)))))

(setf (symbol-function 'sys::simple-format) #'format)


(provide 'format)
