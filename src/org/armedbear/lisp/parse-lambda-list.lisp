;;; parse-lambda-list.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
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

;;; Adapted from SBCL.

(in-package "SYSTEM")

(require '#:collect)

;;; Break something like a lambda list (but not necessarily actually a
;;; lambda list, e.g. the representation of argument types which is
;;; used within an FTYPE specification) into its component parts. We
;;; return 10 values:
;;;  1. a list of the required args;
;;;  2. a list of the &OPTIONAL arg specs;
;;;  3. true if a &REST arg was specified;
;;;  4. the &REST arg;
;;;  5. true if &KEY args are present;
;;;  6. a list of the &KEY arg specs;
;;;  7. true if &ALLOW-OTHER-KEYS was specified.;
;;;  8. true if any &AUX is present (new in SBCL vs. CMU CL);
;;;  9. a list of the &AUX specifiers;
;;; 10. true if any lambda list keyword is present (only for
;;;     PARSE-LAMBDA-LIST-LIKE-THING).
;;;
;;; The top level lambda list syntax is checked for validity, but the
;;; arg specifiers are just passed through untouched. If something is
;;; wrong, we signal an error.

(defun parse-lambda-list-like-thing (list)
  (collect ((required)
            (optional)
            (keys)
            (aux))
    (let ((restp nil)
          (rest nil)
          (keyp nil)
	  (auxp nil)
          (allowp nil)
          (state :required))
      (declare (type (member :allow-other-keys :aux
                             :key
                             :optional
                             :post-rest
                             :required :rest)
                     state))
      (dolist (arg list)
        (if (and (symbolp arg)
                 (let ((name (symbol-name (the symbol arg))))
                   (and (plusp (length name))
                        (char= (char name 0) #\&))))
            (case arg
              (&optional
               (unless (eq state :required)
                 (error "misplaced &OPTIONAL in lambda list: ~S" list))
               (setq state :optional))
              (&rest
               (unless (member state '(:required :optional))
                 (error "misplaced &REST in lambda list: ~S" list))
               (setq state :rest))
              (&key
               (unless (member state
                               '(:required :optional :post-rest))
                 (error "misplaced &KEY in lambda list: ~S" list))
               (setq keyp t
                     state :key))
              (&allow-other-keys
               (unless (eq state ':key)
                 (error "misplaced &ALLOW-OTHER-KEYS in lambda list: ~S" list))
               (setq allowp t
                     state :allow-other-keys))
              (&aux
               (when (eq state :rest)
                 (error "misplaced &AUX in lambda list: ~S" list))
               (setq auxp t
		     state :aux))
              ;; FIXME: I don't think ANSI says this is an error. (It
              ;; should certainly be good for a STYLE-WARNING,
              ;; though.)
              (t
               (error "unknown &KEYWORD in lambda list: ~S" arg)))
            (case state
              (:required (required arg))
              (:optional (optional arg))
              (:rest
               (setq restp t
                     rest arg
                     state :post-rest))
              (:key (keys arg))
              (:aux (aux arg))
              (t
               (error "found garbage in lambda list when expecting a keyword: ~S"
                      arg)))))
      (when (eq state :rest)
        (error "&REST without rest variable"))

      (values (required) (optional) restp rest keyp (keys) allowp auxp (aux)
              (neq state :required)))))

;;; like PARSE-LAMBDA-LIST-LIKE-THING, except our LAMBDA-LIST argument
;;; really *is* a lambda list, not just a "lambda-list-like thing", so
;;; can barf on things which're illegal as arguments in lambda lists
;;; even if they could conceivably be legal in not-quite-a-lambda-list
;;; weirdosities
(defun parse-lambda-list (lambda-list)
  ;; Classify parameters without checking their validity individually.
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux)
      (parse-lambda-list-like-thing lambda-list)
    ;; Check validity of parameters.
    (flet ((need-symbol (x why)
	     (unless (symbolp x)
	       (error "~A is not a symbol: ~S" why x))))
      (dolist (i required)
	(need-symbol i "Required argument"))
      (dolist (i optional)
	(typecase i
	  (symbol)
	  (cons
	   (destructuring-bind (var &optional init-form supplied-p) i
	     (declare (ignore init-form supplied-p))
	     (need-symbol var "&OPTIONAL parameter name")))
	  (t
	   (error "&OPTIONAL parameter is not a symbol or cons: ~S" i))))
      (when restp
	(need-symbol rest "&REST argument"))
      (when keyp
	(dolist (i keys)
	  (typecase i
	    (symbol)
	    (cons
	     (destructuring-bind (var-or-kv &optional init-form supplied-p) i
	       (declare (ignore init-form supplied-p))
	       (if (consp var-or-kv)
		   (destructuring-bind (keyword-name var) var-or-kv
		     (declare (ignore keyword-name))
		     (need-symbol var "&KEY parameter name"))
		   (need-symbol var-or-kv "&KEY parameter name"))))
	    (t
	     (error "&KEY parameter is not a symbol or cons: ~S" i))))))

    ;; Voila.
    (values required optional restp rest keyp keys allowp auxp aux)))
