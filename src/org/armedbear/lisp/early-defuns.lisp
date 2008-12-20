;;; early-defuns.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves
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

(in-package #:system)

(export '(index java-long %type-error check-sequence-bounds require-type
          normalize-type))

;; (deftype index () '(integer 0 (#.most-positive-fixnum)))
(put 'index 'deftype-definition
     (lambda () '(integer 0 (#.most-positive-fixnum))))

;; (deftype java-long ()
;;   '(integer #.most-negative-java-long #.most-positive-java-long))
(put 'java-long 'deftype-definition
     (lambda () '(integer #.most-negative-java-long #.most-positive-java-long)))

(defun %type-error (datum expected-type)
  (error 'type-error :datum datum :expected-type expected-type))

(defun check-sequence-bounds (sequence start end)
  (declare (optimize speed))
  (unless (fixnump start)
    (%type-error start 'fixnum))
  (if end
      (unless (fixnump end)
        (%type-error end 'fixnum))
      (setf end (length sequence)))
  end)

(defun require-type (arg type)
  (if (typep arg type)
      arg
      (error 'simple-type-error
             :datum arg
             :expected-type type)))

(defun normalize-type (type)
  (cond ((symbolp type)
         (case type
           (BIT
            (return-from normalize-type '(integer 0 1)))
           (CONS
            (return-from normalize-type '(cons t t)))
           (FIXNUM
            (return-from normalize-type
                         '(integer #.most-negative-fixnum #.most-positive-fixnum)))
           (SIGNED-BYTE
            (return-from normalize-type 'integer))
           (UNSIGNED-BYTE
            (return-from normalize-type '(integer 0 *)))
           (BASE-CHAR
            (return-from normalize-type 'character))
           (SHORT-FLOAT
            (return-from normalize-type 'single-float))
           (LONG-FLOAT
            (return-from normalize-type 'double-float))
           (COMPLEX
            (return-from normalize-type '(complex *)))
           (ARRAY
            (return-from normalize-type '(array * *)))
           (SIMPLE-ARRAY
            (return-from normalize-type '(simple-array * *)))
           (VECTOR
            (return-from normalize-type '(array * (*))))
           (SIMPLE-VECTOR
            (return-from normalize-type '(simple-array t (*))))
           (BIT-VECTOR
            (return-from normalize-type '(bit-vector *)))
           (SIMPLE-BIT-VECTOR
            (return-from normalize-type '(simple-bit-vector *)))
           (BASE-STRING
            (return-from normalize-type '(array base-char (*))))
           (SIMPLE-BASE-STRING
            (return-from normalize-type '(simple-array base-char (*))))
           (STRING
            (return-from normalize-type '(string *)))
           (SIMPLE-STRING
            (return-from normalize-type '(simple-string *)))
           ((nil)
            (return-from normalize-type nil))
           (t
            (unless (get type 'deftype-definition)
              (return-from normalize-type type)))))
        ((classp type)
         (return-from normalize-type
                      (if (eq (%class-name type) 'fixnum)
                          '(integer #.most-negative-fixnum #.most-positive-fixnum)
                          type)))
        ((and (consp type)
              (memq (%car type) '(and or not eql member satisfies mod values)))
         (cond ((or (equal type '(and fixnum unsigned-byte))
                    (equal type '(and unsigned-byte fixnum)))
                (return-from normalize-type '(integer 0 #.most-positive-fixnum)))
               (t
                (return-from normalize-type type)))))
  ;; Fall through...
  (let (tp i)
    (loop
      (if (consp type)
          (setf tp (%car type) i (%cdr type))
          (setf tp type i nil))
      (if (and (symbolp tp) (get tp 'deftype-definition))
          (setf type (apply (get tp 'deftype-definition) i))
          (return)))
    (case tp
      (INTEGER
       (return-from normalize-type (if i (cons tp i) tp)))
      (CONS
       (let* ((len (length i))
              (car-typespec (if (> len 0) (car i) t))
              (cdr-typespec (if (> len 1) (cadr i) t)))
         (unless (and car-typespec cdr-typespec)
           (return-from normalize-type nil))
         (when (eq car-typespec '*)
           (setf car-typespec t))
         (when (eq cdr-typespec '*)
           (setf cdr-typespec t))
         (return-from normalize-type (cons tp (list car-typespec cdr-typespec)))))
      (SIGNED-BYTE
       (if (or (null i) (eq (car i) '*))
           (return-from normalize-type 'integer)
           (return-from normalize-type
                        (list 'integer
                              (- (expt 2 (1- (car i))))
                              (1- (expt 2 (1- (car i))))))))
      (UNSIGNED-BYTE
       (if (or (null i) (eq (car i) '*))
           (return-from normalize-type '(integer 0 *)))
           (return-from normalize-type (list 'integer 0 (1- (expt 2 (car i))))))
      ((ARRAY SIMPLE-ARRAY)
       (unless i
         (return-from normalize-type (list tp '* '*)))
       (when (= (length i) 1)
         (setf i (append i '(*))))
       (setf (car i) (normalize-type (car i)))
       (return-from normalize-type (cons tp i)))
      (VECTOR
       (case (length i)
         (0
          (return-from normalize-type '(array * (*))))
         (1
          (setf (car i) (normalize-type (car i)))
          (return-from normalize-type (list 'array (car i) '(*))))
         (2
          (setf (car i) (normalize-type (car i)))
          (return-from normalize-type (list 'array (car i) (list (cadr i)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (SIMPLE-VECTOR
       (case (length i)
         (0
          (return-from normalize-type '(simple-array t (*))))
         (1
          (return-from normalize-type (list 'simple-array t (list (car i)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (BIT-VECTOR
       (case (length i)
         (0
          (return-from normalize-type '(bit-vector *)))
         (1
          (return-from normalize-type (list 'bit-vector (car i))))
         (t
          (error "Invalid type specifier ~S." type))))
      (SIMPLE-BIT-VECTOR
       (case (length i)
         (0
          (return-from normalize-type '(simple-bit-vector *)))
         (1
          (return-from normalize-type (list 'simple-bit-vector (car i))))
         (t
          (error "Invalid type specifier ~S." type))))
      (BASE-STRING
       (if i
           (return-from normalize-type (list 'array 'base-char (list (car i))))
           (return-from normalize-type '(array base-char (*)))))
      (SIMPLE-BASE-STRING
       (if i
           (return-from normalize-type (list 'simple-array 'base-char (list (car i))))
           (return-from normalize-type '(simple-array base-char (*)))))
      (SHORT-FLOAT
       (setf tp 'single-float))
      (LONG-FLOAT
       (setf tp 'double-float))
      (COMPLEX
       (cond ((null i)
              (return-from normalize-type '(complex *)))
             ((eq (car i) 'short-float)
              (return-from normalize-type '(complex single-float)))
             ((eq (car i) 'long-float)
              (return-from normalize-type '(complex double-float))))))
    (if i (cons tp i) tp)))

(defun caaaar (list) (car (car (car (car list)))))
(defun caaadr (list) (car (car (car (cdr list)))))
(defun caaddr (list) (car (car (cdr (cdr list)))))
(defun cadddr (list) (car (cdr (cdr (cdr list)))))
(defun cddddr (list) (cdr (cdr (cdr (cdr list)))))
(defun cdaaar (list) (cdr (car (car (car list)))))
(defun cddaar (list) (cdr (cdr (car (car list)))))
(defun cdddar (list) (cdr (cdr (cdr (car list)))))
(defun caadar (list) (car (car (cdr (car list)))))
(defun cadaar (list) (car (cdr (car (car list)))))
(defun cadadr (list) (car (cdr (car (cdr list)))))
(defun caddar (list) (car (cdr (cdr (car list)))))
(defun cdaadr (list) (cdr (car (car (cdr list)))))
(defun cdadar (list) (cdr (car (cdr (car list)))))
(defun cdaddr (list) (cdr (car (cdr (cdr list)))))
(defun cddadr (list) (cdr (cdr (car (cdr list)))))

;;; SOME, EVERY, NOTANY, NOTEVERY (adapted from ECL)

(defun some (predicate sequence &rest more-sequences)
  (setq more-sequences (cons sequence more-sequences))
  (do ((i 0 (1+ i))
       (l (apply #'min (mapcar #'length more-sequences))))
    ((>= i l) nil)
    (let ((that-value
           (apply predicate
                  (mapcar #'(lambda (z) (elt z i)) more-sequences))))
      (when that-value (return that-value)))))

(defun every (predicate sequence &rest more-sequences)
  (declare (optimize speed))
  (cond ((null more-sequences)
         (cond ((listp sequence)
                (dolist (x sequence t)
                  (unless (funcall predicate x)
                    (return nil))))
               (t
                (dotimes (i (length sequence) t)
                  (declare (type index i))
                  (unless (funcall predicate (elt sequence i))
                    (return nil))))))
        (t
         (setq more-sequences (cons sequence more-sequences))
         (do ((i 0 (1+ i))
              (l (apply #'min (mapcar #'length more-sequences))))
             ((>= i l) t)
           (unless (apply predicate (mapcar #'(lambda (z) (elt z i)) more-sequences))
             (return nil))))))

(defun notany (predicate sequence &rest more-sequences)
  (not (apply #'some predicate sequence more-sequences)))

(defun notevery (predicate sequence &rest more-sequences)
  (not (apply #'every predicate sequence more-sequences)))
