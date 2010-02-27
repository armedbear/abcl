;;;; $Id$
;;;; Parse ANSI test list
;;;;
;;;; 'cuz I get lost after comparing about five items in a list
;;;;

#|

To use 

1.  create a "database" of test results consisting of S-exp of form
   
   (compileit|doit <version> (<failing test results>))

where

   compileit|doit   The symbol 'compileit' or 'doit' depending on
                    whether the compiled or interpreted tests were run.

   version          A symbol identifying the version of source of the
                    tests.

   <failing test results>
                    The list of symbols failing the tests.

An example: 

(compileit 0.18.1 (REINITIALIZE-INSTANCE.ERROR.1 DEFGENERIC.ERROR.20
DEFGENERIC.ERROR.21 DEFGENERIC.30 CALL-NEXT-METHOD.ERROR.1
CALL-NEXT-METHOD.ERROR.2 DEFMETHOD.ERROR.14 DEFMETHOD.ERROR.15
MAKE-CONDITION.3 MAKE-CONDITION.4 DELETE-PACKAGE.5 DELETE-PACKAGE.6
MAP.48 TYPE-OF.1 TYPE-OF.4 CHAR-UPCASE.2 CHAR-DOWNCASE.2
ENSURE-DIRECTORIES-EXIST.8 FRESH-LINE.5 MAKE-BROADCAST-STREAM.8
PRINT.BACKQUOTE.RANDOM.14 PRINT.RANDOM-STATE.1 PPRINT-FILL.14
PPRINT-FILL.15 PPRINT-LINEAR.14 PPRINT-TABULAR.13
PPRINT-LOGICAL-BLOCK.17 PPRINT-POP.7 PPRINT-POP.8
FORMAT.LOGICAL-BLOCK.CIRCLE.1 FORMAT.LOGICAL-BLOCK.CIRCLE.2
FORMAT.LOGICAL-BLOCK.CIRCLE.3 FORMAT.JUSTIFY.30 FORMAT.JUSTIFY.32
WITH-STANDARD-IO-SYNTAX.23 TRACE.8)).

2.  Run (PARSE <filename>) on the file of your database.

3.  Then differences between versions can be queried via DIFFERENCE

   CL-USER> (difference 'compileit '0.18.0 'r13590) 

|#

(defvar *doit* (make-hash-table))
(defvar *compileit* (make-hash-table))

(defun get-hash-table (test)
  (getf `(doit ,*doit* compileit ,*compileit*) test))  

(defun parse (&optional (file #p"failures")
  (with-open-file (s file :direction :input)
    (do ((form (read s) (read s nil nil)))
         ((null form))
      (destructuring-bind (test version failures) form
        (setf (gethash version 
                       (get-hash-table test))
              failures)))))

(defun versions (test)
  (loop :for key :being :the :hash-keys :of (get-hash-table test)
     :collecting key))

(defun difference (test version-1 version-2) 
  (let ((failures-1 (gethash version-1 (get-hash-table test)))
        (failures-2 (gethash version-2 (get-hash-table test))))
    (format t "~A: ~A failures~% ~A~%" 
            version-1 (length failures-1) (set-difference failures-1 failures-2))
    (format t "~A: ~A failures~% ~A~%" 
            version-2 (length failures-2) (set-difference failures-2 failures-1)))
  (values))
            
  

         
        