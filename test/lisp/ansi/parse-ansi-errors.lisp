;;;; $Id$
;;;;
;;;; Parse ANSI test results from a s-expr database, allowing queries
;;;; to show differences.
;;;;
;;;; 'cuz I get lost after comparing about five items in a list
;;;;

#|

To use 

1.  Create a "database" of test results consisting of s-exps.  A
    default database is in 'failures'.
   
    The s-exprs have the form:
   
   (compileit|doit <version> :id <id> [:<key> <value>] 
     (<failing test results>))

where

   compileit|doit   The symbol 'compileit' or 'doit' depending on
                    whether the compiled or interpreted tests were run.

   version          A symbol identifying the version of source of the
                    tests (i.e. r12506 or 0.18.0)

   :id <id>         <id> is a symbol identifying the environment for
                    the tests

   :key <value>     Additional key-value pairs

   <failing test results>
                    The list of symbols failing the tests.

An example on an entry:

  (doit r12506 :id jupiter
      :uname "i386-pc-solaris2.11"  :jvm "jdk-1.6.0_13"
   (REINITIALIZE-INSTANCE.ERROR.1 DEFGENERIC.ERROR.20
    DEFGENERIC.ERROR.21 DEFGENERIC.30 CALL-NEXT-METHOD.ERROR.1
    CALL-NEXT-METHOD.ERROR.2 DEFMETHOD.ERROR.14 DEFMETHOD.ERROR.15
    INVOKE-DEBUGGER.1 MAKE-CONDITION.3 MAKE-CONDITION.4
    DELETE-PACKAGE.5 DELETE-PACKAGE.6 MAP.48 TYPE-OF.1 TYPE-OF.4
    CHAR-UPCASE.2 CHAR-DOWNCASE.2 FRESH-LINE.5 PRINT.RANDOM-STATE.1
    PPRINT-FILL.14 PPRINT-FILL.15 PPRINT-LINEAR.14 PPRINT-TABULAR.13
    PPRINT-LOGICAL-BLOCK.17 PPRINT-POP.7 PPRINT-POP.8
    FORMAT.LOGICAL-BLOCK.CIRCLE.1 FORMAT.LOGICAL-BLOCK.CIRCLE.2
    FORMAT.LOGICAL-BLOCK.CIRCLE.3 FORMAT.JUSTIFY.30 FORMAT.JUSTIFY.32
    WITH-STANDARD-IO-SYNTAX.23))

2.  Run (PARSE [<filename>]) on the file of your database.  Without an
    argument, the default database is read.

3.  Then differences between versions can be queried via REPORT

   CL-USER> (REPORT 'compileit '0.18.0 'r13590) 

|#

(in-package :abcl.test.ansi)

(defvar *doit* (make-hash-table))
(defvar *compileit* (make-hash-table))
(defvar *id* (make-hash-table))

(defun reset ()
  (clrhash *doit*)
  (clrhash *compileit*)
  (clrhash *id*))

(defun get-hash-table (test)
  (let ((name (symbol-name test)))
    (when (string-equal name "doit")
      (return-from get-hash-table *doit*))
    (when (string-equal name "compileit")
      (return-from get-hash-table *compileit*))))

(defvar *default-database-file* 
  (asdf:system-relative-pathname
   :abcl "test/lisp/ansi/ansi-test-failures"))

(defun parse (&optional (file *default-database-file*))
  "Parse the ansi test database present at *DEFAULT-DATABASE-FILE*.

Optionally the file to parse may be specified by the FILE argument."
  (format t "Parsing test report database from ~A~%" *default-database-file*)
  (with-open-file (s file :direction :input)
    (do ((form (read s) (read s nil nil)))
         ((null form))
      (destructuring-bind (test version &rest rest) form
        (let ((args) (failures) (id))
          (dolist (arg rest)
            (if (typep arg 'cons)
                (setf failures arg)
                (push arg args)))
          (setf args (nreverse args))
          (unless (getf args :id)
            (push 'noid args) 
            (push :id args))
          (setf id (getf args :id))
          (unless (gethash version (get-hash-table test))
            (setf (gethash version (get-hash-table test))
                  (make-hash-table)))
          (if (> (length args) 2)
              (setf (gethash id *id*) args)
              (if (null (gethash id *id*))
                  (setf (gethash id *id*) args)))
          (setf (gethash id
                         (gethash version (get-hash-table test)))
                failures))))))

(defun versions (test)
  (loop :for key :being :the :hash-keys :of (get-hash-table test)
     :collecting key))

(defun report-versions (&optional (test 'compileit))
  (format t "~A has the following versions:~%~A~%" 
          test (versions test))
  (values))

(defun get-failures (test version)
  (gethash version (get-hash-table test)))

(defun difference (failures-1 failures-2)
  "Report the set-difference between the lists of FAILURES-1 and FAILURES-2"
  (list 
   (list (length failures-1)
         (set-difference failures-1 failures-2))
   (list (length failures-2)
         (set-difference failures-2 failures-1))))

(defun generate-report (test version-1 version-2)
  (flet ((list-results (hash-table)
           (loop 
              :for key :being :the :hash-key :of hash-table
              :using (:hash-value value)
              :collecting (list key value))))
    (let ((entries-1 (list-results (get-failures test version-1)))
          (entries-2 (list-results (get-failures test version-2))))
      (loop :for (id-1 failure-1) :in entries-1
         :appending (loop :for (id-2 failure-2) :in entries-2
                        :collecting (list (cons id-1 id-2)
                                          (difference failure-1
                                                      failure-2)))))))

(defun deprecated/report (test version-1 version-2)
  (report version-1 version-2 :test test))
  
(defun report (version-1 version-2 &key (test 'compileit))
  "Report on the difference of test failures for TEST between VERSION-1 and VERSION-2.

TEST is symbol with a value of 'DOIT specifying the interpreted
version of the tests, or 'COMPILEIT specifiying the compiled verision of the tests.

VERSION-1 and VERSION-2 are symbols of two versions contained in the test database."

  (let ((reports (generate-report test version-1 version-2)))
    (dolist (report reports)
      (destructuring-bind ((id1 . id2) ((total-failures1 diff-1->2)
                                        (total-failures2 diff-2->1)))
          report
        (when diff-1->2
          (format t "~A[~A] --> ~A[~A] additional failures:~%~A~%" 
                version-1 id1 version-2 id2 diff-1->2))
        (when diff-2->1
          (format t "~A[~A] --> ~A[~A] additional failures:~%~A~%" 
                  version-2 id2 version-1 id1 diff-2->1))))))
            
(defun full-report (version-1 version-2)
  (let ((interpreted-reports (generate-report 'doit version-1 version-2))
        (compiled-reports (generate-report 'compileit version-1 version-2)))
    (dolist (interpreted interpreted-reports)
      (destructuring-bind ((id1 . id2) ((total-failures1 diff-1->2)
                                        (total-failures2 diff-2->1)))
          interpreted
        (format t "~2&Interpreted~%")
        (format t "~&~20<~A-~A~>~20<~A-~A~>" id1  version-1 id2 version-2)
        (format t "~&~20<~A failures~>~20<~A failures~>" 
                total-failures1 total-failures2)
        (format t "~&~A-~A:~&  ~A" id1 version-1 diff-1->2)
        (format t "~&~A-~A:~&  ~A" id2 version-2 diff-2->1)))
    (dolist (compiled compiled-reports)
      (destructuring-bind ((id1 . id2) ((total-failures1 diff-1->2)
                                        (total-failures2 diff-2->1)))
          compiled
        (format t "~2&Compiled~%")
        (format t "~&~20<~A-~A~>~20<~A-~A~>" id1  version-1 id2 version-2)
        (format t "~&~20<~A failures~>~20<~A failures~>" 
                total-failures1 total-failures2)
        (format t "~&~A-~A:~&  ~A" id1 version-1 diff-1->2)
        (format t "~&~A-~A:~&  ~A" id2 version-2 diff-2->1)))))

(defun report-compiled (version-1 version-2)
  (let ((compiled-reports (generate-report 'compileit version-1 version-2)))
    (dolist (interpreted interpreted-reports)
      (destructuring-bind ((id1 . id2) ((total-failures1 diff-1->2)
                                        (total-failures2 diff-2->1)))
          interpreted
        (format t "~2&Interpreted~%")
        (format t "~&~20<~A-~A~>~20<~A-~A~>" id1  version-1 id2 version-2)
        (format t "~&~20<~A failures~>~20<~A failures~>" 
                total-failures1 total-failures2)
        (format t "~&~A-~A:~&  ~A" id1 version-1 diff-1->2)
        (format t "~&~A-~A:~&  ~A" id2 version-2 diff-2->1)))
    (dolist (compiled compiled-reports)
      (destructuring-bind ((id1 . id2) ((total-failures1 diff-1->2)
                                        (total-failures2 diff-2->1)))
          compiled
        (format t "~2&Compiled~%")
        (format t "~&~20<~A-~A~>~20<~A-~A~>" id1  version-1 id2 version-2)
        (format t "~&~20<~A failures~>~20<~A failures~>" 
                total-failures1 total-failures2)
        (format t "~&~A-~A:~&  ~A" id1 version-1 diff-1->2)
        (format t "~&~A-~A:~&  ~A" id2 version-2 diff-2->1)))))

      
    
  
    
        
