(defun write-data (&key (file "build-metrics.data"))
  (let ((results (nreverse (parse))))
    (format t "Writing gnuplot file to ~A.~%" file)
    (with-open-file (s file :direction :output :if-exists :supersede)
      (format s "# hg-changeset-number svn-revision date abcl.jar-size user-build-time~%")
      (dolist (line results)
        (format s "~A~%" line)))))

(defun parse (&key (file "build-metrics.out"))
  (let (result)
    (format t "Reading raw build metrics from ~A.~%" file)
    (with-open-file (s file)
      (loop
         (when (eq (peek-char nil s nil 'EOF) 'EOF)
           (return result))
         (let ((record (read-record s)))
           (when (null record)
             (return result))
             (flet ((get-value (key)
                      (cdr (assoc key record :test 'equal))))
               (unless (string-equal (get-value "BUILD")
                                     "FAILED")
                 (let ((changeset (get-value "changeset"))
                       (date (get-value "date"))
                       (svn (get-value "svn")))
                   ;;; Just include the part before the colon
                   (setf changeset (subseq changeset 
                                           0 (search ":" changeset)))
                   ;;; Just include the day
                   (setf date (subseq date 
                                      0 (search " " date)))
                   ;;; String the preceeding "r"
                   (setf svn (subseq svn 1))
                   (push (format nil "~A ~A ~A ~A ~A"
                                 changeset
                                 svn 
                                 date 
                                 (get-value  "abcl.jar-size")
                                 (get-value "user"))
                         result)))))))))
        
(defun read-record (s)
  (let (result)
    (handler-case
        (let ((begin (read-line s)))
          (unless (string-equal begin "-----")
            (error "Stream ~a not at beginning of record: ~a" s begin))
          (loop 
             (when (equal (peek-char nil s) #\-) (return-from read-record result))
             (let* ((line (read-line s))
                    (space (search " " line)))
               (when (numberp space)
                 (let ((key (subseq line 0 space))
                       (value (subseq line (1+ space))))
                   (when (equal #\: (char key (1- (length key))))
                     (setf key (subseq key 0 (1- (length key)))))
                   (push (cons key value) result)))))
          result)
      (end-of-file () (return-from read-record result)))))






    