(in-package :jss)

(defun tree-replace (replace-fn tree)
  "create new tree replacing each element with the result of calling replace-fn on it"
  (labels ((tr-internal (tree)
             (cond ((atom tree) (funcall replace-fn tree))
                   (t (let ((replacement (funcall replace-fn tree)))
                        (if (eq replacement tree)
                            (mapcar #'tr-internal tree)
                            replacement))))))
    (tr-internal tree)))

(defun replace-all (string regex function &rest which)
  (let ((matcher (#"matcher" (if (java-object-p regex) regex (#"compile" 'java.util.regex.pattern regex)) string))
        (sb (new 'stringbuffer)))
    (with-constant-signature ((append "appendReplacement")) 
      (loop for found = (#"find" matcher)
            while found 
            do
               (#"appendReplacement" matcher sb (apply function  
                                                       (loop for g in which collect
                                                                            (#"group" matcher g)))))
      )
    (#"appendTail" matcher sb)
    (#"toString" sb)))

(defparameter *regex-chars-needing-escape* 
  (concatenate 'string (string #\tab) ".[]()\\?*+{}^$&|"))

;; add here even though in util
(defun split-at-char (string char)
  (let ((regex (string char)))
    (when (simple-string-search regex *regex-chars-needing-escape*)
      (setq regex (system::concatenate-to-string (list "\\" regex))))
    (with-constant-signature ((split "split") (tostring "toString"))
      (loop for v across (split string regex) collect (tostring v)))))

(defun all-matches (string regex &rest which)
  (declare (optimize (speed 3) (safety 0)))
  (and string
       (let ((matcher (#"matcher" 
		       (if (stringp regex)
			   (#"compile" 'java.util.regex.pattern regex)
			   regex)
		       string)))
	 (with-constant-signature ((mfind "find") (mgroup "group"))
	   (loop while (mfind matcher) 
	      collect (loop for g in which collect
			   (mgroup matcher g)))))))

(define-compiler-macro all-matches
    (&whole form  string regex &rest which)
  "Compile constant regex to pattern at compile time"
  (cond ((stringp regex)
	 `(all-matches ,string (load-time-value (#"compile" 'java.util.regex.Pattern ,regex))
		       ,@which))
        (t form)))
