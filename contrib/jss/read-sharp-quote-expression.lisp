(in-package :jss)

(defclass sharp-quote-expression-reader (javaparser) ())

(defun read-sharp-java-expression (stream)
  (read-sharp-quote-expression
     (with-output-to-string (s)
       (loop with embedded-string = nil
	     for last = #\space then char
	     for char = (read-char stream)
	     until (and (char= char #\") 
			;; really end if: we've established embedded string and the peek is a space
			;; we're not about to start embedded string. We're about to start embedded string if next character isn't #\).
			;; we're not embedded-string and not about to start one
			(cond ((null (peek-char nil stream nil)) t) ;; eof
			      (embedded-string (system:whitespacep (peek-char nil stream))) ; embedded " needs "<space>" to end
			      ((find last ",(+=" :test 'char=)
			       (setq embedded-string t)
			       nil)
			      (t t)))
	     do
		(write-char char s)))))


(defun read-sharp-quote-expression (string)
  (multiple-value-bind (bindings de-lisped) (extract-lisp-expressions string)
    (let ((read (read-java-expression (make-instance 'sharp-quote-expression-reader) de-lisped)))
      (loop for (var nil) in bindings
	    do (setq read (cl-user::tree-replace (lambda(e) (if (equalp e (string var)) var e)) read )))
      (if bindings
	  `(let ,bindings ,read)
	  read))))

(defun extract-lisp-expressions (string)
  (let ((bindings nil))
    (let ((de-lisped
	    (cl-user::replace-all string "\\{(.*?)\\}" 
				  (lambda(match) 
				    (let ((replacevar (find-symbol-not-matching string (mapcar 'car bindings))))
				      (push (list replacevar (read-from-string match)) bindings)
				      (string replacevar)))
				  1)))
    (values bindings de-lisped))))

(defun find-symbol-not-matching (string already)
  (loop for candidate = (format nil "JSS_~a" (random 10000))
	until (and (not (member candidate already :test 'equalp :key 'string))
		   (not (search string already)))
	finally (return-from find-symbol-not-matching (intern candidate :jss))))
			    
(defun maybe-class (el)
  (if (and (symbolp el) (upper-case-p (char (string el) 0)) (not (eql (search "JSS_" (string el)) 0)))
      `(find-java-class ',el)
      (if (symbolp el)
	  (intern (string-upcase el))
	  el)))

(def-java-read ObjectCreationExpr sharp-quote-expression-reader ()
  `(new ',(process-node obj (#"getName" (#"getType" node))) ,@(mapcar (lambda(e) (process-node obj e)) (j2list (#"getArguments" node))))
  )

(def-java-read MethodCallExpr sharp-quote-expression-reader ()
  (let* ((scope1 (process-node obj (process-node obj (#"getScope" node))))
	 (how  (if (and (symbolp scope1) (not (null scope1)) (upper-case-p (char (string scope1) 0))) 
		   'jstatic
		   'jcall)))
    (if (and (symbolp scope1) (not (null scope1)) (upper-case-p (char (string scope1) 0)))
	(setq scope1 (find-java-class scope1)))
    `(,how ,(#"getIdentifier" (#"getName" node)) ,(or scope1 'this) ,@(mapcar 'maybe-class 
									      (mapcar (lambda(el) (process-node obj el))
										      (j2list (#"getArguments" node)))))
    ))


(def-java-read FieldAccessExpr sharp-quote-expression-reader ()
  (let ((scope (process-node obj (#"getScope" node))))
    (if (and (symbolp scope) (upper-case-p (char (string scope) 0)))
	`(get-java-field ',(process-node obj (#"getScope" node)) ,(#"getIdentifier" (#"getField" node)) t)
	`(get-java-field ,(maybe-class (process-node obj (#"getScope" node))) ,(#"getIdentifier" (#"getField" node)) t))))

(def-java-read ArrayAccessExpr sharp-quote-expression-reader ()
  (let ((index (process-node obj (#"getIndex" node))))
    (if (symbolp index) (setq index (intern (string-upcase index))))
    `(aref ,(process-node obj (#"getName" node)) ,index)))

(def-java-read ClassExpr sharp-quote-expression-reader ()
  (let ((name (process-node obj (#"getName" (#"getType" node)))))
    (if (eql (search "JSS_" (string name) :test 'equalp) 0)
	name
	`(find-java-class ',name))))

(def-java-read NameExpr sharp-quote-expression-reader ()
  (process-node obj (#"getName" node)))

