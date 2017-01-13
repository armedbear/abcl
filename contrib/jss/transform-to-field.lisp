(in-package :jss)

;; JSS syntax for fields
;; #"<thing>.<field>"
;;
;; <thing> is either {<lisp expression>} or a class name or abbreviation that find-java-class can use
;;   If <thing> is a lisp expression, then it is evaluated (in the lexical environment) and used as an instance
;;   If <thing> is a class name the result of find-java-class is used and a static field access is done.
;;
;; <field> is either {<lisp expression} or string
;;   If <field> is a lisp expression it should evaluate to a string that names a field
;;   If <field> is a string (no quotes) it is used as the field name
;;
;; eg. #"foo.bar.baz" -> (get-java-field (find-java-class 'foo.bar) "baz" t)
;;     #"{foo}.baz" -> (get-java-field (find-java-class foo) "baz" t)


(defun jss-transform-to-field (string)
  (let* ((pattern (#"compile" 'java.util.regex.Pattern "(.*)\\.([^.]+)$"))
	 (matcher (#"matcher" pattern string)))
    (#"find" matcher)
    (let ((parts (list (#"group" matcher 1) (#"group" matcher 2))))
      (check-class-or-eval (first parts))
      (check-field-or-eval (second parts))
      (apply 'field-access-expression parts))))

;; http://stackoverflow.com/questions/5205339/regular-expression-matching-fully-qualified-class-names
(defun check-class-or-eval (string)
  (assert 
   (or (#"matches" string "^((\\p{javaJavaIdentifierStart}(\\p{javaJavaIdentifierPart})*)+)(\\.\\p{javaJavaIdentifierStart}(\\p{javaJavaIdentifierPart})*)*$")
       (#"matches" string "^\\{.+}$")) (string)
       "inside #\"..\" expected either an abbreviated class name or an expression surrounded by {}. Found: #~s" string))

(defun check-field-or-eval (string)
  (assert (or (#"matches" string "^(\\p{javaJavaIdentifierStart}(\\p{javaJavaIdentifierPart})*)+$")
	      (#"matches" string "^\\{.+\\}$"))
	  (string)
	  "inside #\"..\" expected either a field name or an expression surrounded by {}. Found: #~s" string))

(defun field-access-expression (thing field)
  `(get-java-field ,(if (char= (char thing 0) #\{)
			(intern (string-upcase (subseq thing 1 (- (length thing) 1))))
			`(load-time-value (find-java-class ,thing)))
		   ,(if (char= (char field 0) #\{)
			(intern (string-upcase (subseq field 1 (- (length field) 1))))
			field)
		   t))
