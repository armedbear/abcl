(in-package :jss)

;; JSS syntax for fields
;; #"[<scope>]<thing>.<field>"
;;
;; <scope> is empty or "==". scope is only paid attention to when <field> is a literal string
;; 
;; <thing> is either {<lisp expression>} or a class name or abbreviation that find-java-class can use
;;   If <thing> is a lisp expression, then it is evaluated (in the lexical environment) and used as an instance
;;     when <scope> is "==" you promise that instance will always be of the same class, and so field lookup
;;     is done once and cached. 
;;   If <thing> is a class name the result of find-java-class is used and a static field access is done.
;;     when <scope> is "==" you promise the static field is final and so the result is wrapped in (load-time-value  ...)
;;
;; <field> is either {<lisp expression} or string
;;   If <field> is a lisp expression it should evaluate to a string that names a field
;;   If <field> is a string (no quotes) it is used as the field name
;;
;; eg. #"foo.bar.baz" -> (get-java-field (find-java-class 'foo.bar) "baz" t)
;;     #"{foo}.baz" -> (get-java-field (find-java-class foo) "baz" t)
;;     #"==foo.baz" -> (load-time-value (get-java-field (find-java-class "foo") "bar" t))
;;     #"=={foo}.baz" -> TL;DR (only look up baz field once based on class of foo, and cache)

(defun jss-transform-to-field (string sharp-arg)
  (let* ((pattern (#"compile" 'java.util.regex.Pattern "((==){0,1})(.*)\\.([^.]+)$"))
         (matcher (#"matcher" pattern string)))
    (#"find" matcher)
    (let ((parts (list (#"group" matcher 3) (#"group" matcher 4)))
          (scope (#"group" matcher 1)))
      (check-class-or-eval (first parts))
      (check-field-or-eval (second parts))
      (apply 'field-access-expression sharp-arg scope parts ))))

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

(defun field-access-expression (sharp-arg scope thing field )
  (if (and (not (char= (char thing 0) #\{)) (not (char= (char field 0) #\{)))
      (static-field-ref-transform thing field sharp-arg scope)
      (if (and (equal scope "==") (char= (char thing 0) #\{) (not (char= (char field 0) #\{)))
          (always-same-signature-field-ref-transform sharp-arg thing field)
          `(get-java-field ,(if (char= (char thing 0) #\{)
                                (read-from-string (subseq thing 1 (- (length thing) 1)))
                                `(load-time-value (find-java-class ,thing)))
                           ,(if (char= (char field 0) #\{)
                                (read-from-string (subseq field 1 (- (length field) 1)))
                                field)
                           t))))

;; If a class name and explicit field name we can look everything up at load time
(defun static-field-ref-transform (class field sharp-arg scope)
  (if (equal scope "==")
      `(load-time-value (get-java-field (find-java-class ,class) ,field t))
      `(,(if (eql sharp-arg 0) 'jcall-raw 'jcall) 
        (load-time-value (jmethod "java.lang.reflect.Field" "get" "java.lang.Object"))
        (load-time-value 
         (let ((jfield (find-declared-field ,field (find-java-class ,class))))
           (#"setAccessible" jfield t)
           jfield))
        (load-time-value (find-java-class ',class)))))

;; 1 case: =={var}.foo
;; Globally cache the field accessor for the first value of {var}. Subsequent calls ignore the class of var.
(defun always-same-signature-field-ref-transform (sharp-arg object field)
  (let ((cached (make-symbol (format nil "CACHED-FIELD-field")))
        (object (intern (string-upcase (subseq object 1 (- (length object) 1))))))
    `(,(if (eql sharp-arg 0) 'jcall-raw 'jcall)
      (load-time-value (jmethod "java.lang.reflect.Field" "get" "java.lang.Object"))
      (locally (declare (special ,cached))
        (if (boundp ',cached)
            ,cached
            (progn (setq ,cached 
                         (find-declared-field ,field (jcall (load-time-value (jmethod "java.lang.Object" "getClass")) ,object)))
                   (jcall (load-time-value (jmethod "java.lang.reflect.Field" "setAccessible" "boolean")) ,cached t)
                   ,cached)))
      ,object)))





 
