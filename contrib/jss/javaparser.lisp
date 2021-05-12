(in-package :jss)

(defvar *class-to-last-component* (make-hash-table :test 'equalp))

(defclass javaparser () ())

(defmacro def-java-read (ast-class class fields &body body)
  (let ((jclass (find-java-class (concatenate 'string "com.github.javaparser.ast.expr." (string ast-class)))))
    `(progn
       (setf (gethash ,jclass *class-to-last-component*) ',ast-class)
       (defmethod ,ast-class ((obj ,class) node &optional
                              ,@(loop for field in fields
                                      collect `(,(intern (string-upcase field)) (get-java-field node ,field t))))
           ,@body))))

(defvar *object-for-this* (new 'lang.object))

(defmethod get-optional ((r javaparser) node)
  (if (equal node (load-time-value (#"empty"  'java.util.Optional ))) nil (#"get" node)))

(defmethod process-node ((r javaparser) node)
  (when (jinstance-of-p  node "java.util.Optional")
    (setq node (get-optional r node)))
  (when (null node)
    (return-from process-node nil))
  (if (java-object-p node)
      (funcall (gethash (jobject-class node) *class-to-last-component*)  r node)
      node))

(defmethod read-java-expression ((r javaparser) expression)
  `(let ((this *object-for-this*))
     (declare (ignorable this))
     ,(process-node r (#"parseExpression" 'javaparser expression))))

(def-java-read LongLiteralExpr javaparser ()
  (read-from-string (#"replaceFirst" (#"getValue" node) "L" "")))

(def-java-read BooleanLiteralExpr javaparser ()
  (if (equal (#"getValue" node) "true") t nil))

(def-java-read IntegerLiteralExpr javaparser nil
 (parse-integer (#"getValue" node)))

(def-java-read DoubleLiteralExpr javaparser nil
  (let ((raw (#"getValue" node)))
    (setq raw (#"replaceAll" raw "_" ""))
    (if (#"matches" raw ".*[dD]$")
        (read-from-string (#"replaceFirst" (subseq raw 0 (1- (length raw))) "e" "d"))
        (if (#"matches" raw ".*[fF]$")
            (read-from-string (subseq raw 0 (1- (length raw))))
            (read-from-string raw)))))

(def-java-read CharLiteralExpr javaparser nil
  (#"getValue" node))

(def-java-read StringLiteralExpr javaparser nil
  (#"getValue" node))

(def-java-read NullLiteralExpr javaparser nil
  +null+)

(def-java-read SimpleName javaparser ()
  (let ((symbol (intern (#"getIdentifier" node))))
    symbol))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-invoke/javaparser (stream char arg) 
    (if (eql arg 1)

        (if (ignore-errors
              (jclass "com.github.javaparser.ParseStart"))         ;; chosen randomly, TODO memoize
            (read-sharp-java-expression stream)
            ;; Deal with possiblity of not loading jar
            (error "Cannot load javaparser code needed for the #1 macro"))
        (read-invoke stream char arg)))
  (set-dispatch-macro-character #\# #\" 'read-invoke/javaparser))


