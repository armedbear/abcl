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
