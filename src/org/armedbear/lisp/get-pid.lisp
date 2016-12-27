(in-package :extensions)

(export '(get-pid)
        :extensions)

(defun get-pid ()
  "Get the process identifier of this lisp process. 

Used to be in SLIME but generally useful, so now back in ABCL proper."
  (handler-case
      (let* ((runtime
              (java::jstatic "getRuntime" "java.lang.Runtime"))
             (command
              (java::jnew-array-from-array
               "java.lang.String" #("sh" "-c" "echo $PPID")))
             (runtime-exec-jmethod
              ;; Complicated because java.lang.Runtime.exec() is
              ;; overloaded on a non-primitive type (array of
              ;; java.lang.String), so we have to use the actual
              ;; parameter instance to get java.lang.Class
              (java::jmethod "java.lang.Runtime" "exec"
                            (java::jcall
                             (java::jmethod "java.lang.Object" "getClass")
                             command)))
             (process
              (java::jcall runtime-exec-jmethod runtime command))
             (output
              (java::jcall (java::jmethod "java.lang.Process" "getInputStream")
                          process)))
         (java::jcall (java::jmethod "java.lang.Process" "waitFor")
                     process)
	 (loop :with b :do
	    (setq b
		  (java::jcall (java::jmethod "java.io.InputStream" "read")
			      output))
	    :until (member b '(-1 #x0a))	; Either EOF or LF
	    :collecting (code-char b) :into result
	    :finally (return
		       (parse-integer (coerce result 'string)))))
    (t () 0)))



