;;; Copyright (C) 2023 Mark Evenson
(in-package java)
(export '(classloader
          context-classloader
          with-classloader))

(defun classloader (&optional java-object)
  "Without a specified JAVA-OBJECT, return the classloader of the current one

Otherwise return the classloader of the specified JAVA-object."
  (if java-object
      (jcall "getClassLoader" java-object)
      (get-default-classloader)))

(defun context-classloader (&optional java-thread)
  "Without a specified JAVA-THREAD, return the context classloader of the current one

Otherwise return the context classloader of specified JAVA-THREAD."
  (jcall "getContextClassLoader"
         (if java-thread
             java-thread
             (threads::get-java-thread))))

;;; TODO forward reference to JAVA and/or JSS cleanly?
(defmacro with-classloader ((thread-context) &body body)
  "Call BODY with classloader associated with THREAD-CONTEXT

If no THREAD-CONTEXT is specified, set the context classloader of the
current thread."
  `(let ((original-context
           (context-classloader)))
     (jcall "setContextClassLoader"
            (if ,thread-context
                ,thread-context 
                (threads::get-java-thread)))
     ,@body
     (when original-context 
       (jcall "setContextClassLoader" original-context))))
           
