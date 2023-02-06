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
  "Without a specified JAVA-THREAD, return the context classloader of the current thread

Otherwise return the context classloader of specified JAVA-THREAD."
  (jcall "getContextClassLoader"
         (if java-thread
             java-thread
             (threads::get-java-thread))))

(defsetf context-classloader (&optional java-thread) (classloader)
  `(progn
     (jcall "setContextClassLoader"
            (if ,java-thread
                ,java-thread
                (threads::get-java-thread))
            ,classloader)
     ,classloader))

(defmacro with-classloader (&optional (classloader) &body body)
  "Call BODY with optional CLASSLOADER argument set as the context classloader

If the CLASSLOADER is not specified, the default classloader is set as
the context classloader."
  `(let ((original-context (context-classloader)))
     (prog2 
         (setf (context-classloader)
               ,(if classloader
                    classloader
                    (classloader)))
         ,@body
       (when original-context
         (setf (context-classloader) original-context)))))
           
