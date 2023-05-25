(in-package :system/posix-syscalls)

(defun c-library-reference ()
  (#"getInstance" 'com.sun.jna.NativeLibrary "c"))

(defun getenv (variable)
  (let ((found (#"invokePointer"
                (#"getFunction" (c-library-reference) "getenv")
                (java:jnew-array-from-list "java.lang.Object" (list variable)))))
    (when found
      (#"getString" found 0))))

(defun putenv (variable value)
  (let ((variable=value
          (java:jnew-array-from-list "java.lang.Object"
                                     (list
                                      (format nil "~a=~a"
                                              variable  value)))))
          (#"invokeInt"
           (#"getFunction" (c-library-reference) "putenv")
           variable=value)))

(defun unsetenv (variable)
  (when
      (= 0
         (#"invokeInt"
          (#"getFunction" (c-library-reference) "unsetenv")
          (java:jnew-array-from-list "java.lang.Object" (list variable))))
    t))
      
  


