(in-package :steppenwolf)

(defun root-directory ()
  (asdf:system-relative-pathname :abcl
                                 "contrib/abcl-stepper/"))
(defun build ()
  (let ((ant-file
          (merge-pathnames "build.xml" (root-directory))))
    (format *standard-output*
            "Invoking compile in ~a" ant-file)
    (abcl-build:ant/call ant-file "compile")))

(defun init ()
  ;; after running BUILD once per installation
  (init-1)
  (let ((class
          (jss:find-java-class "InterpretedStepper")))
    (when class 
      (let ((methods 
              (#"getDeclaredMethods" class)))
        #+nil
        (setf
         (jss:get-java-field 'org.armedbear.lisp.Lisp "stepperHook")
       ;;; TODO: fixme.  Constructing the argument matching is tedious, this was faster
         (elt methods 7))
        methods))))


(defun init-1 ()
  (let ((build (merge-pathnames "build/" (root-directory))))
    (java:add-to-classpath build)))


    
(provide 'steppenwolf)
