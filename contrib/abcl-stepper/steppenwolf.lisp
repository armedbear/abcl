(in-package :steppenwolf)

(defun build ()
  ;;; TODO Use ABCL-BUILD/ANT
  )

(defun init ()
  ;; after running BUILD once per installation
  (let ((build 
          (asdf:system-relative-pathname :abcl
                                          "contrib/steppenwolf/build/")))
    (java:add-to-classpath build)
    (when 
        (jss:find-java-class "InterpretedStepper")
      (let ((methods 
              (#"getDeclaredMethods" (jss:find-java-class "InterpretedStepper"))))
      (setf
       (jss:get-java-field 'org.armedbear.lisp.Lisp "stepperHook")
       ;;; TODO: fixme.  Constructing the argument matching is tedious, this was faster
       (elt methods 7))))))


    
(provide #:steppenwolf)
