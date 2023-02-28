(in-package :steppenwolf)

(defun root-directory ()
  (asdf:system-relative-pathname :abcl
                                 "contrib/abcl-stepper/"))

(defun build ()
  (let ((ant-file
          (merge-pathnames "build.xml" (root-directory))))
    (abcl-build:ant/call  ant-file "compile")))

(defun init ()
  ;; after running BUILD once per installation
  (let ((build (merge-pathnames "build/" (root-directory))))
    (java:add-to-classpath build)
    (when 
        (jss:find-java-class "InterpretedStepper")
      (let ((methods 
              (#"getDeclaredMethods" (jss:find-java-class "InterpretedStepper"))))
      (setf
       (jss:get-java-field 'org.armedbear.lisp.Lisp "stepperHook")
       ;;; TODO: fixme.  Constructing the argument matching is tedious, this was faster
       (elt methods 7))))))
    
(provide 'steppenwolf)
