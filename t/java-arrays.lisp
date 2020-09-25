(in-package :cl-user)

(let ((tests '(("byte"
                #(0 255 128 127))
               ("short"
                #(0 255 128 127))
               ("char"
                #(0 255 128 127))
               ("int"
                #(0 255 128 127))
               ("long"
                #(0 255 128 127))
               ("float"
                #(0 255 128 127))
               ("double"
                #(0 255 128 127))
               ("boolean"
                #(0 255 128 127)))))
  (prove:plan (length tests))
  (loop :for (array-primitive-type initial-contents) :in tests
        :doing 
           (prove:ok
            (java:jnew-array-from-array array-primitive-type initial-contents)
            (format nil "java array from ~a" array-primitive-type))))


(prove:finalize)
