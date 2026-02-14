(require :asdf)

(require :abcl-contrib)

(asdf:make :quicklisp-abcl)

;; test for the correct loading of ABCL-JVM-STEPPER and make sure it can also instrument correctly the code
(require :abcl-jvm-stepper)

(progn (defun loop-1 (a b)
         (loop :for i :below a
               :collect (list a b)))

       (defun loop-2 (a)
         (loop :for i :below a
               :collect i))

       (defun loop-3 (n &optional (times 1))
         (loop :for i :below times
               :collect times)
         (loop-4 (+ n times)))

       (defun loop-4 (k)
         (loop :for i :below k
               :collect (cons (/ i k) (+ i k))))

       (defun test-next (n)
         (loop-1 (1+ n) n)
         (loop-2 (1- n))
         (loop-3 n 3)
         ;; quit (q) here
         (defparameter *test-next-var* (loop :for i :below 7
                                             :collect i))))

(progn (jstepper:instrument-compile-function 'loop-1)
       (jstepper:instrument-compile-function 'loop-2)
       (jstepper:instrument-compile-function 'loop-3)
       (jstepper:instrument-compile-function 'loop-4)
       (jstepper:instrument-compile-function 'test-next))

(ql:quickload :alexandria)

(jstepper:instrument-compile-call (asdf:load-system :alexandria :force t))
