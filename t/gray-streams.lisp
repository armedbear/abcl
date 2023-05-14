;;;; Tests for <https://github.com/armedbear/abcl/issues/512>
(in-package :cl-user)

(unless (ignore-errors 
         (asdf:make :trivial-gray-streams))
  (asdf:make :abcl-quicklisp)
  (ql:quickload :trivial-gray-streams))


(defclass out-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ())

(defclass in-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ())

(prove:plan 4)

(prove:ok
 (make-two-way-stream (make-instance 'in-stream) (make-instance 'out-stream))
 "Create TWO-WAY-STREAM from Gray streams")

(prove:ok 
 (make-broadcast-stream (make-instance 'out-stream))
 "Create BROADCAST-STREAM from Gray stream")

(prove:ok
 (make-concatenated-stream (make-instance 'in-stream))
 "Create CONCATENATED-STREAM from Gray stream")

(prove:ok
 (make-echo-stream (make-instance 'in-stream) (make-instance 'out-stream))
 "Create ECHO-STREAM from Gray streams")

(prove:finalize)
