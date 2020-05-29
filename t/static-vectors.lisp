(in-package :cl-user)

(require :abcl-contrib)
(require :jss)

(prove:plan 1)
(prove:ok
 (let* ((bytebuffer
          (#"allocate" 'nio.ByteBuffer 21))
        (vector
          (ext:make-bytebuffer-byte-vector bytebuffer)))
   (and
    vector
    (typep vector '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (21))))))
(prove:finalize)
