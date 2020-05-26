(in-package :ext)

(defvar *buffer-allocation* :primitive-array
  "The current buffer allocation strategy.")

(defun choose-buffer-strategy (&key (new-default :nio new-default-p))
  "Return current choices for buffer allocation strategy that can then be chosen as a :new-default strategy

Not currently advisable to call in the middle of a runtime. "
  (if new-default-p
      (setf *buffer-allocation*  new-default)
      '((:nio
         . (BasicVector_ByteBuffer BasicVector_IntBuffer BasicVector_LongBuffer))
        (:primitive-array
         . '(BasicVector_UnsignedByte8 BasicVector_UnsignedByte16 BasicVector_UnsignedByte32)))))


  

  

  
