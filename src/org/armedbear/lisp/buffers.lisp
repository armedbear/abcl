(in-package :ext)

(defvar *buffer-allocation* :primitive-array
  "The current buffer allocation strategy.")

(defun choose-buffer-strategy (&key (new-default :nio new-default-p))
  "Return current choices for buffer allocation strategy that can then be chosen as a :new-default strategy

Not currently advisable to call during runtime. "
  (if new-default-p
      (setf *buffer-allocation*  new-default)
      '((:nio
         ((unsigned-byte 8)
          . BasicVector_ByteBuffer)
         ((unsigned-byte 16)
          . BasicVector_CharBuffer)
         ((unsigned-byte 32)
          . BasicVector_IntBuffer))
        (:primitive-array
         ((unsigned-byte 8)
          . BasicVector_UnsignedByte8)
         ((unsigned-byte 16)
          . BasicVector_UnsignedByte16)
         ((unsigned-byte 32)
          . BasicVector_UnsignedByte32)))))

(defun make-niobuffer-vector (nio-buffer &key (element-type '(unsigned-byte 8)))
  (unless (subtypep element-type '(unsigned-byte *))
    (signal 'type-error "Need some subtype of (UNSIGNED-BYTE *)")) ;; FIXME: :datum ,
  (case (second element-type) ;; FIXME: probably wrong for types that aren't 8, 16, or 32
    (8 
     (ext:make-bytebuffer-byte-vector nio-buffer))    
    (16 
     (ext:make-charbuffer-byte-vector nio-buffer))
    (32
     (ext:make-intbuffer-byte-vector nio-buffer))
    (t
     (ext:make-bytebuffer-byte-vector nio-buffer))))


(eval-when (:load-toplevel :execute)
  ;;; FIXME: otherwise EXT:MAKE-INTBUFFER-BYTE-VECTOR doesn't get loaded
  (make-array 1 :element-type '(unsigned-byte 16))
  (make-array 2 :element-type '(unsigned-byte 32)))
  

  

  
