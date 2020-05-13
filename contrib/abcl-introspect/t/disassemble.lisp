(in-package :cl-user)

(let ((disassembler (first (abcl-build:split-string
                            ext:*disassembler* #\Space))))
  (prove:plan 1)
  (prove:ok
   (abcl-build:introspect-path-for disassembler)
   (format nil
           "Testing invocation of ~a specified by EXT:*DISASSEMBLER*…" disassembler)))

(let ((disassemblers '(:objectweb :javap :jad :fernflower :cfr :procyon))) 
  (prove:plan (* 2 (length disassemblers)))
  (dolist (disassembler disassemblers)
    (prove:ok
     (asdf:load-system disassembler)
     (format nil "Loading ~a" disassembler))
    (prove:ok
     (handler-case
         (let ((expected (intern :disassemble-class-bytes
                                 (format nil "ABCL-INTROSPECT/JVM/TOOLS/~a" (symbol-name disassembler)))))
           (equal
            (sys:choose-disassembler disassembler)
            expected))
       (t (e)
         (progn
           (prove:diag (format nil "Choosing ~a failed: ~a" disassembler e))
           nil)))
     (format nil "Able to choose ~a disassembler" disassembler)))
  
  (prove:plan (length disassemblers))
  (dolist (disassembler disassemblers)
    (let ((output (make-string-output-stream)))
      (prove:ok
       (handler-case
           (let ((*standard-output* output))
             (sys:choose-disassembler disassembler)
             (cl:disassemble #'cons)
             (let ((result (get-output-stream-string output)))
               (not (null (and result
                               (stringp result)
                               (> (length result) 0))))))
         (t (e)
            (progn
              (prove:diag (format nil "Invocation failed: ~a" e))
              nil)))
       (format nil "Invocation of ~a disassembler" disassembler)))))

(prove:finalize)

