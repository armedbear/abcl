(in-package :cl-user)

(let ((disassembler (first (abcl-build:split-string
                            ext:*disassembler* #\Space))))
  (prove:plan 1)
  (prove:ok
   (abcl-build:introspect-path-for disassembler)
   (format nil
           "Testing invocation of ~a specified by EXT:*DISASSEMBLER*…" disassembler)))

(let ((disassemblers '(:objectweb :javap :jad))) 
  (prove:plan (* 2 (length disassemblers)))
  (dolist (disassembler disassemblers)
    (prove:is 
     (asdf:load-system disassembler)
     (format nil "Loading ~a" disassembler))
    (prove:is
     (sys:choose-disassembler disassembler)
     (uiop:reify-symbol :disassemble-class-bytes
                        (format nil "abcl-introspect/jvm/tools/~a" disassembler))
     (format nil "Able to choose ~a" disassembler)))
  (prove:plan (length disassemblers))
  (dolist (disassembler disassemblers)
    (let ((output (make-string-output-stream)))
      (sys:choose-disassembler disassembler)
      (let ((*standard-output* output))
        (cl:disassemble #'cons))
      (let ((result (get-output-stream-string output)))
        (prove:ok (and result
                       (stringp result)
                       (> (length result) 0))
                  (format nil "Invocation of ~a disassembler" disassembler))))))

(prove:finalize)

