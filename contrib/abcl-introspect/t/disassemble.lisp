(in-package :cl-user)

(let ((disassembler (first (abcl-build:split-string
                            ext:*disassembler* #\Space))))
  (prove:plan 1)
  (prove:ok (abcl-build:introspect-path-for disassembler)
            (format nil
                    "Testing invocation of ~a specified by EXT:*DISASSEMBLER*…" disassembler)))

;;; FIXME move dependency load into ASDF definitions
(asdf:load-system :objectweb)
(prove:plan 2)
(prove:is
 (sys:choose-disassembler)
 'ABCL/BUILD/JVM/TOOLS/OBJECTWEB::DISASSEMBLE-CLASS-BYTES)
(let ((output (make-string-output-stream)))
  (let ((*standard-output* output))
    (disassemble #'cons))
  (let ((result (get-output-stream-string output)))
    (prove:ok (and result
                   (stringp result)
                   (> (length result) 0))
              "Invocation of Objectweb disassembler.")))

(prove:finalize)

