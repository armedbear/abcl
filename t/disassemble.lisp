(in-package :cl-user)

(prove:plan 1)
(let ((output (make-string-output-stream)))
  (let ((*standard-output* output))
    (disassemble #'cons))
  (let ((result (get-output-stream-string output)))
    (prove:ok (and result
                   (stringp result)
                   (> (length result) 0))
              (format nil
                      "Invocation of default EXT:*DISASSEMBLER* (~a)." ext:*disassembler*))))
(prove:finalize)


