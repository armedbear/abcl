
;; Raw outlines of a graphViz tool to visualize the instruction graph of ABCL generated code.
;;  and the associated stack depths.

(defvar *graph* nil)

(declaim (ftype (function (t) t) branch-opcode-p))
(declaim (inline branch-opcode-p))
(defun branch-opcode-p (opcode)
  (declare (optimize speed))
  (declare (type '(integer 0 255) opcode))
  (or (<= 153 opcode 168)
      (= opcode 198)))

(declaim (ftype (function (t t t) t) walk-code))
(defun walk-code (code start-index depth last-instruction)
  (declare (optimize speed))
  (declare (type fixnum start-index depth))
  (do* ((i start-index (1+ i))
        (limit (length code)))
       ((>= i limit))
    (declare (type fixnum i limit))
    (let* ((instruction (aref code i))
           (instruction-depth (jvm::instruction-depth instruction))
           (instruction-stack (jvm::instruction-stack instruction))
           (this-instruction (format nil "i~A" i)))
      (declare (type fixnum instruction-stack))
      (format t "~A ~A~%" last-instruction this-instruction)
      (push (list last-instruction  this-instruction depth) *graph*)
      (setf last-instruction this-instruction)
      (when instruction-depth
        (unless (= (the fixnum instruction-depth)
                   (the fixnum (+ depth instruction-stack)))
          (internal-compiler-error 
           "Stack inconsistency detected in ~A at index ~D: found ~S, expected ~S." 
           (compiland-name *current-compiland*)
           i instruction-depth (+ depth instruction-stack))
          (return-from walk-code)))
      (let ((opcode (jvm::instruction-opcode instruction)))
        (setf depth (+ depth instruction-stack))
        (setf (jvm::instruction-depth instruction) depth)
        (when (branch-opcode-p opcode)
          (let ((label (car (jvm::instruction-args instruction))))
            (declare (type symbol label))
            (walk-code code (symbol-value label) depth this-instruction)))
        (when (member opcode '(167 176 191)) ; GOTO ARETURN ATHROW
          ;; Current path ends.
          (return-from walk-code))))))

(declaim (ftype (function () t) analyze-stack))
(defun analyze-stack ()
  (declare (optimize speed))
  (let* ((code *code*)
         (code-length (length code)))
    (declare (type vector code))
    (dotimes (i code-length)
      (declare (type (unsigned-byte 16) i))
      (let* ((instruction (aref code i))
             (opcode (jvm::instruction-opcode instruction)))
        (when (eql opcode 202) ; LABEL
          (let ((label (car (jvm::instruction-args instruction))))
            (set label i)))
        (if (jvm::instruction-stack instruction)
            (when (jvm::opcode-stack-effect opcode)
              (unless (eql (jvm::instruction-stack instruction)
                           (jvm::opcode-stack-effect opcode))
                (sys::%format t "instruction-stack = ~S opcode-stack-effect = ~S~%"
                         (jvm::instruction-stack instruction)
                         (jvm::opcode-stack-effect opcode))
                (sys::%format t "index = ~D instruction = ~A~%" i
                              (jvm::print-instruction instruction))))
            (setf (jvm::instruction-stack instruction)
                  (jvm::opcode-stack-effect opcode)))
        (unless (jvm::instruction-stack instruction)
          (sys::%format t "no stack information for instruction ~D~%"
                        (jvm::instruction-opcode instruction))
          (aver nil))))
    (walk-code code 0 0 (gensym))
    (dolist (handler *handlers*)
      ;; Stack depth is always 1 when handler is called.
      (walk-code code (symbol-value (jvm::handler-code handler)) 1 (gensym)))
    (let ((max-stack 0))
      (declare (type fixnum max-stack))
      (dotimes (i code-length)
        (declare (type (unsigned-byte 16) i))
        (let* ((instruction (aref code i))
               (instruction-depth (jvm::instruction-depth instruction)))
          (when instruction-depth
            (setf max-stack (max max-stack (the fixnum instruction-depth))))))
;;       (when *compiler-debug*
;;         (sys::%format t "compiland name = ~S~%" (compiland-name *current-compiland*))
;;         (sys::%format t "max-stack = ~D~%" max-stack)
;;         (sys::%format t "----- after stack analysis -----~%")
;;         (print-code))
      max-stack)))


(defvar *code*)
(defvar *handlers*)
(compile nil '(lambda () nil))
(setq *handlers* nil)
(setq *code* nil)
(setq jvm::*saved-code* nil)
(setq jvm::*compiler-debug* t)
(defun f ()
  (let ((stream (make-string-input-stream "f" 0)))
    (read-line stream)
    (lambda () 
      (return-from f))))
(ignore-errors  (compile 'f))

(setq *graph* nil)
(let ((*code* (coerce (car jvm::*saved-code*) 'vector))
      (*handlers* (car jvm::*saved-handlers*)))
  (analyze-stack))
(with-open-file (f #p"g.gvz" :direction :output :if-exists :supersede)
  (format f "digraph main {~%")
  (dolist (e *graph*)
    (format f "~A -> ~A [label=\"~A\"];~%"
            (first e) (second e) (third e)))
  (let ((*code* (coerce (car jvm::*saved-code*) 'vector)))
    (dotimes (i (length *code*))
      (format f "i~A [label=\"~A:~A\"]~%" i i
              (jvm::opcode-name (jvm::instruction-opcode (aref *code* i))))))
  (format f "}~%"))

