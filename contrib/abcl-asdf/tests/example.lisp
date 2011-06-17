(require :jss)

(let ((logger (#"getLogger" 'Logger (symbol-name (gensym)))))
  (#"log" logger "Kilroy wuz here."))

