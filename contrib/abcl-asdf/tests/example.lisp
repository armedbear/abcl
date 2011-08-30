(require :jss)

(let ((logger (#"getLogger" 'log4j.Logger (symbol-name (gensym)))))
  (#"trace" logger "Kilroy wuz here."))

