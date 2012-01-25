(in-package :abcl-asdf-test)

(deftest LOG4J.2
    (progn
      (asdf:load-system "log4j")
      (let ((logger (#"getLogger" 'log4j.Logger (symbol-name (gensym)))))
        (#"trace" logger "Kilroy wuz here.")))
  t)




