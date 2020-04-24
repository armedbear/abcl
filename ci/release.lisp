(require :asdf)
(require :abcl-contrib)

(when (asdf:find-system :abcl)
  (asdf:load-system :abcl-build)
  (let (ant)
    (abcl-build:with-ensured-ant (ant)
      (abcl-build:ant/call "abcl.release"))))

    
