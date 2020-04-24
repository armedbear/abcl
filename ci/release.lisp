(require :asdf)
(asdf:load-system :abcl-contrib)
(asdf:load-system :abcl)

(if (not (asdf:find-system :abcl-build))
    (warn "Can't find <urn:abcl.org/contrib/abcl-build> in ASDF.")
    (let (ant)
      (asdf:load-system :abcl-build)
      (abcl-build:with-ensured-ant (ant)
        (abcl/build:ant/call "build.xml"
                             '("abcl.release" "abcl.wrapper")))))


    
