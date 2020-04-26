(require :asdf)
(require :abcl-contrib)

(if (not (asdf:find-system :abcl-build))
    (warn "Failed to find <urn:abcl.org/contrib/abcl-build> in ASDF to build release.")
    (let (ant)
      (asdf:make :abcl-build)
      (abcl-build:with-ensured-ant (ant)
        (abcl/build:ant/call "build.xml"
                             '("abcl.release" "abcl.wrapper")))))



    
