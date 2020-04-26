(require :asdf)
(require :abcl-contrib)

(if (not (asdf:find-system :abcl-build))
    (warn "Failed to find <urn:abcl.org/contrib/abcl-build> in ASDF to hash release.")
    (asdf:make :abcl-build))

(let ((root (uiop:getenv "ABCL_ROOT"))
      ant)
  (format *standard-output* "ABCL_ROOT='~a'~%" root)
  #+(or) ;;; needs TeXlive to render User Manual
  (abcl-build:with-ensured-ant (ant)
    (abcl-build:ant/call "build.xml"
                         '("abcl.release" "abcl.wrapper")))
  (let ((dist (concatenate 'string root "/dist/")))
    (format *standard-output* "dist='~a'~%" dist)
    (multiple-value-bind (hashes report) 
        (abcl-build:directory-hashes dist)
      (format *standard-output* report))))




            
           
                                               






    
