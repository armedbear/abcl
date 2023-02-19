(require :asdf)
(require :abcl-contrib)

(asdf:make :abcl-build)
(abcl-build:mvn/install)

(asdf:make :abcl-asdf)
(setf abcl-asdf:*mvn-libs-directory*
      (multiple-value-bind (mvn entries)
          (abcl-build:mvn/install)
        (let* ((root
                 (first (last entries)))
               (lib
                 (merge-pathnames  "./lib/" root )))
          (abcl-asdf:with-aether (lib)
            (values
             (and
             ;; for good measure
              (when (asdf:clear-system :jna)
                (asdf:make :jna))
              lib)
             (abcl-asdf:ensure-mvn-version)
             (abcl-build:ensure-maven)
             lib)))))

  

