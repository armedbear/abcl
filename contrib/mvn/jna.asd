;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to have a chance of working.
(asdf:defsystem :jna 
    :version "3.4.0"
    :defsystem-depends-on (abcl-asdf))
;; FIXME:  install a better handler in abcl-asdf  :components ((:mvn "net.java.dev.jna/jna/3.4.0")))

(defmethod asdf:perform :after ((o asdf:load-op) (c (eql (asdf:find-system :jna))))
  ;; Theoretically this should be the same thing as the MVN component.
  (handler-case 
      (unless 
          (flet ((match-jna-jar (p)
                   "Match `jna.jar`,`jna-3.0.9.jar`, or `jna-3.4.0.jar`."
                   (and (pathnamep p)
                        (equal (pathname-type p) "jar")
                        (java:jstatic "matches"
                                      "java.util.regex.Pattern" 
                                      "jna(-[0-9]\\.[0-9]\\.[0-9](-.+)?)?" 
                                      (pathname-name p))
                        p)))
            (dolist (loader (java:dump-classpath))
              (let ((jna-jar (some #'match-jna-jar loader)))
                (when jna-jar
                  (return abcl-jar)))))
        (java:add-to-classpath (abcl-asdf:resolve
                                "net.java.dev.jna:jna:3.4.0")))
    (t (e) (error "Failed to resolve 'jna.jar' because~&~A." e))))

                         
