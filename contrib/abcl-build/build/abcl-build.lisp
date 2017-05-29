(in-package :abcl/build)

(defun make-dist (version-string)
  (warn "Unimplemented"))

(defun build-abcl (&key
                     force ;; DEPRECATED: not sure of meaning in new underlying API
                     (batch t) ;; DEPRECATED: lack of meaning
                     compile-system ;; DEPRECATED: COMPILE-SYSTEM is always invoked
                     jar ;; DEPRECATED: a jar archive is always built
                     clean
                     full) ;; DEPRECATED:  a full build is always performed
  (unless (ignore-errors (asdf:find-system :abcl))
    (return-from build-abcl
      nil))
  (let ((targets '("abcl")))
    (when clean
      (push "abcl.clean" targets))
    (ant/call (asdf:system-relative-pathname :abcl "build.xml")
              (nreverse targets))))




