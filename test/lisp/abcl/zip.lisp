(in-package #:abcl.test.lisp)

(deftest zip.1 
    (let ((mapping (make-hash-table :test 'equal)))
      (loop :for (key value) 
         :in `(("/etc/hosts" "/etc/hosts")
               ("/etc/group" "groups")               
               ("/etc/resolv.conf" "/opt/etc/resolv.conf"))
         :doing 
            (setf (gethash key mapping) value))
      (values 
       (system:zip #p"/var/tmp/foo.jar" mapping)
       (not (probe-file "jar:file:/var/tmp/foo.jar!/etc/hosts"))
       (not (probe-file "jar:file:/var/tmp/foo.jar!/groups"))
       (not (probe-file "jar:file:/var/tmp/foo.jar!/opt/etc/resolv.conf"))))
  #p"/var/tmp/foo.jar" nil nil nil)

(eval-when (:load-toplevel)
  (if (not (find :unix *features*))
      (pushnew 'zip.1 *expected-failures*)))
