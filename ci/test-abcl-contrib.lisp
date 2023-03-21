(require :asdf)
(require :abcl-contrib)

(dolist (system '(:jss :asdf-jar))
  (asdf:make system)
  (time 
   (asdf:test-system system)))


