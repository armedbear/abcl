(ql:quickload
 '(:nontrivial-gray-streams/test))

(time 
 (asdf:test-system :nontrivial-gray-streams))

