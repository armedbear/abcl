(ql:quickload
 '(:ironclad :ironclad/tests))

(time 
 (asdf:test-system :ironclad))

