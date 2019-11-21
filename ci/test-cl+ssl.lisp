(require :asdf)
(require :abcl-contrib)

(ql:quickload :cl+ssl)
(ql:quickload :cl+ssl.test)

(fiveam:run-all-tests)

