(require :asdf)
(require :abcl-contrib)

(ql:quickload :cl_ssl.test)

(fiveam:run-all-tests)

