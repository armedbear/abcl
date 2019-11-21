;; https://abcl.org/trac/ticket/353
(declaim (inline foo))
(defun foo ()
  (foo))

(defun bar ()
  (foo))
