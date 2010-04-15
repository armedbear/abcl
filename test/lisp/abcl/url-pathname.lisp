(in-package #:abcl.test.lisp)

;; URL Pathname tests
(deftest pathname-url.1
    (let* ((p #p"http://example.org/a/b/foo.lisp")
           (host (pathname-host p)))
      (values 
       (check-physical-pathname p '(:absolute "a" "b") "foo" "lisp")
       (and (consp host)
            (equal (getf host :scheme) 
                   "http")
            (equal (getf host :authority)
                   "example.org"))))
  (t t))

(deftest pathname-url.2
    (let* ((p #p"http://example.org/a/b/foo.lisp?query=this#that-fragment")
           (host (pathname-host p)))
      (values 
       (check-physical-pathname p '(:absolute "a" "b") "foo" "lisp")
       (and (consp host)
            (equal (getf host :scheme) 
                   "http")
            (equal (getf host :authority)
                   "example.org")
            (equal (getf host :query)
                   "query=this")
            (equal (getf host :fragment)
                   "that-fragment"))))
  (t t))
