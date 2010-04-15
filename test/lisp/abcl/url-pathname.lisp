(in-package #:abcl.test.lisp)

;; URL Pathname tests
(deftest url-pathname.1
    (let* ((p #p"http://example.org/a/b/foo.lisp")
           (host (pathname-host p)))
      (values 
       (check-physical-pathname p '(:absolute "a" "b") "foo" "lisp")
       (and (consp host)
            (equal (getf host :scheme) 
                   "http")
            (equal (getf host :authority)
                   "example.org"))))
  t t)

(deftest url-pathname.2
    (let* ((p (pathname "http://example.org/a/b/foo.lisp?query=this#that-fragment"))
           (host (pathname-host p)))
      (values 
       (check-physical-pathname p '(:absolute "a" "b") "foo" "lisp")
       (consp host)
       (getf host :scheme) 
       (getf host :authority)
       (getf host :query)
       (getf host :fragment)))
  t 
  t
  "http"
  "example.org"
  "query=this"  
  "that-fragment")

(deftest url-pathname.3
    (let* ((p (pathname
               "http://example.org/a/b/foo.lisp?query=this#that-fragment")))
      (values 
       (ext:url-pathname-scheme p)
       (ext:url-pathname-authority p)
       (ext:url-pathname-query p)
       (ext:url-pathname-fragment p)))
  "http"
  "example.org"
  "query=this"  
  "that-fragment")
