;;;; TODO: move to a utility package
(in-package :abcl/build)

(defun localize-executable-name (name)
  (let* ((p (if (pathnamep name)
                name
                (pathname name)))
         (type (pathname-type p)))
    (make-pathname :defaults p
                   :type 
                   (if (uiop:os-windows-p)
                       (when (null type)
                         "exe")
                       type))))

(defun introspect-path-for (executable)
  (let ((which-command (if (uiop:os-windows-p)
                           "where"
                           "which")))
    (when (ignore-errors
            (uiop:run-program (list which-command which-command) :output :string))
      (let ((raw-result 
             (uiop:run-program
              (list which-command
                    (namestring (localize-executable-name executable)))
              :output :string)))
        (let ((result 
               (string-trim 
                '(#\Space #\Newline #\Backspace #\Tab 
                  #\Linefeed #\Page #\Return #\Rubout)
                raw-result)))
          (values
           result
           (pathname result)))))))

(defun probe-executable (directory executable)
  (probe-file
   (localize-executable-name
    (merge-pathnames executable directory))))
  
(defun split-string (string split-char)
  (loop :for i = 0 :then (1+ j)
     :as j = (position split-char string :test #'string-equal :start i)
     :collect (subseq string i j)
     :while j))

(defun stringify (thing)
  (cond
    ((pathnamep thing)
     (namestring thing))
    ((stringp thing)
     thing)
    (t
     (error "Don't know how stringify ~a." thing))))

(defun listify (thing)
  (if (consp thing)
      thing
      (list thing)))

(defun some-directory-containing (executable)
  ;; search path
  (let ((in-path (introspect-path-for executable)))
    (when in-path
      (return-from some-directory-containing
                   in-path))
    (dolist (d 
              (if (uiop:os-windows-p)
                  '(#p"c:/Program Files/") ;; TODO localize me!
                  '(#p"/usr/local/bin/" #p"/opt/local/bin/" #p"/usr/bin/")))
      (let* ((e (localize-executable-name
                 (merge-pathnames :defaults d
                                  :name executable)))
             (p (probe-file p)))
        (when p
          (return-from some-directory-containing p))))))


        
       
