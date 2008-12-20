;;; init.lisp
;;; $Id: init.lisp,v 1.36 2007-03-04 19:08:11 piso Exp $

;;; ~/.j/init.lisp (if it exists) is loaded automatically when j starts up.

(defun java-version ()
  (jstatic "getProperty" "java.lang.System" "java.version"))

(defun adjust-appearance ()
  (when (member (subseq (java-version) 0 5)
                '("1.4.0" "1.4.1" "1.4.2" "1.5.0" "1.6.0" "1.7.0")
                :test #'string=)
    (set-global-property "adjustAscent" -2)
    (set-global-property "adjustLeading" -2)
    (reset-display)))

;; Do it now!
(adjust-appearance)

;; Turn off the remove-trailing-whitespace preference for files in the
;; directory ~/gcl/ansi-tests.
(defun my-open-file-hook (buf)
  (let ((pathname (buffer-pathname buf)))
    (when (and pathname
               (string= (directory-namestring pathname)
                        "/home/peter/gcl/ansi-tests/"))
      (set-buffer-property "removeTrailingWhitespace" nil))))

(add-hook 'open-file-hook 'my-open-file-hook)

;; Helper function for MY-BUFFER-ACTIVATED-HOOK.
(defun sub-p (namestring dirname)
  "Returns T if NAMESTRING is in DIRNAME or one of its subdirectories"
  (let ((dirname-length (length dirname)))
    (and (> (length namestring) dirname-length)
         (string= (subseq namestring 0 dirname-length) dirname))))

(defun my-buffer-activated-hook (buf)
  (let ((pathname (buffer-pathname buf)))
    ;; PATHNAME might be NIL (not all buffers have associated files).
    (when pathname
      (let ((type (pathname-type pathname)))
        ;; We only care about Lisp and Java buffers.
        (cond ((string= type "el")
               (set-buffer-property
                "tagPath"
                "/home/peter/emacs-21.3/lisp:/home/peter/emacs-21.3/lisp/emacs-lisp"))
              ((member type '("lisp" "lsp" "cl" "java") :test 'string=)
               (let* ((namestring (namestring pathname))
                      (tagpath
                       (cond ((sub-p namestring "/home/peter/cmucl/src/")
                              "/home/peter/cmucl/src/code:/home/peter/cmucl/src/compiler:/home/peter/cmucl/src/pcl")
                             ((sub-p namestring "/home/peter/cl-bench/")
                              "/home/peter/cl-bench:/home/peter/cl-bench/files:/home/peter/depot/j/src/org/armedbear/lisp")
                             ((sub-p namestring "/home/peter/gcl/ansi-tests/")
                              "/home/peter/gcl/ansi-tests:/home/peter/depot/j/src/org/armedbear/lisp")
                             ((sub-p namestring "/home/peter/phemlock")
                              "/home/peter/phemlock/src/core:/home/peter/phemlock/src/user")
                             ((sub-p namestring "/home/peter/sbcl")
                              "/home/peter/sbcl/src/code:/home/peter/sbcl/src/compiler")
                             (t ; default case: no change
                              nil))))
                 ;; If we end up here with a non-NIL TAGPATH, use it to set the
                 ;; buffer-specific value of the TAG-PATH preference for the current
                 ;; buffer.
                 (when tagpath
                   (set-buffer-property "tagPath" tagpath)))))))))

;; Install our hook function.
(add-hook 'buffer-activated-hook 'my-buffer-activated-hook)

;; Call ADJUST-APPEARANCE after saving ~/.j/prefs.
(defun my-after-save-hook (buf)
  (let ((pathname (buffer-pathname buf)))
    (when (equal pathname #p"/home/peter/.j/prefs")
      (adjust-appearance))))

(add-hook 'after-save-hook 'my-after-save-hook)

(defun reset-incoming-filters ()
  (jstatic "resetIncomingFilters" "org.armedbear.j.mail.IncomingFilter"))

(defun add-incoming-filter (mailbox pattern action parameter)
  (jstatic "addIncomingFilter" "org.armedbear.j.mail.IncomingFilter"
           mailbox pattern action parameter))

(add-hook 'mailbox-mode-hook
          (lambda ()
            (reset-incoming-filters)
            (add-incoming-filter "inbox"
                                 "~C linux-kernel"
                                 "move"
                                 "mail/linux-kernel")
            (add-incoming-filter "inbox"
                                 "~C ix.netcom.com"
                                 "move"
                                 "mail/netcom")))

(defun maybe-load (pathname)
  (when (probe-file pathname)
    (load pathname)))

(maybe-load "/home/peter/.j/key-pressed.lisp")
(maybe-load "/home/peter/.j/update-check-enabled.lisp")

(maybe-load #+windows "c:/cygwin/home/peter/j/build-abcl.lisp"
            #-windows "/home/peter/j/build-abcl.lisp")

(map-key-for-mode ")" "electricCloseParen" "Lisp Shell")

(map-key-for-mode "[" "insertParentheses" "Lisp")
(map-key-for-mode "]" "movePastCloseAndReindent" "Lisp")
