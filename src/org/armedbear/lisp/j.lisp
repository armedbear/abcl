;;; j.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
;;; $Id$
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

(in-package #:j)

(export '(*current-command*
          *last-command*
          add-hook
          after-save-hook
          backward-char
          backward-sexp
          backward-up-list
          beginning-of-line
          buffer-activated-hook
          buffer-live-p
          buffer-modified-p
          buffer-name
          buffer-offset
          buffer-pathname
          buffer-stream-buffer
          buffer-string
          buffer-substring
          char-after
          char-before
          copy-mark
          current-buffer
          current-editor
          current-line
          current-mark
          current-point
          defcommand
          defun-at-point
          delete-region
          editor-buffer
          emacs-mode
          end-of-line
          execute-command
          find-beginning-of-defun
          find-file-buffer
          forward-char
          forward-sexp
          get-buffer
          get-buffer-property
          get-global-property
          get-last-event-internal-time
          global-map-key
          global-unmap-key
          goto-char
          insert
          invoke-hook
          invoke-later
          key-pressed-hook
          kill-theme
          line-chars
          line-flags
          line-next
          line-number
          line-previous
          lisp-shell-startup-hook
          location-bar-cancel-input
          log-debug
          looking-at
          make-buffer-stream
          make-mark
          map-key-for-mode
          mark-charpos
          mark-line
          mark=
          move-to-position
          open-file-hook
          other-editor
          point-max
          point-min
          pop-to-buffer
          re-search-backward
          re-search-forward
          reset-display
          restore-focus
          save-excursion
          search-backward
          search-forward
          set-buffer-modified-p
          set-buffer-property
          set-global-property
          set-mark
          set-mode-property
          status
          switch-to-buffer
          undo
          unmap-key-for-mode
          update-display
          update-location-bar
          with-editor
          with-other-editor
          with-single-undo))

(declaim (special *current-command* *last-command*))

(autoload 'emacs-mode "emacs")

(defun reset-display ()
  (jstatic "resetDisplay" "org.armedbear.j.Editor"))

(defun log-debug (control-string &rest args)
  (%log-debug (apply 'format nil control-string args)))

(defun update-display (&optional ed)
  (let ((method (jmethod "org.armedbear.j.Editor" "updateDisplay"))
        (ed (or ed (current-editor))))
    (jcall method ed)))

(defun update-location-bar (&optional ed)
  (let ((method (jmethod "org.armedbear.j.Editor" "updateLocation"))
        (ed (or ed (current-editor))))
    (jcall method ed)))

(defun location-bar-cancel-input ()
  (jstatic "cancelInput" "org.armedbear.j.LocationBar"))

;; Internal.
(defun %execute-command (command &optional ed)
  (let ((method (jmethod "org.armedbear.j.Editor"
                         "executeCommand" "java.lang.String"))
        (ed (or ed (current-editor))))
    (jcall method ed command)
    (update-display ed)))

(defmacro defcommand (name &optional (command nil))
  (unless command
    (setf command (remove #\- (string `,name))))
  `(setf (symbol-function ',name)
         (lambda (&optional arg)
           (%execute-command (if arg
                                 (concatenate 'string ,command " " arg)
                                 ,command)))))

(defcommand execute-command)

;;; HOOKS
(defun add-hook (hook function)
  (when (symbolp hook)
    (unless (boundp hook) (set hook nil))
    (let ((hook-functions (symbol-value hook)))
      (unless (memq function hook-functions)
        (push function hook-functions)
        (set hook hook-functions)))))

(defun invoke-hook (hook &rest args)
  (when (and (symbolp hook) (boundp hook))
    (let ((hooks (symbol-value hook)))
      (when hooks
        (dolist (function hooks)
          (apply function args))
        t))))

(defvar open-file-hook nil)

(defvar buffer-activated-hook nil)

(defvar after-save-hook nil)

(defvar key-pressed-hook nil)

(defvar lisp-shell-startup-hook nil)

(defsetf current-editor %set-current-editor)

(defsetf buffer-mark %set-buffer-mark)

(defsetf editor-mark %set-editor-mark)

(defsetf line-flags %set-line-flags)

(defmacro with-editor (editor &rest forms)
  (let ((old-editor (gensym)))
  `(let ((,old-editor (current-editor)))
     (unwind-protect
      (progn
        (setf (current-editor) ,editor)
        ,@forms)
      (update-display ,editor)
      (setf (current-editor) ,old-editor)))))

(defmacro with-other-editor (&rest forms)
  (let ((old-editor (gensym))
        (other-editor (gensym)))
    `(let ((,old-editor (current-editor))
           (,other-editor (other-editor)))
       (unless ,other-editor
         (error "there is no other editor"))
       (unwind-protect
        (progn
          (setf (current-editor) ,other-editor)
          ,@forms)
        (update-display ,other-editor)
        (setf (current-editor) ,old-editor)))))

(defmacro with-single-undo (&rest forms)
  (let ((info (gensym)))
    `(let ((,info (begin-compound-edit)))
       (unwind-protect
        (progn ,@forms)
        (end-compound-edit ,info)))))

(defmacro save-excursion (&rest forms)
  (let ((old-point (gensym)))
    `(let ((,old-point (current-point)))
       (unwind-protect
        (progn ,@forms)
        (goto-char ,old-point)))))

(defun search-forward (pattern &key buffer start ignore-case whole-words-only)
  (%search pattern :forward nil buffer start ignore-case whole-words-only))

(defun search-backward (pattern &key buffer start ignore-case whole-words-only)
  (%search pattern :backward nil buffer start ignore-case whole-words-only))

(defun re-search-forward (pattern &key buffer start ignore-case whole-words-only)
  (%search pattern :forward t buffer start ignore-case whole-words-only))

(defun re-search-backward (pattern &key buffer start ignore-case whole-words-only)
  (%search pattern :backward t buffer start ignore-case whole-words-only))

(in-package "COMMON-LISP-USER")

(use-package '#:j)

(provide '#:j)
