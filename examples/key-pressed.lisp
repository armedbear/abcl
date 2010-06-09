;;; key-pressed.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
;;; $Id: key-pressed.lisp,v 1.8 2005-11-18 01:47:25 piso Exp $
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

(unless (find-package "KEY-PRESSED")
  (make-package "KEY-PRESSED" :nicknames '("KP") :use '("CL" "J")))

(in-package "KEY-PRESSED")

;; No exports.

(defcommand open-file)
(defcommand open-file-in-other-window)
(defcommand open-file-in-other-frame)
;; (defcommand new-buffer)
(defcommand recent-files)
(defcommand save)
(defcommand save-as)
(defcommand save-copy)
(defcommand save-all)
(defcommand kill-buffer)
(defcommand properties)
(defcommand next-buffer)
(defcommand prev-buffer)
(defcommand new-frame)
;; (defcommand execute-command "executeCommand")
;; (defcommand j-print "print")
(defcommand save-all-exit)
(defcommand quit)
(defcommand jump-to-line)
(defcommand jump-to-column)
(defcommand j-find "find")
(defcommand incremental-find)
(defcommand list-occurrences)
(defcommand find-in-files)
(defcommand list-files)
(defcommand sidebar-list-tags)
(defcommand j-replace "replace")
(defcommand replace-in-files)
(defcommand dir)
(defcommand goto-bookmark)
(defcommand help)
(defcommand describe-key)
(defcommand next-frame)
(defcommand select-word)
(defcommand kill-frame)
(defcommand toggle-sidebar)
(defcommand sidebar-list-buffers)
(defcommand split-window)
(defcommand unsplit-window)
(defcommand other-window)
(defcommand shell)

;;; Incremental find needs special handling.
(defun invoke-incremental-find ()
  (location-bar-cancel-input)
  (restore-focus)
  (invoke-later 'incremental-find))

(defvar *table* (make-hash-table :test #'equalp))

;;; Object can be a symbol or a function.
(defun assign-key (key object)
  (setf (gethash key *table*) object))

;;; The hook function.
(defun key-pressed (&rest args)
  (let* ((key (car args))
         (value (gethash key *table*)))
    (when (and value
               (or (functionp value)
                   (and (symbolp value) (fboundp value))))
      (funcall value))))

;;; Key assignments.
(assign-key "Ctrl O"
            #'(lambda ()
               (location-bar-cancel-input)
               (update-location-bar)
               (open-file)))
(assign-key "Ctrl Alt O"
            #'(lambda () (open-file-in-other-window) (update-location-bar)))
(assign-key "Ctrl Shift O" 'open-file-in-other-frame)
;; Ctrl N is used for history in textfields.
;; (assign-key "Ctrl N" 'new-buffer)
(assign-key "Alt R" 'recent-files)
(assign-key "Ctrl S" 'save)
(assign-key "Ctrl Shift S" 'save-as)
(assign-key "Ctrl Alt S" 'save-copy)
(assign-key "F2" 'save-all)
(assign-key "Ctrl F4" 'kill-buffer)
(assign-key "Ctrl W" 'kill-buffer)
(assign-key "Alt P" 'properties)
(assign-key "Alt NumPad Right"
            #'(lambda () (restore-focus) (next-buffer)))
(assign-key "Alt Right"
            #'(lambda () (restore-focus) (next-buffer)))
(assign-key "Alt NumPad Left"
            #'(lambda () (restore-focus) (prev-buffer)))
(assign-key "Alt Left"
            #'(lambda () (restore-focus) (prev-buffer)))
(assign-key "Ctrl Shift N" 'new-frame)
(assign-key "Alt X" 'execute-command)
;; Ctrl P is used for history in textfields.
;; (assign-key "Ctrl P" 'j-print)
(assign-key "Ctrl Shift Q" 'save-all-exit)
(assign-key "Ctrl Q" 'quit)
(assign-key "Ctrl J" 'jump-to-line)
(assign-key "Ctrl Shift J" 'jump-to-column)
(assign-key "Alt F3"
            #'(lambda () (location-bar-cancel-input) (restore-focus) (j-find)))
(assign-key "Ctrl F" 'invoke-incremental-find)
(assign-key "Alt L" 'list-occurrences)
(assign-key "F6" 'find-in-files)
(assign-key "Ctrl Shift F" 'find-in-files)
(assign-key "Ctrl L" 'list-files)
(assign-key "Ctrl Shift L" 'sidebar-list-tags)
(assign-key "Ctrl R" 'j-replace)
(assign-key "Ctrl Shift R" 'replace-in-files)
(assign-key "Ctrl D" 'dir)
(assign-key "Ctrl 0" 'goto-bookmark)
(assign-key "Ctrl 1" 'goto-bookmark)
(assign-key "Ctrl 2" 'goto-bookmark)
(assign-key "Ctrl 3" 'goto-bookmark)
(assign-key "Ctrl 4" 'goto-bookmark)
(assign-key "Ctrl 5" 'goto-bookmark)
(assign-key "Ctrl 6" 'goto-bookmark)
(assign-key "Ctrl 7" 'goto-bookmark)
(assign-key "Ctrl 8" 'goto-bookmark)
(assign-key "Ctrl 9" 'goto-bookmark)
(assign-key "F1" 'help)
(assign-key "Alt K" 'describe-key)
(assign-key "Alt N" 'next-frame)
(assign-key "Alt W" 'select-word)
(assign-key "Ctrl Shift W" 'kill-frame)
(assign-key "Alt =" 'toggle-sidebar)
(assign-key "Alt B" 'sidebar-list-buffers)
(assign-key "F10" 'split-window)
(assign-key "Shift F10" 'unsplit-window)
(assign-key "Alt O" 'other-window)
(assign-key "Alt F9"
            #'(lambda () (restore-focus) (shell)))

;;; Enable the hook.
(add-hook 'key-pressed-hook 'key-pressed)
(set-global-property "enableKeyPressedHook" t)

;; NOTE: ENABLE-KEY-PRESSED-HOOK will be reset to its default value (NIL) when
;; preferences are reloaded (which happens automatically when you edit your
;; preferences file). To prevent this (and keep the key-pressed hook working
;; properly across preference file edits), add this line to ~/.j/prefs:
;;
;;      enableKeyPressedHook = true
;;
