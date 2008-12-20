;;; emacs.lisp
;;;
;;; Copyright (C) 2005 Peter Graves
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

(export '(emacs-mode
          java-mode-map
          lisp-mode-map
          lisp-shell-mode-map
          directory-mode-map))

(defpackage #:emacs
  (:use #:cl #:ext #:j))

(in-package #:emacs)

(defun set-mark-command ()
  (setf (buffer-mark (current-buffer)) (current-point))
  (status "Mark set"))

(defun define-keys (map mappings)
  (dolist (mapping mappings)
    (define-key map (first mapping) (second mapping))))

(defparameter *emacs-global-map* (make-keymap))
(defparameter *esc-map* (make-keymap))
(defparameter *control-x-map* (make-keymap))
(defparameter *help-map* (make-keymap))
(defparameter *java-mode-map* (make-keymap))

(define-key *emacs-global-map* "Escape" *esc-map*)
(define-key *emacs-global-map* "Ctrl X" *control-x-map*)
(define-key *emacs-global-map* "Ctrl H" *help-map*)

;; // File menu.
(define-key *control-x-map* "Ctrl F" "openFile")
;; mapKey(KeyEvent.VK_O, CTRL_MASK | ALT_MASK, "openFileInOtherWindow");
;; mapKey(KeyEvent.VK_O, CTRL_MASK | SHIFT_MASK, "openFileInOtherFrame");
;; mapKey(KeyEvent.VK_N, CTRL_MASK, "newBuffer");
;; mapKey(KeyEvent.VK_R, ALT_MASK, "recentFiles");
(define-key *control-x-map* "Ctrl S" "save")
;; mapKey(KeyEvent.VK_S, CTRL_MASK | SHIFT_MASK, "saveAs");
;; mapKey(KeyEvent.VK_S, CTRL_MASK | ALT_MASK, "saveCopy");
;; mapKey(KeyEvent.VK_F2, 0, "saveAll");

;; j's killBuffer is really kill-this-buffer
(define-key *control-x-map* "k" "killBuffer")

;; mapKey(KeyEvent.VK_P, ALT_MASK, "properties");
;; mapKey(KeyEvent.VK_N, CTRL_MASK | SHIFT_MASK, "newFrame");
(define-key *emacs-global-map* "Alt X" "executeCommand")
(define-key *esc-map* #\x "executecommand")
;; mapKey(KeyEvent.VK_P, CTRL_MASK, "print");
;; mapKey(KeyEvent.VK_Q, CTRL_MASK | SHIFT_MASK, "saveAllExit");
(define-key *control-x-map* "Ctrl C" "saveAllExit")

(define-key *emacs-global-map* "Ctrl Space" #'set-mark-command)
(define-key *emacs-global-map* "Ctrl Shift 2" #'set-mark-command) ; C-@

;; // Edit menu.
(define-keys *emacs-global-map*
  `(("Ctrl /"                   "undo")
    ("Ctrl Shift 0x2d"          "undo") ; C-_
    ("Shift Alt 0x2d"           "redo") ; M-_
    ("Ctrl W"                   "killRegion")
    ("Shift Delete"             "killRegion")
    ("Alt W"                    "copyRegion")
    ("Ctrl NumPad Insert"       "copyRegion")
    ("Ctrl Y"                   "paste")
    ("Shift NumPad Insert"      "paste")
    ("Alt Y"                    "cyclePaste")))

(define-key *control-x-map* "u" "undo")
(define-key *esc-map* #\y "cyclePaste")
;; mapKey(KeyEvent.VK_X, CTRL_MASK | SHIFT_MASK, "killAppend");
;; mapKey(KeyEvent.VK_C, CTRL_MASK | SHIFT_MASK, "copyAppend");
;; mapKey(KeyEvent.VK_T, ALT_MASK, "cycleTabWidth");

;; // Goto menu.
;; mapKey(KeyEvent.VK_J, CTRL_MASK, "jumpToLine");
(define-key *emacs-global-map* "Alt G" "jumpToLine")
(define-key *esc-map* #\g "jumpToLine")
;; mapKey(KeyEvent.VK_J, CTRL_MASK | SHIFT_MASK, "jumpToColumn");
;; mapKey(KeyEvent.VK_M, CTRL_MASK, "findMatchingChar");
;; mapKey(KeyEvent.VK_M, CTRL_MASK | SHIFT_MASK, "selectSyntax");

(define-keys *emacs-global-map*
  '(("Ctrl Alt Up"              "findFirstOccurrence")
    ("Ctrl Alt NumPad Up"       "findFirstOccurrence")
    ("Alt Up"                   "findPrevWord")
    ("Alt NumPad Up"            "findPrevWord")
    ("Alt Down"                 "findNextWord")
    ("Alt NumPad Down"          "findNextWord")))

;; mapKey(KeyEvent.VK_N, CTRL_MASK | ALT_MASK, "nextChange");
;; mapKey(KeyEvent.VK_P, CTRL_MASK | ALT_MASK, "previousChange");
(define-key *emacs-global-map* "F5" "pushPosition")
(define-key *emacs-global-map* "Shift F5" "popPosition")

;; // Search menu.
(define-keys *emacs-global-map*
  '(("Ctrl S"                   "incrementalFind")
    ("Alt F3"                   "find")
    ("F3"                       "findNext")
    ("Shift F3"                 "findPrev")
    ("F6"                       "findInFiles")
    ("Ctrl Shift F"             "findInFiles")
    ("Ctrl F3"                  "listOccurrences")
    ("Ctrl Shift L"             "listFiles")
    ("Shift Alt 5"              "replace") ; M-%
    ("Ctrl Shift R"             "replaceInFiles")))

;; Emacs uses Ctrl Alt L for reposition-window
;; XEmacs uses Ctrl Alt L for switch-to-other-buffer
(define-key *emacs-global-map* "Ctrl Alt L" "listOccurrencesOfPatternAtDot")

;; mapKey(KeyEvent.VK_K, CTRL_MASK, "killLine");
(define-key *emacs-global-map* "Ctrl K" "killLine")
;; mapKey(KeyEvent.VK_DELETE, CTRL_MASK, "deleteWordRight");
(define-key *emacs-global-map* "Ctrl Delete" "deleteWordRight")

(define-keys *emacs-global-map*
  '(("Home"                     "home")
    ("Ctrl A"                   "home")
    ("End"                      "end");
    ("Ctrl E"                   "end");
    ("Shift Home"               "selectHome")
    ("Shift End"                "selectEnd")
    ("Ctrl Home"                "bob")
    ("Ctrl Shift Home"          "selectBob")
    ("Ctrl End"                 "eob")
    ("Ctrl Shift End"           "selectEob")
    ("Ctrl P"                   "up")
    ("Up"                       "up")
    ("NumPad Up"                "up")
    ("Ctrl N"                   "down")
    ("Down"                     "down")
    ("NumPad Down"              "down")
    ("Shift Up"                 "selectUp")
    ("Shift NumPad Up"          "selectUp")
    ("Shift Down"               "selectDown")
    ("Shift NumPad Down"        "selectDown")
    ("Ctrl B"                   "left")
    ("Left"                     "left")
    ("NumPad Left"              "left")
    ("Ctrl F"                   "right")
    ("Right"                    "right")
    ("NumPad Right"             "right")
    ("Shift Left"               "selectLeft")
    ("Shift NumPad Left"        "selectLeft")
    ("Shift Right"              "selectRight")
    ("Shift NumPad Right"       "selectRight")
    ("Alt B"                    "wordLeft")
    ("Ctrl Left"                "wordLeft")
    ("Ctrl NumPad Left"         "wordLeft")
    ("Ctrl Right"               "wordRight")
    ("Ctrl NumPad Right"        "wordRight")
    ("Ctrl Shift Left"          "selectWordLeft")
    ("Ctrl Shift NumPad Left"   "selectWordLeft")
    ("Ctrl Shift Right"         "selectWordRight")
    ("Ctrl Shift NumPad Right"  "selectWordRight")
    ("Alt V"                    "pageUp")
    ("Page Up"                  "pageUp")
    ("Ctrl V"                   "pageDown")
    ("Page Down"                "pageDown")))

(define-keys *esc-map*
  '((#\<                        "bob")
    (#\>                        "eob")
    (#\.                        "findTagAtDot")
    (#\,                        "listMatchingTagsAtDot")
    (#\%                        "replace")
    ))

;; Emacs uses Ctrl Up for backward-paragraph, which j doesn't have.
(define-keys *emacs-global-map*
  '(("Ctrl Up"                  "windowUp")
    ("Ctrl NumPad Up"           "windowUp")))
;; Emacs uses Ctrl Down for forward-paragraph, which j doesn't have.
(define-keys *emacs-global-map*
  '(("Ctrl Down"                "windowDown")
    ("Ctrl NumPad Down"         "windowDown")))

;; Emacs uses Alt Left for backward-word, which is also on Alt B and Ctrl Left.
(define-keys *emacs-global-map*
  '(("Alt Left"                 "prevBuffer")
    ("Alt NumPad Left"          "prevBuffer")))
;; Emacs uses Alt Right for forward-word, which is also on Alt F and Ctrl Right.
(define-keys *emacs-global-map*
  '(("Alt Right"                "nextBuffer")
    ("Alt NumPad Right"         "nextBuffer")))

;; mapKey(KeyEvent.VK_PAGE_UP, ALT_MASK, "pageUpOtherWindow");
;; mapKey(KeyEvent.VK_PAGE_UP, SHIFT_MASK, "selectPageUp");
;; mapKey(KeyEvent.VK_PAGE_DOWN, ALT_MASK, "pageDownOtherWindow");
;; mapKey(KeyEvent.VK_PAGE_DOWN, SHIFT_MASK, "selectPageDown");
;; mapKey(KeyEvent.VK_PAGE_UP, CTRL_MASK, "top");
;; mapKey(KeyEvent.VK_PAGE_DOWN, CTRL_MASK, "bottom");
;; mapKey(KeyEvent.VK_DELETE, 0, "delete");
(define-keys *emacs-global-map*
  '(("Delete"                   "delete")
    ("Ctrl D"                   "delete")
    ("Backspace"                "backspace")
    ("Shift Backspace"          "backspace")
    ("Ctrl Backspace"           "deleteWordLeft")
    ("Enter"                    "newline")
    ("Ctrl M"                   "newline")
    ("Ctrl J"                   "newlineAndIndent")))

(define-key *emacs-global-map* "Shift Alt 9" "insertParentheses")
(define-key *emacs-global-map* "Shift Alt 0" "movePastCloseAndReindent")

(define-key *emacs-global-map* "Ctrl G" "escape") ; keyboard-quit

(define-key *emacs-global-map* "Ctrl Shift G" "gotoFile")
(define-key *emacs-global-map* "Ctrl Shift B" "browsefileatdot")

(define-key *control-x-map* #\d "dir")

;; mapKey(KeyEvent.VK_F2, SHIFT_MASK, "stamp");

;; mapKey(KeyEvent.VK_A, CTRL_MASK, "selectAll");

;; mapKey(KeyEvent.VK_OPEN_BRACKET, ALT_MASK, "slideOut");
;; mapKey(KeyEvent.VK_CLOSE_BRACKET, ALT_MASK, "slideIn");

;; // Bookmarks MUST be mapped like this!
;; mapKey(KeyEvent.VK_0, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_1, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_2, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_3, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_4, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_5, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_6, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_7, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_8, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_9, ALT_MASK, "dropBookmark");
;; mapKey(KeyEvent.VK_0, CTRL_MASK, "gotoBookmark");
;; mapKey(KeyEvent.VK_1, CTRL_MASK, "gotoBookmark");
;; mapKey(KeyEvent.VK_2, CTRL_MASK, "gotoBookmark");
;; mapKey(KeyEvent.VK_3, CTRL_MASK, "gotoBookmark");
;; mapKey(KeyEvent.VK_4, CTRL_MASK, "gotoBookmark");
;; mapKey(KeyEvent.VK_5, CTRL_MASK, "gotoBookmark");
;; mapKey(KeyEvent.VK_6, CTRL_MASK, "gotoBookmark");
;; mapKey(KeyEvent.VK_7, CTRL_MASK, "gotoBookmark");
;; mapKey(KeyEvent.VK_8, CTRL_MASK, "gotoBookmark");
;; mapKey(KeyEvent.VK_9, CTRL_MASK, "gotoBookmark");

;; // Temporary marker commands.
;; mapKey(KeyEvent.VK_BACK_SLASH, ALT_MASK, "dropTemporaryMarker");
;; mapKey(KeyEvent.VK_BACK_SLASH, CTRL_MASK, "gotoTemporaryMarker");
;; mapKey(KeyEvent.VK_BACK_SLASH, CTRL_MASK | SHIFT_MASK, "selectToTemporaryMarker");

;; mapKey(KeyEvent.VK_F11, 0, "commentRegion");
(define-key *emacs-global-map* "F11" "commentRegion")
;; mapKey(KeyEvent.VK_F11, SHIFT_MASK, "uncommentRegion");
(define-key *emacs-global-map* "Shift F11" "uncommentRegion")

;; // Duplicate mappings to support IBM 1.3 for Linux.
;; mapKey(0xffc8, 0, "commentRegion");
;; mapKey(0xffc8, SHIFT_MASK, "uncommentRegion");

;; mapKey(KeyEvent.VK_F12, 0, "wrapParagraph");
(define-key *emacs-global-map* "F12" "wrapParagraph")
;; mapKey(KeyEvent.VK_F12, SHIFT_MASK, "unwrapParagraph");
(define-key *emacs-global-map* "Shift F12" "unwrapParagraph")
;; mapKey(KeyEvent.VK_F12, CTRL_MASK, "toggleWrap");
(define-key *emacs-global-map* "Ctrl F12" "toggleWrap")

;; // Duplicate mappings to support IBM 1.3 for Linux.
;; mapKey(0xffc9, 0, "wrapParagraph"); // F12
;; mapKey(0xffc9, SHIFT_MASK, "unwrapParagraph"); // Shift F12
;; mapKey(0xffc9, CTRL_MASK, "toggleWrap"); // Ctrl F12

;; mapKey(KeyEvent.VK_T, CTRL_MASK | ALT_MASK, "visibleTabs");

;; mapKey(KeyEvent.VK_SLASH, ALT_MASK, "expand");
(define-key *emacs-global-map* "Alt /" "expand")

;; // On Windows, Alt Space drops down the window menu.
;; if (!Platform.isPlatformWindows())
;; mapKey(KeyEvent.VK_SPACE, ALT_MASK, "expand");

;; mapKey(KeyEvent.VK_N, ALT_MASK, "nextFrame");

;; mapKey(KeyEvent.VK_W, ALT_MASK, "selectWord");

;; FIXME These are j's normal mouse bindings. We don't have the required
;; functionality in the right form to support the emacs mouse bindings.
(define-keys *emacs-global-map*
  '(("Mouse-1"                  "mouseMoveDotToPoint")
    ("Shift Mouse-1"            "mouseSelect")
    ("Ctrl Shift Mouse-1"       "mouseSelectColumn")
    ("Double Mouse-1"           "selectWord")
    ("Mouse-3"                  "mouseShowContextMenu")))

(when (featurep :unix)
  (define-key *emacs-global-map* "Mouse-2" "pastePrimarySelection"))

(define-keys *control-x-map*
  '((#\(                        "startMacro")
    (#\)                        "endMacro")
    (#\e                        "playbackMacro")))

;; mapKey(KeyEvent.VK_W, CTRL_MASK | SHIFT_MASK, "killFrame");

;; // Sidebar.
;; mapKey(KeyEvent.VK_EQUALS, ALT_MASK, "toggleSidebar");
;; mapKey(KeyEvent.VK_B, ALT_MASK, "sidebarListBuffers");
;; mapKey(KeyEvent.VK_T, CTRL_MASK | SHIFT_MASK, "sidebarListTags");

(define-keys *control-x-map*
  '(("2"                        "splitWindow")
    ("1"                        "unsplitwindow")
    ("0"                        "killWindow")
    ("o"                        "otherwindow")))

(when (get-global-property 'enable-experimental-features)
  (define-key *emacs-global-map* "Alt F9" "shell"))

(define-key *control-x-map* "`" "nextError")
(define-key *emacs-global-map* "F4" "nextError")
(define-key *emacs-global-map* "Shift F4" "previousError")
(define-key *emacs-global-map* "Ctrl Alt M" "showMessage")

;; FIXME We need a binding for findTag.
(define-key *emacs-global-map* "Alt ." "findTagAtDot")
(define-key *emacs-global-map* "Alt ," "listMatchingTagsAtDot")

;;; Help.

(define-keys *help-map*
  '(("a"                        "apropos")
    ("b"                        "describeBindings")
    ("c"                        "describeKey") ; describe-key-briefly
    ("i"                        "help")
    ("k"                        "describeKey")))

;;; Java mode.

(define-keys *java-mode-map*
  '(("{"                        "electricOpenBrace")
    ("}"                        "electricCloseBrace")
    ("Tab"                      "tab")
    ("Ctrl Tab"                 "insertTab")
    ("';'"                      "electricSemi")
    (#\:                        "electricColon")
    (#\*                        "electricStar")
    (#\)                        "closeParen")
    ("Ctrl Shift ["             "insertBraces")
    ("F12"                      "wrapComment")
    ("F9"                       "compile")
    ("Ctrl F9"                  "recompile")
    ("Alt F1"                   "jdkHelp")
    ("Ctrl F1"                  "source")))

(defun java-mode-map ()
  *java-mode-map*)

;;; Lisp mode

(defparameter *lisp-mode-map* (make-keymap))
(defparameter *lisp-mode-control-c-map* (make-keymap))
(defparameter *lisp-mode-control-x-map* (make-keymap))
(define-key *lisp-mode-map* "Ctrl C" *lisp-mode-control-c-map*)
(define-key *lisp-mode-map* "Ctrl X" *lisp-mode-control-x-map*)

(define-keys *lisp-mode-map*
  '(("Tab"                      "tab")
    ("Ctrl Tab"                 "insertTab")
    ("F12"                      "wrapComment")
    (#\)                        "closeParen")
    ("Alt F1"                   "hyperspec")
    ("Ctrl Alt F"               "forwardSexp")
    ("Ctrl Alt B"               "backwardSexp")
    ("Ctrl Alt Space"           "markSexp")
    ("Ctrl Alt D"               "downList")
    ("Ctrl Alt U"               "backwardUpList")
    ("Ctrl Alt X"               "evalDefunLisp")))

(define-keys *lisp-mode-control-c-map*
  '(("Ctrl C"                   "compileDefunLisp")
    ("Ctrl R"                   "evalRegionLisp")))

(defun lisp-mode-map ()
  *lisp-mode-map*)

;;; Lisp shell mode

(defparameter *lisp-shell-mode-map* (make-keymap))
(defparameter *lisp-shell-mode-esc-map* (make-keymap))
(defparameter *lisp-shell-mode-control-c-map* (make-keymap))
(define-key *lisp-shell-mode-map* "Escape" *lisp-shell-mode-esc-map*)
(define-key *lisp-shell-mode-map* "Ctrl C" *lisp-shell-mode-control-c-map*)

(define-keys *lisp-shell-mode-map*
  '(("Home"                     "shellHome")
    ("Ctrl A"                   "shellHome")
    ("Backspace"                "shellbackspace")
    ("Alt P"                    "shellPreviousInput")
    ("Alt N"                    "shellNextInput")
    ("Enter"                    "LispShellMode.enter")
    ("Alt Enter"                "newlineandindent")
    ("Ctrl R"                   "resetLisp")
    ("Tab"                      "indentLineOrRegion")
    ("Alt F1"                   "hyperspec")))

(define-keys *lisp-shell-mode-esc-map*
  '((#\p                        "shellPreviousInput")
    (#\n                        "shellNextInput")))

(define-keys *lisp-shell-mode-control-c-map*
  '(("Ctrl C"                   "shellInterrupt")
    ("Ctrl P"                   "shellPreviousPrompt")
    ("Ctrl N"                   "shellNextPrompt")))

(defun lisp-shell-mode-map ()
  *lisp-shell-mode-map*)

;;; Slime

(defun define-keys-for-slime ()
  (define-keys *lisp-mode-map*
    '(("Space"                  "(slime:slime-space)")
      ("Alt ."                  "(slime:slime-edit-definition)")
      ("Ctrl Alt I"             "(slime:slime-complete-symbol)")
      ("Ctrl Alt X"             "(slime:slime-eval-defun)")))
  (define-keys *lisp-mode-control-c-map*
    '(("Tab"                    "(slime:slime-complete-symbol)")
      ("Ctrl C"                 "(slime:slime-compile-defun)")
      ("Ctrl I"                 "(slime:slime-complete-symbol)")
      ("Ctrl K"                 "(slime:slime-compile-and-load-file)")
      ("Ctrl R"                 "(slime:slime-eval-region)")))
  (define-keys *lisp-mode-control-x-map*
    '(("Ctrl E"                 "(slime:slime-eval-last-expression)")))
  (define-keys *lisp-shell-mode-map*
    '(("Tab"                    "(slime:slime-complete-symbol)")
      ("Space"                  "(slime:slime-space)")
      ("Alt ."                  "(slime:slime-edit-definition)")
      ("Ctrl Alt I"             "(slime:slime-complete-symbol)")))
  (define-keys *lisp-shell-mode-esc-map*
    '(("Tab"                    "(slime:slime-complete-symbol)")))
  (define-keys *lisp-shell-mode-control-c-map*
    '(("Tab"                    "(slime:slime-complete-symbol)"))))

;;; Directory mode

(defparameter *directory-mode-map* (make-keymap))

;; These are basically j's normal directory mode bindings. J's directory mode
;; doesn't really work like dired in emacs.
(define-keys *directory-mode-map*
  '(("Enter"                    "dirOpenFile")
    ("Ctrl Shift G"             "diropenfile")
    ("Double Mouse-1"           "dirOpenFile")
    ("Mouse-2"                  "dirOpenFile")
    ("Ctrl Enter"               "diropenfileandkilldirectory")
    ("Ctrl Shift B"             "dirBrowseFile")
    ("Backspace"                "dirUpDir")
    (#\u                        "dirUpDir")
    (#\l                        "dirLimit")
    (#\L                        "dirUnlimit")
    (#\s                        "dirCycleSortBy")
    (#\r                        "dirRescan")
    ("Delete"                   "dirDeleteFiles")
    (#\c                        "dirCopyFile")
    (#\g                        "dirGetFile")
    (#\m                        "dirMoveFile")
    (#\t                        "dirTagFile")
    (#\!                        "dirDoShellCommand")
    ("Home"                     "dirHome")
    (#\b                        "dirBack")
    (#\f                        "dirForward")))

(defun directory-mode-map ()
  *directory-mode-map*)

(defun emacs-mode (&optional (arg t))
  (cond (arg
         ;; FIXME This is the right idea (so mappings like Alt F will be
         ;; possible), but it doesn't work.
         (set-global-property "useMenuMnemonics" "false")
         (set-global-property "emulation" "emacs")
         (use-global-map *emacs-global-map*)
         (j::%execute-command "reloadKeyMaps"))
        ((null arg)
         (set-global-property "useMenuMnemonics" "true")
         (set-global-property "emulation" nil)
         (j::%execute-command "defaultKeyMaps"))))
