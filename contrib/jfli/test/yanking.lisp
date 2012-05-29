(defpackage :my (:use :cl))
(in-package :my)

;; runtime-class.lisp is a part of ABCL, but it is excluded from ABCL build,
;; because it requires asm.jar to be present in classpath during the build.
;;
;; The functionality it provides is necessary for dynamic creation of
;; new java classes from Lisp (in particular for the
;; NEW-CLASS macro of jfli ABCL port)
(load (concatenate 'string abclidea:*lisp-dir* "org/armedbear/lisp/runtime-class.lisp"))

;; Load jfli
(load (concatenate 'string abclidea:*lisp-dir* "jfli-abcl/jfli-abcl.lisp"))

(use-package :jfli)

;; "Import" java classes we use.
;;
;; You may produce DEF-JAVA-CLASS forms for all the IDEA API classes automatically:
;;
;; (jfli:dump-wrapper-defs-to-file (concatenate 'string abclidea:*lisp-dir* "idea-api.lisp")
;;                                 (jfli:get-jar-classnames "path/to/idea/openapi.jar"
;;                                                          "com/intellij"))
;;
;;
;; In result they will be stored in idea-api.lisp file.
;;
;; But we do it manually, because there are not so many classes we use.

(def-java-class "com.intellij.openapi.ui.Messages")
(use-package "com.intellij.openapi.ui")

(def-java-class "com.intellij.openapi.application.ModalityState")
(def-java-class "com.intellij.openapi.application.Application")
(def-java-class "com.intellij.openapi.application.ApplicationManager")
(use-package "com.intellij.openapi.application")

(def-java-class "com.intellij.openapi.actionSystem.AnAction")
(def-java-class "com.intellij.openapi.actionSystem.AnActionEvent")
(def-java-class "com.intellij.openapi.actionSystem.ActionManager")
(def-java-class "com.intellij.openapi.actionSystem.DefaultActionGroup")
(def-java-class "com.intellij.openapi.actionSystem.CustomShortcutSet")
(def-java-class "com.intellij.openapi.actionSystem.Shortcut")
(def-java-class "com.intellij.openapi.actionSystem.KeyboardShortcut")
(def-java-class "com.intellij.openapi.actionSystem.CustomShortcutSet")
(use-package "com.intellij.openapi.actionSystem")

(def-java-class "com.intellij.openapi.ide.CopyPasteManager")
(use-package "com.intellij.openapi.ide")

(def-java-class "com.intellij.openapi.keymap.KeymapManager")
(def-java-class "com.intellij.openapi.keymap.Keymap")
(use-package "com.intellij.openapi.keymap")

(def-java-class "com.intellij.openapi.project.ProjectManager")
(use-package "com.intellij.openapi.project")

(def-java-class "com.intellij.openapi.editor.Editor")
(def-java-class "com.intellij.openapi.editor.Document")
(def-java-class "com.intellij.openapi.editor.SelectionModel")
(use-package "com.intellij.openapi.editor")

(def-java-class "com.intellij.openapi.fileEditor.FileEditorManager")
(def-java-class "com.intellij.openapi.fileEditor.FileEditor")
(def-java-class "com.intellij.openapi.fileEditor.TextEditor")
(use-package "com.intellij.openapi.fileEditor")

(def-java-class "com.intellij.openapi.command.CommandProcessor")
(def-java-class "com.intellij.openapi.command.CommandAdapter")
(def-java-class "com.intellij.openapi.command.CommandEvent")
(use-package "com.intellij.openapi.command")

(def-java-class "com.intellij.openapi.wm.WindowManager")
(def-java-class "com.intellij.openapi.wm.StatusBar")
(use-package "com.intellij.openapi.wm")

(def-java-class "java.lang.Runnable")
(def-java-class "java.lang.Thread")
(def-java-class "java.lang.Object")
(def-java-class "java.lang.Class")
(def-java-class "java.lang.String")
(use-package "java.lang")

(def-java-class "java.awt.datatransfer.Transferable")
(def-java-class "java.awt.datatransfer.DataFlavor")
(use-package "java.awt.datatransfer")

(def-java-class "javax.swing.KeyStroke")
(use-package "javax.swing")

(define-condition action-is-not-applicable ()
  ((why :initarg :why :reader why))
  (:report (lambda (condition stream)
             (format stream "Action is not applicable: ~A" (why condition)))))

(defun cur-prj ()
  (let ((all-prjs (projectmanager.getopenprojects (projectmanager.getinstance))))
    (when (> (jlength all-prjs) 0)
      (jref all-prjs 0))))

(defun cur-prj-safe ()
  (or (cur-prj) (error 'action-is-not-applicable :why "no current project")))

(defun cur-editor (prj)
  (fileeditormanager.getselectedtexteditor (fileeditormanager.getinstance prj)))

(defun cur-editor-safe (prj)
  (or (cur-editor prj) 
      (error 'action-is-not-applicable 
             :why "no text editor is selected")))

;; region object
(defun make-region (start end)
  (cons start end))

(defun region-start (region)
  (car region))

(defun region-end (region)
  (cdr region))

(defun get-sel-region()
  "Selection in the currently active editor"
  (let* ((cur-prj (cur-prj-safe))
         (cur-editor (cur-editor-safe cur-prj))
         (sel-model (editor.getselectionmodel cur-editor)))
    (make-region 
       (selectionmodel.getselectionstart sel-model)
       (selectionmodel.getselectionend sel-model))))

(defun replace-region (replacement-text region)
  "Replace text in the curently active editor"
  (let* ((cur-prj (cur-prj-safe))
         (cur-editor (cur-editor-safe cur-prj))
         (cur-doc (editor.getdocument cur-editor)))
    (document.replacestring cur-doc 
                            (region-start region)
                            (region-end region)
                            replacement-text)))

(defvar *yank-index* 0
    "Index of clipboard item that will be pasted by the next yank or
 yank-pop operation \(similar to kill-ring-yank-pointer in Emacs\).")

(defvar *yank-region* nil
    "Region of text that was inserted by previous yank or yank-pop command,
and that must be replaced by next yank-pop.")

(defvar *yank-undo-id* 0
    "Yank following by a sequence of yank-pop must be considered as a
single action by undo mechanism. This variable is unique identifier
of such an compound action.")

(defun get-yank-text (&optional (index 0))
  (let ((all-contents (copypastemanager.getallcontents (copypastemanager.getinstance)))
        content)
    (when (zerop (jlength all-contents))
      (RETURN-FROM get-yank-tex nil))
    (setf content (jref all-contents (mod index (jlength all-contents))))
    (transferable.gettransferdata content (dataflavor.stringflavor))))

(defun get-yank-text-safe (&optional (index 0))
  (or (get-yank-text index) 
      (error 'action-is-not-applicable :why "clipboard is empty")))

(defun next-yank-region (cur-selection-region replacement-text)
  (make-region (region-start cur-selection-region)
               (+ (region-start cur-selection-region)
                  (length (java:jobject-lisp-value replacement-text)))))
(defun yank()
  (let ((sel-region (get-sel-region))
        (yank-text (get-yank-text-safe)))
    (replace-region yank-text
                    sel-region)
    (setf *yank-region* (next-yank-region sel-region 
                                          yank-text))
    (setf *yank-index* 1)))

(defun make-runnable (fun)
  (java:jinterface-implementation 
   "java.lang.Runnable"
   "run"
   ;; wrap FUN into lambda to allow it to be
   ;; not only function objects, but also symbols
   ;; (java:jinterface-implementation supports
   ;; only function objects)
   (lambda () (funcall fun))))

(defmacro runnable (&body body)
  `(make-runnable (lambda () ,@body)))

(defun run-write-action (fun)
  (let ((app (applicationmanager.getapplication))
        (runnable (make-runnable fun)))
    (application.runwriteaction app runnable)))

(defun exec-cmd (fun name group-id)
  (commandprocessor.executecommand (commandprocessor.getinstance)
                                   (cur-prj)
                                   (make-runnable fun)
                                   name
                                   group-id))

;; set status bar text
(defun set-status (status-text)
  (statusbar.setinfo (windowmanager.getstatusbar 
                      (windowmanager.getinstance) 
                      (cur-prj))
                     status-text))

(new-class 
 "MY.MyAction" ;; class name
 anaction. ;; super class

 ;; constructors
 (
  (((text "java.lang.String") (func "java.lang.Object"))
   (super text)
   (setf (myaction.func this) func))
  )
 
 ;; methods
 ( 
  ("actionPerformed" :void :public (action-event) 
                     ;; It's usefull to setup a restart before
                     ;; calling FUNC.
                     ;;
                     ;; It helps when slime is connected to
                     ;; the IDEA and error happens
                     ;; during action execution.
                     ;;
                     ;; Slime debugger hooks the error,
                     ;; but as actions are invoked from
                     ;; idea UI event dispatching thread,
                     ;; no slime restarts are set
                     ;; and our restart is the only
                     ;; way to leave SLIME debugger.
                     (restart-case
                         (handler-case
                             (funcall (myaction.func this) action-event)
                           (action-is-not-applicable ()
                             ;; NOTE: it is not guaranteed
                             ;; that execution will be passed to this
                             ;; handler, even if your code signals
                             ;; ACTION-IS-NOT-APPLICABLE.
                             ;;
                             ;; It's so because ABCL impements
                             ;; non local exits using java exceptions
                             ;; (org.armedbear.lisp.Go); if somewhere
                             ;; in the call stack below our HANDLER-CASE
                             ;; and above the SIGNAL there is a
                             ;;
                             ;;    catch (Throwable)
                             ;;
                             ;; then ABCL's Go exception will be catched. 
                             ;;
                             ;; catch (Throwable) is in partiular
                             ;; used by IDEA methods that accept Runnable
                             ;; (like CommandProcessor.executeCommand,
                             ;; Application.runWriteAction)
                             ;;
                             ;; But even despite that, HANDLER-CASE
                             ;; is useful, because ACTION-IS-NOT-APPLICABLE
                             ;; is not trapped by Slime debugger.
                             ))
                       (continue () 
                         :report "Return from IDEA action"
                         nil)))
  )

  ;; fields
 (
  ("func" "java.lang.Object" :public))
 )

(setf act-yank (myaction.new "yank" nil))
(setf (myaction.func act-yank)
      #'(lambda (action-event) 
          (declare (ignore action-event))
          (incf *yank-undo-id*)
          (exec-cmd (lambda () 
                      (run-write-action 'yank)) 
                    "yank" 
                    (format nil "yank-~A" *yank-undo-id*))))

(setf edit-menu (actionmanager.getaction (actionmanager.getinstance) "EditMenu"))

(actionmanager.registeraction (actionmanager.getinstance) "yank" act-yank)
(defaultactiongroup.add edit-menu act-yank)

;;(actionmanager.unregisteraction (actionmanager.getinstance) "yank")
;;(defaultactiongroup.remove edit-menu act-yank)

;; assign keyboard shortcut Ctrl-Y to our action
;; (by default Ctrl-Y is used for delete-line operation in IDEA;
;; override this by unregistering Ctrl-Y from delete-line)
(defun action-shortcut (anaction)
  "The first element of AnAction.getShorcuts()"
  (jref (customshortcutset.getshortcuts (anaction.getshortcutset anaction)) 0))

(defun remove-shortcut (keystroke-str)
  "Unregister all the shortcuts specified by KEYSTROKE-STR
for all the actions in the active keymap. 
Example \(REMOVE-SHORTCUT \"control Y\"\)"
  (let* ((keymap (keymapmanager.getactivekeymap (keymapmanager.getinstance)))
         (keystroke (keystroke.getkeystroke keystroke-str))
         (act-ids (keymap.getactionids keymap keystroke)))
    (dotimes (i (jlength act-ids))
      (let ((shortcuts (keymap.getshortcuts keymap (jref act-ids i))))
        (dotimes (j (jlength shortcuts))
          (let ((shortcut (jref shortcuts j)))
            (when (class.isinstance (class.forname "com.intellij.openapi.actionSystem.KeyboardShortcut")
                                    shortcut)
              (when (jeq (keyboardshortcut.getfirstkeystroke shortcut)
                         keystroke)
                (keymap.removeshortcut keymap (jref act-ids i) shortcut)))))))))

;; this is to display shortcut correctly in the menu
(anaction.setshortcutset act-yank 
                         (customshortcutset.new (keystroke.getkeystroke "control Y")))

;; this is to make it actually fired when user presses the key combination
(remove-shortcut "control Y")
(keymap.addshortcut (keymapmanager.getactivekeymap (keymapmanager.getinstance))
                    "yank"
                    (action-shortcut act-yank))

;; yank-pop is allowed only if previous command was yank or yank-pop.
;; Add a command listentener that clears *yank-region* when any
;; other command is executed, and thus makes yank-pop impossible.
(new-class 
 "MY.MyCommandListener" ;; class name
 commandadapter. ;; super class

 ;; constructors
 ()
 
 ;; methods
 ( 
  ("commandFinished" :void :public (command-event) 
                     (unless (member (java:jobject-lisp-value (commandevent.getcommandname 
                                                                command-event))
                                     '("yank" "yank-pop")
                                     :test #'string=)
                       (setf *yank-region* nil)))
  )

  ;; fields
 ()
 )

(setf my-cmd-listener (mycommandlistener.new))
(commandprocessor.addcommandlistener (commandprocessor.getinstance)
                                     my-cmd-listener)

;; (actionmanager.unregisteraction (actionmanager.getinstance) "yank-pop")
;; (defaultactiongroup.remove edit-menu act-yank-pop)

(defun yank-pop ()
  (let ((yank-text (get-yank-text *yank-index*))) 
    (replace-region yank-text *yank-region*)
    (setf *yank-region* (make-region (region-start *yank-region*)
                                     (+ (region-start *yank-region*)
                                        (string.length yank-text)))))
  (incf *yank-index*))

(setf act-yank-pop (myaction.new "yank-pop" nil))
(setf (myaction.func act-yank-pop)
      #'(lambda (action-event)
          (if *yank-region* 
              (exec-cmd (lambda () 
                          (run-write-action 'yank-pop)) 
                        "yank-pop" 
                        (format nil "yank-~A" *yank-undo-id*))
              (set-status "Previous command was not a yank"))))

(actionmanager.registeraction (actionmanager.getinstance) "yank-pop" act-yank-pop)
(defaultactiongroup.add edit-menu act-yank-pop)

(anaction.setshortcutset act-yank-pop 
                         (customshortcutset.new (keystroke.getkeystroke "alt Y")))

(keymap.addshortcut (keymapmanager.getactivekeymap (keymapmanager.getinstance))
                    "yank-pop"
                    (action-shortcut act-yank-pop))

