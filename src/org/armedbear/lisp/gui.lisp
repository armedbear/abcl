(in-package :extensions)

(require :java)

(export '(*gui-backend* init-gui make-dialog-prompt-stream))


(defvar *gui-backend* :swing)

(defun init-gui ()
  "Dummy function used to autoload this file"
  t)

(defun make-dialog-prompt-stream ()
  (%make-dialog-prompt-stream *gui-backend*))

(defgeneric %make-dialog-prompt-stream (gui-backend))

(defmethod %make-dialog-prompt-stream ((gui-backend (eql :swing)))
  (java:jnew (java:jconstructor
              "org.armedbear.lisp.java.swing.SwingDialogPromptStream")))

(defmethod %make-dialog-prompt-stream ((gui-backend (eql :awt)))
  (java:jnew (java:jconstructor
              "org.armedbear.lisp.java.awt.AwtDialogPromptStream")))
